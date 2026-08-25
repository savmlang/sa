use std::{
  cell::UnsafeCell,
  hint::cold_path,
  mem::zeroed,
  ptr::{addr_of_mut, null, null_mut},
  sync::{
    Arc, LazyLock, OnceLock,
    atomic::{Ordering, compiler_fence},
  },
};

#[cfg(feature = "native")]
use sajit::Executable;
use sart::{ctr::VMTaskState, salloc, structures::QuadPackedData};

use crate::{
  BytecodeResolver, CODE_CACHE, PickleJumpData, SymbolMapTable, VM,
  acaot::pickle::{
    PickleWorker,
    def::{
      DISPATCH_TOTAL_ITEMS, PICKLE_OPCODE_HINT, PICKLE_OPCODE_MARK, PickleInstruction,
      pickle_generate_table,
    },
    implementation::{ResolveFn, SIZE_128KB, WorkingSet},
  },
  kvwrap::SaVMJumpWrap,
  sync::preps::cleanup_vmstat,
};

pub static GLOBAL_DATA: OnceLock<UnSafePtr<u8>> = OnceLock::new();
static UNUSED_RELOCMAP: LazyLock<Arc<[PickleJumpData]>> = LazyLock::new(Default::default);

const SCRATCHPAD: usize = 50 * 24 * size_of::<QuadPackedData>();

pub struct UnSafePtr<T>(pub *mut T);
unsafe impl<T> Send for UnSafePtr<T> {}
unsafe impl<T> Sync for UnSafePtr<T> {}

pub struct VMState {
  pub ws: WorkingSet,
  pub ts: [VMTaskState; 50],
  pub cindex: usize,
}

impl VMState {
  pub fn init() -> Self {
    VMState {
      ws: WorkingSet {
        arr: &[],
        dispatch: null(),
        largepad: unsafe { salloc::aligned_malloc(SIZE_128KB, 8) as _ },
        largepad_cursor: 0,
        ame: null_mut(),
        ame_free: true,
        jmp: (0, 0),
        relocmap: SaVMJumpWrap(UNUSED_RELOCMAP.clone()),
      },
      ts: unsafe {
        let mut ts: [VMTaskState; 50] = zeroed();

        let alloca = salloc::aligned_malloc(SCRATCHPAD, 64) as *mut QuadPackedData;
        for (i, t) in ts.iter_mut().enumerate() {
          t.scratchpad = alloca.add(i * 24);
        }

        ts
      },
      cindex: 0,
    }
  }
}

impl Drop for VMState {
  fn drop(&mut self) {
    unsafe {
      salloc::aligned_free(self.ws.largepad as _);
      salloc::aligned_free(self.ts[0].scratchpad as _);

      if !self.ws.ame.is_null() {
        salloc::aligned_free(self.ws.ame as _);
      }
    }
  }
}

thread_local! {
  pub static VMSTAT: UnsafeCell<VMState> = UnsafeCell::new(VMState::init());
}

/// Functions for preparation & standard prologue-epilogues
pub(crate) mod preps {
  use crate::{
    PickleJumpData,
    acaot::pickle::{def::PickleInstruction, implementation::DispatchFn},
    kvwrap::{SaVMJumpWrap, SaVMJumpWrapImpl},
    sync::{VMSTAT, VMState},
  };
  use sart::{
    ctr::{
      FLAGS::{FLAG_FIRST, FLAG_JUMP_TO_RESUME},
      VMTaskState,
    },
    structures::QuadPackedData,
  };
  use std::{os::raw::c_void, ptr, sync::Arc};

  #[inline(always)]
  pub fn fncall_prep(vmstat: *mut VMState, oldtsk: *mut VMTaskState) {
    unsafe {
      (*vmstat).cindex += 1;
      ptr::write((*vmstat).ts.as_mut_ptr().add((*vmstat).cindex), *oldtsk);
    }
  }

  #[inline(always)]
  pub fn fncall_out(vmstat: *mut VMState) -> [QuadPackedData; 2] {
    unsafe {
      let resp = (*vmstat).ts.get_unchecked((*vmstat).cindex);

      (*vmstat).cindex -= 1;

      [resp.r7, resp.r8]
    }
  }

  #[inline(always)]
  #[allow(dead_code)]
  pub fn prep_jit(ts: *mut VMTaskState, marker: Option<u64>) {
    unsafe {
      // Add the JUMP entry
      if let Some(marker) = marker {
        (*ts).flags |= FLAG_JUMP_TO_RESUME;
        (*ts).curline_or_resume.unsigned = marker;
      }
    }
  }

  #[inline(always)]
  pub fn prepare_interpreter_loop<F: FnOnce(&mut DispatchFn)>(
    t: *mut VMState,
    jumps: Arc<[PickleJumpData]>,
    engine: *mut c_void,
    dispatch: F,
  ) -> *mut VMTaskState {
    unsafe {
      let wrapped = SaVMJumpWrap(jumps);

      dispatch(&mut (*t).ws.dispatch);
      (*t).ws.jmp = (0, wrapped.get(&0).unwrap_or_default());
      (*t).ws.relocmap = wrapped;

      let ts = (*t).ts.as_mut_ptr().add((*t).cindex as usize);

      (*ts).engine.pt = engine;
      (*ts).curline_or_resume.usi = 0;

      ts
    }
  }

  #[inline(always)]
  pub fn interpreter_process_hintmark(
    dt: &[PickleInstruction],
    ts: *mut VMTaskState,
    marker: &mut Option<u64>,
  ) {
    unsafe {
      let data: [PickleInstruction; 2] = {
        dt[((*ts).curline_or_resume.usi + 1)..(*ts).curline_or_resume.usi + 3]
          .try_into()
          .unwrap()
      };

      let out = u64::from_le_bytes([
        data[0].opcode,
        data[0].u1,
        data[0].u2,
        data[0].u3,
        data[1].opcode,
        data[1].u1,
        data[1].u2,
        data[1].u3,
      ]);

      *marker = Some(out);

      // Also increment by the correct amount
      (*ts).curline_or_resume.usi += 3;
    }
  }

  #[inline(always)]
  pub fn cleanup_vmstat(engine: *mut c_void) {
    VMSTAT.with(|x| {
      let t = x.get();
      let ts = unsafe { (*t).ts.get_unchecked_mut((*t).cindex) };

      ts.flags = ts.flags & FLAG_FIRST; // only FLAG_FIRST is allowed to stay
      ts.opcode = 0;
      ts.engine.pt = engine;
    });
  }
}

impl<E: BytecodeResolver + Send + Sync + 'static> VM<E> {
  pub const PICKLE_DISPATCH_TABLE: [ResolveFn; DISPATCH_TOTAL_ITEMS] = pickle_generate_table::<E>();

  pub fn fncall(&self, sectionid: u64, oldtsk: *mut VMTaskState) -> [QuadPackedData; 2] {
    let vmstat = VMSTAT.with(|x| x.get());
    preps::fncall_prep(vmstat, oldtsk);

    self.dispatch_chocolate::<true>(sectionid);

    let vmstat = VMSTAT.with(|x| x.get());
    preps::fncall_out(vmstat)
  }

  pub fn call_section(&self, sectionid: u64) {
    return self.dispatch_chocolate::<true>(sectionid);
  }

  #[inline(always)]
  pub fn dispatch_chocolate<const JMPTOJIT: bool>(&self, sectionid: u64) {
    let Some((data, jumps)) = CODE_CACHE.get(&sectionid) else {
      self.pickle_section(sectionid);

      return self.dispatch_chocolate::<JMPTOJIT>(sectionid);
    };

    let leng = data.len();

    #[allow(unused)]
    let mut marker = None;
    #[allow(unused)]
    let mut run_jit = false;

    let t = VMSTAT.with(UnsafeCell::get);
    let ts = preps::prepare_interpreter_loop(t, jumps, self as *const _ as *mut _, |x| {
      *x = Self::PICKLE_DISPATCH_TABLE.as_ptr();
    });

    unsafe {
      cleanup_vmstat(self as *const _ as *mut _);

      'jcheck: loop {
        let dt = data.as_ref();

        // Optimize the HINT interpreter opcode
        let dptr = dt.as_ptr();
        (*ts).ws.pt = dptr as _;

        #[cfg(feature = "native")]
        if JMPTOJIT {
          if let Some(_) = &crate::JIT_CACHE.get().unwrap_unchecked().get(sectionid) {
            // We have to marker to jump to
            preps::prep_jit(ts, marker);

            // Jump to JIT
            run_jit = true;
            break 'jcheck;
          };
        }

        // We use loop-in-a-loop to correctly manage state!
        // eg, yield that makes it re-check for JIT
        loop {
          if (*ts).curline_or_resume.usi == leng {
            break 'jcheck;
          }

          let pickle = dt.get_unchecked((*ts).curline_or_resume.usi);

          // USE A NO-OP to our benefit
          if pickle.opcode == PICKLE_OPCODE_HINT && PICKLE_OPCODE_MARK == pickle.u1 {
            preps::interpreter_process_hintmark(dt, ts, &mut marker);

            continue 'jcheck;
          }

          // Ensure the state's reflected
          compiler_fence(Ordering::SeqCst);
          (Self::PICKLE_DISPATCH_TABLE.get_unchecked(pickle.opcode as usize))(
            pickle,
            addr_of_mut!((*t).ws),
            ts,
          );
          compiler_fence(Ordering::SeqCst);

          (*ts).curline_or_resume.usi += 1;
        }
      }
    }

    #[cfg(feature = "native")]
    if run_jit {
      return self.dispatch_jit(sectionid);
    }

    cold_path();
    return self.ame_free(sectionid);
  }

  #[inline(always)]
  #[cfg(feature = "native")]
  pub fn dispatch_jit(&self, sectionid: u64) {
    use crate::JIT_CACHE;
    use std::ops::Deref;

    let jitcache = JIT_CACHE
      .get()
      .expect("JITCache should NOT be uninitialized");

    // Loop is set to set up - JIT Check points.
    loop {
      use sart::ctr::OPCODES::OPCODE_JIT_CHECK;

      let Some(jit) = jitcache.get(sectionid) else {
        return self.dispatch_chocolate::<true>(sectionid);
      };

      let (_, exec) = jit.get();

      let opcode;
      let dref = exec.deref();

      if dref.cinder {
        #[cfg(all(
          feature = "native",
          any(target_arch = "x86_64"),
          any(target_os = "windows", target_os = "linux")
        ))]
        {
          cold_path();

          let pickle;
          loop {
            if let Some((data, _)) = CODE_CACHE.get(&sectionid) {
              pickle = data;
              break;
            }

            self.pickle_section(sectionid)
          }
          opcode = self.exec_jit_cinder(&pickle, dref.exec);
        }

        #[cfg(not(all(
          feature = "native",
          any(target_arch = "x86_64"),
          any(target_os = "windows", target_os = "linux")
        )))]
        unreachable!();
      } else {
        opcode = self.exec_jit(dref.exec);
      }
      drop(exec);

      if opcode == OPCODE_JIT_CHECK {
        cold_path();
        continue;
      }

      break;
    }

    return self.ame_free(sectionid);
  }

  #[inline(never)]
  #[cfg(all(
    feature = "native",
    any(target_arch = "x86_64"),
    any(target_os = "windows", target_os = "linux")
  ))]
  pub fn exec_jit_cinder(&self, pickle: &[PickleInstruction], exec: *const Executable) -> u32 {
    use crate::acaot::cinder::DispatchStarter;

    let vmstate = VMSTAT.with(UnsafeCell::get);

    unsafe {
      use crate::acaot::cinder::setws;
      use std::mem::transmute;

      let curr_taskstate = (*vmstate).ts.as_mut_ptr().add((*vmstate).cindex);

      let mut cinder: DispatchStarter = DispatchStarter {
        hotness_or_resume: 0,
        ws: &mut (*vmstate).ws as *mut _ as _,
        pickle: pickle.as_ptr() as _,
        taskstate: curr_taskstate,
        wsarr: setws,
      };

      // Setup Pointers
      {
        let task = &mut *curr_taskstate;

        task.engine.pt = self as *const _ as *mut Self as _;
        task.ws.pt = &mut cinder as *mut _ as _;
      }

      let exec: extern "C" fn(vmtskstate: *mut VMTaskState) = transmute(exec);
      exec(curr_taskstate);

      (*curr_taskstate).opcode
    }
  }

  #[inline(always)]
  #[cfg(feature = "native")]
  pub fn exec_jit(&self, exec: *const Executable) -> u32 {
    let vmstate = VMSTAT.with(UnsafeCell::get);
    unsafe {
      use std::mem::transmute;

      let curr_taskstate = (*vmstate).ts.as_mut_ptr().add((*vmstate).cindex);

      // Setup Pointers
      {
        let task = &mut *curr_taskstate;

        task.engine.pt = self as *const _ as *mut Self as _;
        task.ws.pt = &mut (*vmstate).ws as *mut _ as _;
      }

      let exec: extern "C" fn(vmtskstate: *mut VMTaskState) = transmute(exec);
      exec(curr_taskstate);

      (*curr_taskstate).opcode
    }
  }

  pub(crate) fn pickle_section(&self, sectionid: u64) {
    // Compile
    let SymbolMapTable::MixedSizedBytecode { bytecode } = self.resolve.resolve_data(sectionid)
    else {
      return;
    };

    let mut worker = PickleWorker {
      bytecode,
      libcalls: Default::default(),
      out: vec![],
      jump: Default::default(),
    };
    worker.pass1();

    let out: Arc<[PickleInstruction]> = Arc::from(worker.out.into_boxed_slice());

    CODE_CACHE.insert(sectionid, (out, Arc::from(worker.jump)));
    CODE_CACHE.run_pending_tasks();
  }

  pub(crate) fn ame_free(&self, _sectionid: u64) {
    let vm = VMSTAT.with(UnsafeCell::get);

    unsafe {
      for tsk in &mut (*vm).ts {
        if !tsk.ame.is_null() {
          (*vm).ws.freeame(tsk.ame);
          tsk.ame = null_mut();
        }
      }
    }
  }
}
