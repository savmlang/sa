use std::{
  cell::UnsafeCell,
  hint::cold_path,
  mem::zeroed,
  ptr::{self, null_mut},
  sync::{
    Arc, OnceLock,
    atomic::{Ordering, compiler_fence},
  },
};

#[cfg(feature = "native")]
use sajit::Executable;
use sart::{ctr::VMTaskState, salloc, structures::QuadPackedData};

use crate::{
  CODE_CACHE, SymbolMapTable, VM,
  acaot::pickle::{
    PickleWorker,
    def::{PICKLE_DISPATCH_TABLE, PICKLE_OPCODE_HINT, PICKLE_OPCODE_MARK, PickleInstruction},
    implementation::{SIZE_128KB, WorkingSet},
  },
};

pub static GLOBAL_DATA: OnceLock<UnSafePtr<u8>> = OnceLock::new();

pub struct UnSafePtr<T>(pub *mut T);

unsafe impl<T> Send for UnSafePtr<T> {}
unsafe impl<T> Sync for UnSafePtr<T> {}

const SCRATCHPAD: usize = 50 * 24 * size_of::<QuadPackedData>();

pub struct VMState {
  pub ws: WorkingSet,
  pub ts: [VMTaskState; 50],
  pub cindex: usize,
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
  pub static VMSTAT: UnsafeCell<VMState> = UnsafeCell::new(VMState {
    ws: WorkingSet {
      arr: &[],
      largepad: unsafe { salloc::aligned_malloc(SIZE_128KB, 8) as _ },
      largepad_cursor: 0,
      ame: null_mut(),
      ame_free: true,
      jmp: (0, 0),
      relocmap: Default::default()
    },
    ts: unsafe {
      let mut ts: [VMTaskState; 50] = zeroed();

      let alloca = salloc::aligned_malloc(SCRATCHPAD, 64) as *mut QuadPackedData;
      for (i, t) in ts.iter_mut().enumerate() {
        t.scratchpad = alloca.add(i * 24);
      }

      ts
    },
    cindex: 0
  });
}

impl VM {
  pub fn call_section(&self, sectionid: u64) {
    return self.dispatch_chocolate::<true>(sectionid);
  }

  #[inline(always)]
  pub fn dispatch_chocolate<const JMPTOJIT: bool>(&self, sectionid: u64) {
    let Some((mut data, jumps)) = CODE_CACHE.get(&sectionid) else {
      return self.pickle_section(sectionid, Self::dispatch_chocolate::<JMPTOJIT>);
    };

    let leng = data.len();

    #[allow(unused)]
    let mut jumptomark = None;
    #[allow(unused)]
    let mut run_jit = false;

    VMSTAT.with(|x| unsafe {
      let t = x.get();

      (*t).ws.jmp = (0, jumps.get(&0).map(|x| *x).unwrap_or_default());
      (*t).ws.relocmap = jumps;

      let ts = (*t).ts.as_mut_ptr().add((*t).cindex as usize);

      (*ts).engine_or_pt.pt = self as *const _ as _;
      (*ts).curline_or_resume.usi = 0;

      let mut atmark = false;

      'jcheck: loop {
        let dt = data.as_ref();

        #[cfg(feature = "native")]
        if JMPTOJIT {
          if let Some(_) = &crate::JIT_CACHE.get().unwrap_unchecked().0.get(&sectionid) {
            if let Some(marker) = jumptomark {
              use sart::ctr::FLAGS::FLAG_JUMP_TO_RESUME;

              (*ts).flags = FLAG_JUMP_TO_RESUME;
              (*ts).curline_or_resume.unsigned = marker;
            }

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

            jumptomark = Some(out);

            (*ts).curline_or_resume.usi += 3;
            atmark = true;
            continue 'jcheck;
          }

          if pickle.opcode == PICKLE_OPCODE_HINT {
            let dptr = dt.as_ptr();
            (*ts).engine_or_pt.pt = dptr as _;
          }

          // Ensure the state's reflected
          compiler_fence(Ordering::SeqCst);
          (PICKLE_DISPATCH_TABLE.get_unchecked(pickle.opcode as usize))(
            pickle,
            &mut (*t).ws,
            &mut *ts,
          );
          compiler_fence(Ordering::SeqCst);

          (*ts).curline_or_resume.usi += 1;
        }
      }
    });

    #[cfg(feature = "native")]
    if run_jit {
      // TODO: Replace with `become`
      return self.dispatch_jit(sectionid);
    }

    cold_path();
    return self.ame_free(sectionid);
  }

  #[inline(always)]
  #[cfg(feature = "native")]
  pub fn dispatch_jit(&self, sectionid: u64) {
    use std::ops::Deref;

    use crate::JIT_CACHE;

    let Some(jitcache) = JIT_CACHE.get() else {
      return unreachable!();
    };

    let Some(jit) = jitcache.0.get_one(&sectionid) else {
      return self.dispatch_chocolate::<true>(sectionid);
    };

    let (_, exec) = unsafe { &**jit }.get();

    self.exec_jit(*exec.deref());

    drop(exec);

    return self.ame_free(sectionid);
  }

  #[inline(always)]
  #[cfg(feature = "native")]
  pub fn exec_jit(&self, exec: *const Executable) {
    VMSTAT.with(|x| unsafe {
      use std::mem::transmute;

      let vmstate = x.get();

      let curr_taskstate = (*vmstate).ts.as_mut_ptr().add((*vmstate).cindex);

      // Setup Pointers
      // todo!() figure out jumping
      {
        let task = &mut *curr_taskstate;

        task.engine_or_pt.pt = self as *const _ as *mut VM as _;
        task.ws_or_pt2.pt = &mut (*vmstate).ws as *mut _ as _;
      }

      // Execute
      let exec: extern "C" fn(vmtskstate: *mut VMTaskState) = transmute(exec);
      exec(curr_taskstate);
    });
  }

  fn pickle_section(&self, sectionid: u64, dispatch: fn(vm: &VM, sectionid: u64) -> ()) {
    // Compile
    let SymbolMapTable::MixedSizedBytecode { bytecode } = self.resolve.resolve_data(sectionid)
    else {
      return;
    };

    let mut worker = PickleWorker {
      bytecode,
      out: vec![],
      jump: Default::default(),
    };
    worker.pass1();

    let out: Arc<[PickleInstruction]> = Arc::from(worker.out.into_boxed_slice());

    CODE_CACHE.insert(sectionid, (out, Arc::new(worker.jump)));
    CODE_CACHE.run_pending_tasks();

    // TODO: Replace with `become`
    return dispatch(self, sectionid);
  }

  fn ame_free(&self, _sectionid: u64) {
    VMSTAT.with(|vtsk| unsafe {
      let vm = &mut *vtsk.get();

      for tsk in &mut vm.ts {
        if !tsk.ame.is_null() {
          vm.ws.freeame(tsk.ame);
          tsk.ame = null_mut();
        }
      }
    })
  }
}
