use std::{
  cell::UnsafeCell,
  hint::cold_path,
  mem::zeroed,
  ptr,
  sync::{Arc, OnceLock},
};

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
    }
  }
}

thread_local! {
  pub static VMSTAT: UnsafeCell<VMState> = UnsafeCell::new(VMState {
    ws: WorkingSet {
      arr: &[],
      largepad: unsafe { salloc::aligned_malloc(SIZE_128KB, 8) as _ },
      largepad_cursor: 0,
      jmp: (0, 0),
      relocmap: Default::default()
    },
    ts: unsafe {
      let mut ts: [VMTaskState; 50] = zeroed();

      let alloca = salloc::aligned_malloc(SCRATCHPAD, 64) as *mut QuadPackedData;
      for (i, t) in ts.iter_mut().enumerate() {
        t.scratchpad = alloca.add(i * 24 * size_of::<QuadPackedData>());
      }

      ts
    },
    cindex: 0
  });
}

impl VM {
  pub fn call_section(&self, sectionid: u64) {
    let Some((mut data, jumps)) = CODE_CACHE.get(&sectionid) else {
      // TODO: Replace with `become`
      return self.pickle_section(sectionid);
    };

    let leng = data.len();

    let mut run_jit = false;

    VMSTAT.with(|x| unsafe {
      let t = &mut *x.get();

      t.ws.jmp = (0, jumps.get(&0).map(|x| *x).unwrap_or_default());
      t.ws.relocmap = jumps;

      let ts = t.ts.get_unchecked_mut(t.cindex);

      ts.engine_or_pt.pt = self as *const _ as _;
      ts.curline_or_resume.usi = 0;

      let mut atmark = false;

      'jcheck: loop {
        #[cfg(feature = "native")]
        if let Some(_) = &crate::JIT_CACHE.get().unwrap_unchecked().0.get(&sectionid) {
          run_jit = true;
          break 'jcheck;
        };

        // Try to get a newer Pickle to run chocolate faster
        // If not there - keep the old arc
        // if let Some((inst, jumps)) = CODE_CACHE.get(&sectionid) {
        //   let marker = data = inst;
        //   t.ws.jmp = (0, jumps.get(&0).map(|x| *x).unwrap_or_default());

        //   ts.curline_or_resume.usi = *jumps
        //     .get(&ptr::read_unaligned(t.ws.arr as *const u8 as *const u64))
        //     .unwrap();
        //   t.ws.relocmap = jumps;
        // };

        let dt = data.as_ref();
        // We use loop-in-a-loop to correctly manage state!
        // eg, yield that makes it re-check for JIT
        loop {
          if ts.curline_or_resume.usi == leng {
            break 'jcheck;
          }

          let pickle = dt.get_unchecked(ts.curline_or_resume.usi);

          // USE A NO-OP to our benefit
          if pickle.opcode == PICKLE_OPCODE_HINT
            && [PICKLE_OPCODE_MARK].iter().any(|x| *x == pickle.u1)
          {
            ts.curline_or_resume.usi += 3;
            atmark = true;
            continue 'jcheck;
          }

          if pickle.opcode == PICKLE_OPCODE_HINT {
            let dptr = dt.as_ptr();
            ts.engine_or_pt.pt = dptr as _;
          }

          (PICKLE_DISPATCH_TABLE.get_unchecked(pickle.opcode as usize))(pickle, &mut t.ws, ts);

          ts.curline_or_resume.usi += 1;
        }
      }
    });

    #[cfg(feature = "native")]
    if run_jit {
      // TODO: Replace with `become`
      return self.dispatch_jit(sectionid);
    }

    cold_path();
  }

  #[cfg(feature = "native")]
  pub(crate) fn dispatch_jit(&self, _: u64) {}

  fn pickle_section(&self, sectionid: u64) {
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
    return self.call_section(sectionid);
  }
}
