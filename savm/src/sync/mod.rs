use std::{cell::UnsafeCell, collections::HashMap, hint::cold_path, mem::zeroed};

use sart::{ctr::VMTaskState, salloc, structures::QuadPackedData};

use crate::{
  BytecodeResolver, CODE_CACHE, JIT_CACHE, VM,
  acaot::pickle::{
    def::{PICKLE_DISPATCH_TABLE, PICKLE_OPCODE_HINT, PICKLE_OPCODE_JMP, PICKLE_OPCODE_MARK},
    implementation::{SIZE_128KB, WorkingSet},
  },
};

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
      arr: [0u8;20],
      largepad: unsafe { salloc::aligned_malloc(SIZE_128KB, 8) as _ },
      largepad_cursor: 0,
      relocmap: HashMap::default()
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

impl<T: BytecodeResolver + Send + Sync + 'static> VM<T> {
  pub fn call_section(&self, sectionid: u64) {
    let Some(data) = CODE_CACHE.get(&sectionid) else {
      // TODO: Replace with `become`
      return self.pickle_section(sectionid);
    };

    let leng = data.len();
    let dt = data.as_ref();

    let mut run_jit = false;

    VMSTAT.with(|x| unsafe {
      let t = &mut *x.get();

      let ts = t.ts.get_unchecked_mut(t.cindex);

      ts.engine_or_pt.pt = self as *const _ as _;
      ts.curline_or_resume.usi = 0;

      'jcheck: loop {
        if let Some(_) = &JIT_CACHE.get().unwrap_unchecked().0.get(&sectionid) {
          run_jit = true;
          break 'jcheck;
        };

        // We use loop-in-a-loop to correctly manage state!
        // eg, yield that makes it re-check for JIT
        loop {
          if ts.curline_or_resume.usi == leng {
            break 'jcheck;
          }

          let pickle = dt.get_unchecked(ts.curline_or_resume.usi);

          // USE A NO-OP to our benefit
          if pickle.opcode == PICKLE_OPCODE_HINT
            && [PICKLE_OPCODE_MARK, PICKLE_OPCODE_JMP]
              .iter()
              .any(|x| *x == pickle.u1)
          {
            ts.curline_or_resume.usi += 1;
            continue 'jcheck;
          }

          (PICKLE_DISPATCH_TABLE.get_unchecked(pickle.opcode as usize))(pickle, &mut t.ws, ts);

          ts.curline_or_resume.usi += 1;
        }
      }
    });

    if run_jit {
      // TODO: Replace with `become`
      return self.dispatch_jit(sectionid);
    }

    cold_path();
  }

  pub(crate) fn dispatch_jit(&self, _: u64) {}

  fn pickle_section(&self, sectionid: u64) {
    // Compile

    // TODO: Replace with `become`
    return self.call_section(sectionid);
  }
}
