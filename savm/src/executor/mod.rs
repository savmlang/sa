use std::mem::zeroed;

use sart::ctr::{Instruction, VMTaskState};

use crate::{BytecodeResolver, VM};

impl<T: BytecodeResolver + Send + Sync + 'static> VM<T> {
  /// Please note that from this point onwards we'll purely compute the values
  /// and wont check any single thing
  ///
  /// This strictly runs module id `0` section `0`
  pub unsafe fn run(&mut self) {
    unsafe {
      let mut task = zeroed::<VMTaskState>();

      self.run_module(&mut task, 0)
    };
  }

  pub unsafe fn run_module(&mut self, state: &mut VMTaskState, region: u64) {
    let old = self.cursection;
    self.cursection = region;

    // Absolute performance
    unsafe {
      let module = self.code.get(&region).unwrap_unchecked();

      state.curline = 0;

      let module: &[Instruction] = &module;

      loop {
        if state.curline == module.len() {
          break;
        }

        // ACAoT compiler automatically converts
        // libcalls to native instructions
        let inst = module.get_unchecked(state.curline);

        let (u, f) = inst.fn_;
        f(self as *const _ as _, state as _, u);

        state.curline += 1;
      }
    };

    // Restore to old section
    self.cursection = old;
  }
}
