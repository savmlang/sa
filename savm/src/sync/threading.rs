use core::ffi::c_void;
use std::{mem::zeroed, thread::spawn};

use sart::{ctr::VMTaskState, map::HeapStructure};

use crate::{BytecodeResolver, MaybeBoxed, VM};

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_sync_spawn<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  module: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);

    let task = &mut *task;

    let (r8, vm) = vm.create_copy();

    *task.r6.heap() = HeapStructure { complex: r8 };

    spawn(move || match vm {
      MaybeBoxed::Unboxed(mut vm) => {
        let mut state: VMTaskState = zeroed();

        vm.run_module(&mut state, module);
      }
      MaybeBoxed::Boxed(mut vm) => {
        let mut state: Box<VMTaskState> = Box::new(zeroed());

        vm.run_module(&mut state, module);
      }
    });
  }
}
