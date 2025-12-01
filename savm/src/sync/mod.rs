use std::{
  ffi::c_void,
  mem::{replace, zeroed},
  ptr::{self, null_mut},
};

use sart::{
  boxed::drop_rtbox,
  ctr::{REGISTER_SET_SIZE, RegistryValue, VMTaskState},
};

mod arithmatic;

pub use arithmatic::*;

use crate::{BytecodeResolver, VM};

pub extern "C" fn inst_clr(_: *mut c_void, task: *mut VMTaskState, u1: u64) {
  unsafe {
    let task = &mut *task;

    match u1 {
      1 => task.r1 = RegistryValue { data: 0 },
      2 => task.r2 = RegistryValue { data: 0 },
      3 => task.r3 = RegistryValue { data: 0 },
      // Mutable Ones
      4 => task.r4 = null_mut(),
      5 => task.r5 = null_mut(),
      // Contained Value
      6 => {
        if !task.r6.is_null() {
          drop_rtbox(task.r6);
          task.r6 = null_mut();
        }
      }
      _ => unreachable!(),
    }
  }
}

pub extern "C" fn inst_clr_full(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    if !task.r6.is_null() {
      drop_rtbox(task.r6);
    }

    ptr::write_bytes(
      &mut task.r1 as *mut RegistryValue as *mut u8, // Start address of r1
      0,                                             // Byte value to write (0)
      REGISTER_SET_SIZE,                             // Total bytes to clear (64 bytes)
    );
  }
}

pub extern "C" fn inst_sync_libcall<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  u: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    vm.counter += 1;

    if vm.counter > 10 {
      let mut state = Box::new(zeroed::<VMTaskState>());

      state.super_ = task as _;

      vm.run_module(state.as_mut(), u);

      drop(state);

      vm.counter -= 1;
      return;
    }

    let mut state = zeroed::<VMTaskState>();

    state.super_ = task as _;

    vm.run_module(&mut state, u);

    drop(state);
    vm.counter -= 1;
  }
}

pub extern "C" fn new_context(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let new_task: VMTaskState = zeroed();

    let old_task = Box::into_raw(Box::new(replace(&mut *task, new_task)));

    let task = &mut *task;

    task.super_ = old_task;
  }
}

pub extern "C" fn restore_context(_: *mut c_void, task: *mut VMTaskState, u: u64) {
  unsafe {
    let old_task_moved_from_heap = *Box::from_raw((*task).super_);

    let new_task = &mut *task;
    drop(replace(new_task, old_task_moved_from_heap));
  }
}
