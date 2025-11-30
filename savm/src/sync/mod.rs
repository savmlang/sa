use std::{
  ffi::c_void,
  mem::zeroed,
  ptr::{self, null_mut},
};

use sart::{
  boxed::drop_rtbox,
  ctr::{RegistryValue, VMTaskState},
};

use crate::{BytecodeResolver, VM};

pub extern "C" fn inst_clr(_: *mut c_void, task: *mut VMTaskState, u1: u64) {
  unsafe {
    let task = &mut *task;

    match u1 {
      1 => task.r1 = RegistryValue { data: 0 },
      2 => task.r2 = RegistryValue { data: 0 },
      3 => task.r3 = RegistryValue { data: 0 },
      4 => task.r4 = RegistryValue { data: 0 },
      5 => task.r5 = RegistryValue { data: 0 },
      // Mutable Ones
      6 => task.r6 = null_mut(),
      7 => task.r7 = null_mut(),
      // Contained Value
      8 => {
        if !task.r8.is_null() {
          drop_rtbox(task.r8);
          task.r8 = 0 as _;
        }
      }
      _ => unreachable!(),
    }
  }
}

pub extern "C" fn inst_clr_full(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    if !task.r8.is_null() {
      drop_rtbox(task.r8);
    }

    ptr::write_bytes(
      &mut task.r1 as *mut RegistryValue as *mut u8, // Start address of r1
      0,                                             // Byte value to write (0)
      5 * size_of::<RegistryValue>() + 3 * size_of::<*const c_void>(), // Total bytes to clear (64 bytes)
    );
  }
}

pub extern "C" fn inst_sync_libcall<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  u: u64,
) {
  unsafe {
    let vm = &*(vm as *const VM<T>);
    let task = &mut *task;

    if task.counter > 10 {
      let mut state = Box::new(zeroed::<VMTaskState>());

      state.super_ = task as _;

      vm.run_module(state.as_mut(), u);

      drop(state);
      return;
    }

    let mut state = zeroed::<VMTaskState>();

    state.super_ = task as _;

    vm.run_module(&mut state, u);

    drop(state);
  }
}
