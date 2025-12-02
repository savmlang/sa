use std::{
  ffi::c_void,
  mem::{replace, zeroed},
  ptr::{self, null_mut},
};

use sart::{
  boxed::{drop_rtbox, peek},
  ctr::{REGISTER_SET_SIZE, RegistryValue, VMTaskState},
};

mod alc;
mod arithmatic;
mod bitwise;
mod cmp;

pub use alc::*;
pub use arithmatic::*;
pub use bitwise::*;
pub use cmp::*;

use crate::{BytecodeResolver, VM};

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_r1(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    task.r1 = RegistryValue { data: 0 };
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_r2(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    task.r2 = RegistryValue { data: 0 };
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_r3(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    task.r3 = RegistryValue { data: 0 };
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_r4(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    task.r4 = null_mut();
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_r5(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    task.r5 = null_mut();
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_r6(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    debug_assert!(
      !task.r6.is_null(),
      "inst_clr_r6 called when R6 was null. Use inst_clr_r6_unsure"
    );

    // We assume that r6 is already null, if not this is undefined behaviour
    drop_rtbox(task.r6);
    task.r6 = null_mut();
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_r6_unsure(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    if !task.r6.is_null() {
      // We assume that r6 is already null, if not this is undefined behaviour
      drop_rtbox(task.r6);
      task.r6 = null_mut();
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_jmp(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    task.curline = index as usize;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_jz_data(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    // Compare the inline 64-bit value directly. No pointer dereference.
    if task.r1.data == 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
/// Conditional Jump (Non-Zero): Checks R1's *INLINE DATA* field for non-zero.
/// FAST PATH: Avoids memory dereference. Used for integer/immediate comparisons.
pub extern "C" fn inst_jnz_data(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    // Compare the inline 64-bit value directly. No pointer dereference.
    if task.r1.data != 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
/// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
/// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
pub extern "C" fn inst_jz_ptr(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;
    // Assumes R1.ptr is pointing to a valid single byte (u8) for comparison.
    let data = *(task.r1.ptr as *const u8);

    if data == 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
/// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
/// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
pub extern "C" fn inst_jnz_ptr(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;
    // Assumes R1.ptr is pointing to a valid single byte (u8) for comparison.
    let data = *(task.r1.ptr as *const u8);

    if data != 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_jz_r6(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    let data = peek::<u8>(task.r6 as _);

    if data == 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_jnz_r6(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    let data = peek::<u8>(task.r6 as _);

    if data != 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_jz_r6_unsure(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    if task.r6.is_null() {
      task.curline = index as usize;
      return;
    }

    let data = peek::<u8>(task.r6 as _);

    if data == 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_jnz_r6_unsure(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    if task.r6.is_null() {
      return;
    }

    let data = peek::<u8>(task.r6 as _);

    if data != 0 {
      task.curline = index as usize;
    }
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
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

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_sync_libcall<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  u: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    let real_heap = replace(&mut vm.heapmap, zeroed());

    vm.counter += 1;

    if vm.counter > 10 {
      let mut state = Box::new(zeroed::<VMTaskState>());

      state.super_ = task as _;

      vm.run_module(state.as_mut(), u);

      drop(state);

      vm.counter -= 1;

      drop(replace(&mut vm.heapmap, real_heap));
      return;
    }

    let mut state = zeroed::<VMTaskState>();

    state.super_ = task as _;

    vm.run_module(&mut state, u);

    drop(state);

    drop(replace(&mut vm.heapmap, real_heap));
    vm.counter -= 1;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn new_context<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  _: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);

    vm.heaprestore = Box::into_raw(Box::new(replace(&mut vm.heapmap, zeroed())));

    let new_task: VMTaskState = zeroed();

    let old_task = Box::into_raw(Box::new(replace(&mut *task, new_task)));

    let task = &mut *task;

    task.super_ = old_task;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn restore_context<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  _: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);

    let old_heapmap = *Box::from_raw(vm.heaprestore);

    drop(replace(&mut vm.heapmap, old_heapmap));

    vm.heaprestore = null_mut();

    let old_task_moved_from_heap = *Box::from_raw((*task).super_);

    let new_task = &mut *task;
    drop(replace(new_task, old_task_moved_from_heap));
  }
}
