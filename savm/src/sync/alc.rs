use std::{os::raw::c_void, ptr::null_mut};

use sart::{boxed::ContainedRTBox, ctr::VMTaskState};

use crate::{BytecodeResolver, VM, pack_u64};

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_alloc<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    let wrap = task.r6;

    task.r6 = null_mut();

    let module = vm.cursection;
    let address = addr;

    vm.heap
      .insert(pack_u64(module, address), ContainedRTBox::new(wrap));
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_alloc_super<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *(&mut *task).super_;

    let wrap = task.r6;

    task.r6 = null_mut();

    let module = vm.cursection;
    let address = addr;

    vm.heap
      .insert(pack_u64(module, address), ContainedRTBox::new(wrap));
  }
}
