use std::{
  mem::{replace, zeroed},
  os::raw::c_void,
  thread::yield_now,
};

use sart::{boxed::RTSafeBoxWrapper, ctr::VMTaskState, futures::FutureTask, map::HeapStructure};

use crate::{BytecodeResolver, GLOBAL_RUNTIME, VM};

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

    let wrap = replace(&mut task.r6, zeroed());

    *(vm.heapmap as *mut HeapStructure).add(addr as usize) = wrap.data;
  }
}

macro_rules! defineload {
  (
    $(
      $name:ident
    ),*
  ) => {
    pastey::paste! {
      $(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_load_to_ $name>]<T: BytecodeResolver + Send + Sync + 'static>(
          vm: *mut c_void,
          task: *mut VMTaskState,
          addr: u64,
        ) {
          unsafe {
            let vm = &mut *(vm as *mut VM<T>);
            let task = &mut *task;

            task.$name = HeapStructure {
              complex: (vm.heapmap as *mut HeapStructure).add(addr as usize) as *mut _ as _
            };
          }
        }
      )*
    }
  };
}

defineload! {
  r1, r2, r3
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_load_to_r4<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    task.r4 = *((vm.heapmap as *mut HeapStructure).add(addr as _) as *mut _);
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_load_to_r5<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    task.r5.ptr = (vm.heapmap as *mut HeapStructure).add(addr as _) as *mut _ as _;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_yield(_: *mut c_void, _: *mut VMTaskState, _: u64) {
  yield_now();
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_sync_await(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let wrap = RTSafeBoxWrapper::construct::<FutureTask>(task.r6.heap().complex);

    *task.r6.heap() = HeapStructure {
      complex: GLOBAL_RUNTIME
        .block_on(async move { wrap.into_future().await })
        .into_raw(),
    };
  }
}
