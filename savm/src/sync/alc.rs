use std::{
  mem::{replace, swap, zeroed},
  os::raw::c_void,
  ptr::null_mut,
  thread::yield_now,
};

use sart::{
  boxed::{ContainedRTBox, RTSafeBoxWrapper},
  ctr::{RegistryValue, VMTaskState},
  futures::FutureTask,
};

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

    let wrap = task.r6;

    task.r6 = null_mut();

    swap(
      vm.heapmap.get_unchecked_mut(addr as usize),
      &mut ContainedRTBox::new(wrap),
    );
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

    swap(
      vm.heapmap.get_unchecked_mut(addr as usize),
      &mut ContainedRTBox::new(wrap),
    );
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

            task.$name = RegistryValue {
              ptr: vm.heapmap.get_unchecked_mut(addr as usize) as *mut _ as _
            };
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_load_to_ $name _super>]<T: BytecodeResolver + Send + Sync + 'static>(
          vm: *mut c_void,
          task: *mut VMTaskState,
          addr: u64,
        ) {
          unsafe {
            let vm = &mut *(vm as *mut VM<T>);
            let superheap = &*vm.heaprestore;

            let task = &mut *task;

            task.$name = RegistryValue { ptr: superheap.get_unchecked(addr as usize) as *const _ as _ };
          }
        }
      )*
    }
  };
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_load_to_r4_super<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let superheap = &*vm.heaprestore;

    let task = &mut *task;

    task.r4 = superheap.get_unchecked(addr as usize) as *const _ as _;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_load_to_r5_super<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let superheap = &*vm.heaprestore;

    let task = &mut *task;

    task.r4 = superheap.get_unchecked(addr as usize) as *const _ as _;
  }
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

    task.r4 = vm.heapmap.get_unchecked_mut(addr as usize) as *mut _ as _;
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

    task.r5 = vm.heapmap.get_unchecked_mut(addr as usize) as *mut _ as _;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_free<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  _: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);

    drop(replace(
      vm.heapmap.get_unchecked_mut(addr as usize),
      zeroed(),
    ));
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_own<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    task.r6 = replace(vm.heapmap.get_unchecked_mut(addr as usize), zeroed()).into_raw();
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_free_super<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  _: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(&mut *(vm as *mut VM<T>)).heaprestore;

    drop(replace(vm.get_unchecked_mut(addr as usize), zeroed()));
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_own_super<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  addr: u64,
) {
  unsafe {
    let vm = &mut *(&mut *(vm as *mut VM<T>)).heaprestore;
    let task = &mut *task;

    task.r6 = replace(vm.get_unchecked_mut(addr as usize), zeroed()).into_raw();
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

    let wrap = RTSafeBoxWrapper::construct::<FutureTask>(task.r6);

    task.r6 = GLOBAL_RUNTIME
      .block_on(async move { wrap.into_future().await })
      .into_raw();
  }
}
