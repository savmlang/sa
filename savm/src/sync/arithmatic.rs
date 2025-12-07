use std::os::raw::c_void;

use sart::ctr::{RegistryValue, VMTaskState};

macro_rules! value_based {
  (
    $($name:ident),+
  ) => {
    pastey::paste! {
      $(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ $name>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = task.r1.data;
            let r2 = task.r2.data;

            let r3 = r1.[<unchecked_ $name>](r2 as _);

            task.r3 = RegistryValue { data: r3 };
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ $name _ptr>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = *(task.r1.ptr as *const u64);
            let r2 = *(task.r2.ptr as *const u64);

            let r3 = r1.[<unchecked_ $name>](r2 as _);

            task.r3 = RegistryValue { data: r3 };
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ $name _mut>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = *(task.r1.ptr as *const u64);
            let r4 = &mut *(task.r4 as *mut u64);

            *r4 = r1.[<unchecked_ $name>](*r4 as _);
          }
        }
      )+
    }
  };
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_div(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = task.r1.data;
    let r2 = task.r2.data;

    let r3 = r1 / r2;

    task.r3 = RegistryValue { data: r3 };
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_div_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = *(task.r1.ptr as *const u64);
    let r2 = *(task.r2.ptr as *const u64);

    let r3 = r1 / r2;

    task.r3 = RegistryValue { data: r3 };
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_div_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = *(task.r1.ptr as *const u64);
    let r4 = &mut *(task.r4 as *mut u64);

    *r4 = r1 / *r4;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_rem(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = task.r1.data;
    let r2 = task.r2.data;

    let r3 = r1.wrapping_rem(r2);

    task.r3 = RegistryValue { data: r3 };
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_rem_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = *(task.r1.ptr as *const u64);
    let r4 = &mut *(task.r4 as *mut u64);

    *r4 = r1.wrapping_rem(*r4);
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_rem_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = *(task.r1.ptr as *const u64);
    let r2 = *(task.r2.ptr as *const u64);

    let r3 = r1.wrapping_rem(r2);

    task.r3 = RegistryValue { data: r3 };
  }
}

value_based! {
  add, sub, mul, shl, shr
}
