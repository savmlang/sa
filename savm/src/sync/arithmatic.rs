use std::os::raw::c_void;

use sart::{
  boxed::peek,
  ctr::{RegistryValue, VMTaskState},
};

macro_rules! value_based {
  (
    $($name:ident),+
  ) => {
    pastey::paste! {
      $(
        pub extern "C" fn [<inst_ $name>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = task.r1.data;
            let r2 = task.r2.data;

            let r3 = r1.[<unchecked_ $name>](r2 as _);

            task.r3 = RegistryValue { data: r3 };
          }
        }

        pub extern "C" fn [<inst_ $name _mut>](_: *mut c_void, task: *mut VMTaskState, u: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = *(task.r1.ptr as *const u64);
            let r2 = task.r2.ptr;

            let r3 = r1.[<unchecked_ $name>](r2 as _);

            task.r3 = RegistryValue { data: r3 };
          }
        }
      )+
    }
  };
}

pub extern "C" fn inst_div(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = task.r1.data;
    let r2 = task.r2.data;

    let r3 = r1 / r2;

    task.r3 = RegistryValue { data: r3 };
  }
}

pub extern "C" fn inst_rem(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    let r1 = task.r1.data;
    let r2 = task.r2.data;

    let r3 = r1.wrapping_rem(r2);

    task.r3 = RegistryValue { data: r3 };
  }
}

value_based! {
  add, sub, mul, shl, shr
}
