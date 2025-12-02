use std::os::raw::c_void;

use sart::ctr::{RegistryValue, VMTaskState};

use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign};

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

            let r3 = r1.[<bit $name>](&r2);

            task.r3 = RegistryValue { data: r3 };
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ $name _ptr>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = *(task.r1.ptr as *const u64);
            let r2 = &*(task.r2.ptr as *const u64);

            let r3 = r1.[<bit $name>](r2);

            task.r3 = RegistryValue { data: r3 };
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ $name _mut>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = &*(task.r1.ptr as *const u64);
            let r4 = &mut *(task.r4 as *mut u64);

            r4.[<bit $name _assign>](r1);
          }
        }
      )+
    }
  };
}

value_based! {
  and, or, xor
}
