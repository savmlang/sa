use std::os::raw::c_void;

use sart::ctr::{RegistryValue, VMTaskState};

macro_rules! value_based {
  (
    $(
      $($op:ident);+ on $ra:ident and $rb:ident
    ),+
  ) => {
    pastey::paste! {
      $($(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_cmp_ $op _ $ra _ $rb>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = task.$ra.data;
            let r2 = task.$rb.data;

            let r3 = r1.$op(&r2);

            task.r1 = RegistryValue { data: r3 as u64 };
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_cmp_ $op _ $ra _ $rb _ptr>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = *(task.$ra.ptr as *const u64);
            let r2 = *(task.$rb.ptr as *const u64);

            let r3 = r1.$op(&r2);

            task.r1 = RegistryValue { data: r3 as u64 };
          }
        }
      )+)+
    }
  };
}

value_based! {
  eq;ne;gt;lt;le;ge on r1 and r2,
  eq;ne;gt;lt;le;ge on r2 and r1,
  eq;ne;gt;lt;le;ge on r1 and r3,
  eq;ne;gt;lt;le;ge on r3 and r1,
  eq;ne;gt;lt;le;ge on r2 and r3,
  eq;ne;gt;lt;le;ge on r3 and r2
}
