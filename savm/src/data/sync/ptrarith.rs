use std::os::raw::c_void;

use sart::ctr::{DispatchFn, VMTaskState};

pub fn isize_to_u64(value: isize) -> u64 {
  // === Case 1: 64-bit Architecture (isize == i64) ===
  #[cfg(target_pointer_width = "64")]
  return {
    // On 64-bit, isize is i64. The 'as' cast performs the desired
    // sign-extension from i64 to u64, which is a lossless operation.
    // This is equivalent to what you might attempt with transmute (but safe).

    value.cast_unsigned() as _
  };

  // === Case 2: 32-bit Architecture (isize == i32) ===
  #[cfg(target_pointer_width = "32")]
  {
    let usized = value.cast_unsigned();

    usized as u64
  }
}

pub fn u64_to_isize(value: u64) -> isize {
  // === Case 1: 64-bit Architecture (isize == i64) ===
  #[cfg(target_pointer_width = "64")]
  return {
    // On 64-bit, isize is i64. The 'as' cast performs the desired
    // sign-extension from i64 to u64, which is a lossless operation.
    // This is equivalent to what you might attempt with transmute (but safe).

    (value as usize).cast_signed() as _
  };

  // === Case 2: 32-bit Architecture (isize == i32) ===
  #[cfg(target_pointer_width = "32")]
  {
    let usized = (value as u32).cast_signed();

    usized as _
  }
}

macro_rules! value_based {
  () => {
    value_based! {
      generate
      r1, r2, r3, r4, r5, r6
    }
  };
  (
    generate $(
      $reg1:ident
    ),*
  ) => {
    pastey::paste! {
      $(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ptr_add $reg1>](_: *mut c_void, task: *mut VMTaskState, data: u64) {
          unsafe {
            let task = &mut *task;

            task.$reg1.heap().complex = task.$reg1.heap().complex.add(data as _);
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ptr_sub $reg1>](_: *mut c_void, task: *mut VMTaskState, data: u64) {
          unsafe {
            let task = &mut *task;

            task.$reg1.heap().complex = task.$reg1.heap().complex.sub(data as _);
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ptr_offset $reg1>](_: *mut c_void, task: *mut VMTaskState, data: u64) {
          unsafe {
            let task = &mut *task;

            task.$reg1.heap().complex = task.$reg1.heap().complex.offset(u64_to_isize(data as _));
          }
        }
      )*
    }
  };
}

pub const PTRADDOP: u8 = 0;
pub const PTRSUBOP: u8 = 1;
pub const PTROFFSETOP: u8 = 2;

pub fn inst_ptrarith_handler(reg1: u8, op: u8) -> DispatchFn {
  let f = match reg1 {
    1 => match op {
      0 => inst_ptr_addr1,
      1 => inst_ptr_subr1,
      2 => inst_ptr_offsetr1,
      _ => unreachable!(),
    },
    2 => match op {
      0 => inst_ptr_addr2,
      1 => inst_ptr_subr2,
      2 => inst_ptr_offsetr2,
      _ => unreachable!(),
    },
    3 => match op {
      0 => inst_ptr_addr3,
      1 => inst_ptr_subr3,
      2 => inst_ptr_offsetr3,
      _ => unreachable!(),
    },
    4 => match op {
      0 => inst_ptr_addr4,
      1 => inst_ptr_subr4,
      2 => inst_ptr_offsetr4,
      _ => unreachable!(),
    },
    5 => match op {
      0 => inst_ptr_addr5,
      1 => inst_ptr_subr5,
      2 => inst_ptr_offsetr5,
      _ => unreachable!(),
    },
    6 => match op {
      0 => inst_ptr_addr6,
      1 => inst_ptr_subr6,
      2 => inst_ptr_offsetr6,
      _ => unreachable!(),
    },
    _ => unreachable!(),
  };

  f
}

value_based! {}
