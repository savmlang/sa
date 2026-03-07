use std::ops::{BitAnd, BitOr, BitXor};
use std::os::raw::c_void;

use sart::ctr::{DispatchFn, VMTaskState};

macro_rules! value_based {
  () => {
    value_based! {
      generate
      and u8,
      or u8,
      xor u8,
      and u16,
      or u16,
      xor u16,
      and u32,
      or u32,
      xor u32,
      and u64,
      or u64,
      xor u64,
      and i8,
      or i8,
      xor i8,
      and i16,
      or i16,
      xor i16,
      and i32,
      or i32,
      xor i32,
      and i64,
      or i64,
      xor i64
    }
  };
  (
    generate $(
      $op:ident $data:ident
    ),*
  ) => {
    pastey::paste! {
      $(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_bitwise_ $op _data_is_ $data>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = task.r1.heap().$data;
            let r2 = task.r2.heap().$data;

            let r3 = r1.[<bit $op>](&r2);

            task.r1.heap().$data = r3;
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_bitwise_ $op _data_is_ $data _mut>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = task.r1.heap().$data;
            let r2 = task.r2.heap().$data;

            task.r2.heap().$data = r1.[<bit $op>](&r2);
          }
        }
      )*
    }
  };
}

pub const ANDOPS: u8 = 1;
pub const OROPS: u8 = 2;
pub const XOROPS: u8 = 3;

pub const ANDMUTOPS: u8 = 4;
pub const ORMUTOPS: u8 = 5;
pub const XORMUTOPS: u8 = 6;

pub fn inst_bitwise_handler(typ: u8, op: u8) -> DispatchFn {
  let f = match typ {
    // 0 = u8
    0 => match op {
      1 => inst_bitwise_and_data_is_u8,
      2 => inst_bitwise_or_data_is_u8,
      3 => inst_bitwise_xor_data_is_u8,
      4 => inst_bitwise_and_data_is_u8_mut,
      5 => inst_bitwise_or_data_is_u8_mut,
      6 => inst_bitwise_xor_data_is_u8_mut,
      _ => unreachable!(),
    },
    // 1 = u16
    1 => match op {
      1 => inst_bitwise_and_data_is_u16,
      2 => inst_bitwise_or_data_is_u16,
      3 => inst_bitwise_xor_data_is_u16,
      4 => inst_bitwise_and_data_is_u16_mut,
      5 => inst_bitwise_or_data_is_u16_mut,
      6 => inst_bitwise_xor_data_is_u16_mut,
      _ => unreachable!(),
    },
    // 2 = u32
    2 => match op {
      1 => inst_bitwise_and_data_is_u32,
      2 => inst_bitwise_or_data_is_u32,
      3 => inst_bitwise_xor_data_is_u32,
      4 => inst_bitwise_and_data_is_u32_mut,
      5 => inst_bitwise_or_data_is_u32_mut,
      6 => inst_bitwise_xor_data_is_u32_mut,
      _ => unreachable!(),
    },
    // 3 = u64
    3 => match op {
      1 => inst_bitwise_and_data_is_u64,
      2 => inst_bitwise_or_data_is_u64,
      3 => inst_bitwise_xor_data_is_u64,
      4 => inst_bitwise_and_data_is_u64_mut,
      5 => inst_bitwise_or_data_is_u64_mut,
      6 => inst_bitwise_xor_data_is_u64_mut,
      _ => unreachable!(),
    },
    // 4 = i8
    4 => match op {
      1 => inst_bitwise_and_data_is_i8,
      2 => inst_bitwise_or_data_is_i8,
      3 => inst_bitwise_xor_data_is_i8,
      4 => inst_bitwise_and_data_is_i8_mut,
      5 => inst_bitwise_or_data_is_i8_mut,
      6 => inst_bitwise_xor_data_is_i8_mut,
      _ => unreachable!(),
    },
    // 5 = i16
    5 => match op {
      1 => inst_bitwise_and_data_is_i16,
      4 => inst_bitwise_and_data_is_i16_mut,
      2 => inst_bitwise_or_data_is_i16,
      5 => inst_bitwise_or_data_is_i16_mut,
      3 => inst_bitwise_xor_data_is_i16,
      6 => inst_bitwise_xor_data_is_i16_mut,
      _ => unreachable!(),
    },
    // 6 = i32
    6 => match op {
      1 => inst_bitwise_and_data_is_i32,
      4 => inst_bitwise_and_data_is_i32_mut,
      2 => inst_bitwise_or_data_is_i32,
      5 => inst_bitwise_or_data_is_i32_mut,
      3 => inst_bitwise_xor_data_is_i32,
      6 => inst_bitwise_xor_data_is_i32_mut,
      _ => unreachable!(),
    },
    // 7 = i64
    7 => match op {
      1 => inst_bitwise_and_data_is_i64,
      4 => inst_bitwise_and_data_is_i64_mut,
      2 => inst_bitwise_or_data_is_i64,
      5 => inst_bitwise_or_data_is_i64_mut,
      3 => inst_bitwise_xor_data_is_i64,
      6 => inst_bitwise_xor_data_is_i64_mut,
      _ => unreachable!(),
    },
    _ => unreachable!(),
  };

  f
}

value_based! {}
