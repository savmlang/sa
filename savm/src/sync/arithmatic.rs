use std::ops::{Add, Div, Mul, Rem, Shl, Shr, Sub};
use std::os::raw::c_void;

use sart::ctr::{DispatchFn, VMTaskState};

macro_rules! value_based {
  () => {
    value_based! {
      generate
      add u8,
      add u16,
      add u32,
      add u64,
      add i8,
      add i16,
      add i32,
      add i64,
      sub u8,
      sub u16,
      sub u32,
      sub u64,
      sub i8,
      sub i16,
      sub i32,
      sub i64,
      mul u8,
      mul u16,
      mul u32,
      mul u64,
      mul i8,
      mul i16,
      mul i32,
      mul i64,
      shl u8,
      shl u16,
      shl u32,
      shl u64,
      shl i8,
      shl i16,
      shl i32,
      shl i64,
      shr u8,
      shr u16,
      shr u32,
      shr u64,
      shr i8,
      shr i16,
      shr i32,
      shr i64,
      div u8,
      div u16,
      div u32,
      div u64,
      div i8,
      div i16,
      div i32,
      div i64,
      rem u8,
      rem u16,
      rem u32,
      rem u64,
      rem i8,
      rem i16,
      rem i32,
      rem i64
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
        pub extern "C" fn [<inst_ $op _data_is_ $data>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = task.r1.heap().$data;
            let r2 = task.r2.heap().$data;

            let r3 = r1.$op(r2);

            task.r1.heap().$data = r3;
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_ $op _data_is_ $data _mut>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            let r1 = task.r1.heap().$data;
            let r2 = task.r2.heap().$data;

            task.r2.heap().$data = r1.$op(r2);
          }
        }
      )*
    }
  };
}

// Operation codes for Arithmetic Operations
pub const ADDOPS: u8 = 1;
pub const SUBOPS: u8 = 2;
pub const MULOPS: u8 = 3;
pub const DIVOPS: u8 = 4;
pub const REMOPS: u8 = 5;
pub const SHLOPS: u8 = 6;
pub const SHROPS: u8 = 7;

pub const ADDMUTOPS: u8 = 8;
pub const SUBMUTOPS: u8 = 9;
pub const MULMUTOPS: u8 = 10;
pub const DIVMUTOPS: u8 = 11;
pub const REMMUTOPS: u8 = 12;
pub const SHLMUTOPS: u8 = 13;
pub const SHRMUTOPS: u8 = 14;

pub fn inst_arithmetic_handler(typ: u8, op: u8) -> DispatchFn {
  // Helper macro to reduce repetition in the main function.
  // Maps the operation code to the correct function name suffix for a given type.
  macro_rules! map_op_to_func {
    ($data:ident, $op:expr) => {
      pastey::paste! {
        match $op {
          ADDOPS => [<inst_add_data_is_ $data>],
          SUBOPS => [<inst_sub_data_is_ $data>],
          MULOPS => [<inst_mul_data_is_ $data>],
          DIVOPS => [<inst_div_data_is_ $data>],
          REMOPS => [<inst_rem_data_is_ $data>],
          SHLOPS => [<inst_shl_data_is_ $data>],
          SHROPS => [<inst_shr_data_is_ $data>],

          ADDMUTOPS => [<inst_add_data_is_ $data _mut>],
          SUBMUTOPS => [<inst_sub_data_is_ $data _mut>],
          MULMUTOPS => [<inst_mul_data_is_ $data _mut>],
          DIVMUTOPS => [<inst_div_data_is_ $data _mut>],
          REMMUTOPS => [<inst_rem_data_is_ $data _mut>],
          SHLMUTOPS => [<inst_shl_data_is_ $data _mut>],
          SHRMUTOPS => [<inst_shr_data_is_ $data _mut>],
          _ => unreachable!("Invalid arithmetic operation code"),
        }
      }
    };
  }

  let f = match typ {
    // 0 = u8
    0 => map_op_to_func!(u8, op),
    // 1 = u16
    1 => map_op_to_func!(u16, op),
    // 2 = u32
    2 => map_op_to_func!(u32, op),
    // 3 = u64
    3 => map_op_to_func!(u64, op),
    // 4 = i8
    4 => map_op_to_func!(i8, op),
    // 5 = i16
    5 => map_op_to_func!(i16, op),
    // 6 = i32
    6 => map_op_to_func!(i32, op),
    // 7 = i64
    7 => map_op_to_func!(i64, op),
    _ => unreachable!("Invalid data type code"),
  };

  f
}

value_based! {}
