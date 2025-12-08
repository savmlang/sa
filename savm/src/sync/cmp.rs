use std::os::raw::c_void;

use sart::ctr::{DispatchFn, VMTaskState};

macro_rules! value_based {
  () => {
    value_based! {
      generate
      eq u8,
      lt u8,
      gt u8,
      ne u8,
      le u8,
      ge u8,
      eq u16,
      lt u16,
      gt u16,
      ne u16,
      le u16,
      ge u16,
      eq u32,
      lt u32,
      gt u32,
      ne u32,
      le u32,
      ge u32,
      eq u64,
      lt u64,
      gt u64,
      ne u64,
      le u64,
      ge u64,
      eq i8,
      lt i8,
      gt i8,
      ne i8,
      le i8,
      ge i8,
      eq i16,
      lt i16,
      gt i16,
      ne i16,
      le i16,
      ge i16,
      eq i32,
      lt i32,
      gt i32,
      ne i32,
      le i32,
      ge i32,
      eq i64,
      lt i64,
      gt i64,
      ne i64,
      le i64,
      ge i64
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
        pub extern "C" fn [<inst_cmp_ $op _data_is_ $data>](_: *mut c_void, task: *mut VMTaskState, regdata: u64) {
          unsafe {
            let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_be_bytes();

            let task = &mut *task;

            let r1 = match r1loc {
              1 => task.r1.heap().$data,
              2 => task.r2.heap().$data,
              3 => task.r3.heap().$data,
              4 => task.r4.heap().$data,
              5 => task.r5.heap().$data,
              6 => task.r6.heap().$data,
              _ => unreachable!()
            };
            let r2 = match r2loc {
              1 => task.r1.heap().$data,
              2 => task.r2.heap().$data,
              3 => task.r3.heap().$data,
              4 => task.r4.heap().$data,
              5 => task.r5.heap().$data,
              6 => task.r6.heap().$data,
              _ => unreachable!()
            };

            let r3 = r1.$op(&r2);

            task.r1.heap().$data = if r3 {1} else {0};
          }
        }
      )*
    }
  };
}

pub fn inst_cmp_handler(typ: u8, op: u8, r1: u8, r2: u8) -> (u64, DispatchFn) {
  let data64 = u64::from_be_bytes([r1, r2, 0, 0, 0, 0, 0, 0]);

  let f = match typ {
    // 0 = u8
    0 => match op {
      1 => inst_cmp_eq_data_is_u8,
      2 => inst_cmp_ne_data_is_u8,
      3 => inst_cmp_gt_data_is_u8,
      4 => inst_cmp_lt_data_is_u8,
      5 => inst_cmp_ge_data_is_u8,
      6 => inst_cmp_le_data_is_u8,
      _ => unreachable!(),
    },
    // 1 = u16
    1 => match op {
      1 => inst_cmp_eq_data_is_u16,
      2 => inst_cmp_ne_data_is_u16,
      3 => inst_cmp_gt_data_is_u16,
      4 => inst_cmp_lt_data_is_u16,
      5 => inst_cmp_ge_data_is_u16,
      6 => inst_cmp_le_data_is_u16,
      _ => unreachable!(),
    },
    // 2 = u32
    2 => match op {
      1 => inst_cmp_eq_data_is_u32,
      2 => inst_cmp_ne_data_is_u32,
      3 => inst_cmp_gt_data_is_u32,
      4 => inst_cmp_lt_data_is_u32,
      5 => inst_cmp_ge_data_is_u32,
      6 => inst_cmp_le_data_is_u32,
      _ => unreachable!(),
    },
    // 3 = u64
    3 => match op {
      1 => inst_cmp_eq_data_is_u64,
      2 => inst_cmp_ne_data_is_u64,
      3 => inst_cmp_gt_data_is_u64,
      4 => inst_cmp_lt_data_is_u64,
      5 => inst_cmp_ge_data_is_u64,
      6 => inst_cmp_le_data_is_u64,
      _ => unreachable!(),
    },
    // 4 = i8
    4 => match op {
      1 => inst_cmp_eq_data_is_i8,
      2 => inst_cmp_ne_data_is_i8,
      3 => inst_cmp_gt_data_is_i8,
      4 => inst_cmp_lt_data_is_i8,
      5 => inst_cmp_ge_data_is_i8,
      6 => inst_cmp_le_data_is_i8,
      _ => unreachable!(),
    },
    // 5 = i16
    5 => match op {
      1 => inst_cmp_eq_data_is_i16,
      2 => inst_cmp_ne_data_is_i16,
      3 => inst_cmp_gt_data_is_i16,
      4 => inst_cmp_lt_data_is_i16,
      5 => inst_cmp_ge_data_is_i16,
      6 => inst_cmp_le_data_is_i16,
      _ => unreachable!(),
    },
    // 6 = i32
    6 => match op {
      1 => inst_cmp_eq_data_is_i32,
      2 => inst_cmp_ne_data_is_i32,
      3 => inst_cmp_gt_data_is_i32,
      4 => inst_cmp_lt_data_is_i32,
      5 => inst_cmp_ge_data_is_i32,
      6 => inst_cmp_le_data_is_i32,
      _ => unreachable!(),
    },
    // 7 = i64
    7 => match op {
      1 => inst_cmp_eq_data_is_i64,
      2 => inst_cmp_ne_data_is_i64,
      3 => inst_cmp_gt_data_is_i64,
      4 => inst_cmp_lt_data_is_i64,
      5 => inst_cmp_ge_data_is_i64,
      6 => inst_cmp_le_data_is_i64,
      _ => unreachable!(),
    },
    _ => unreachable!(),
  };

  (data64, f)
}

value_based! {}
