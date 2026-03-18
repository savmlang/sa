use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};
use sart::ctr::VMTaskState;
use std::ptr::{self, addr_of_mut};

macro_rules! arithprelude {
    ($ws:ident, $task:ident) => {
      {
      // [<type tag (3 bits)> <count bit>] [Src1 (4-bits)] [Src2 (4-bits)] [Target1 (4-bits)] (16b)
      // [<Carry/Sigflow bit>] [<saturation bit>] [Padding] (16b)
      let flags = arrcastint!($ws, start = 0, stop = 4, u32);

      let instdefined = flags as u16;

      let topflags = (flags >> 16) as u16;
      let countbit = (topflags >> 12 as u8) & 0x01;
      let typetag = topflags >> 13 as u8;

      let count_data = arrcastint!($ws, start = 4, stop = 8, u32);

      let count = if (countbit == 0) {
        count_data
      } else {
        unsafe { $task.r1.u32 }
      };

      let offset1 = arrcastint!($ws, start = 8, stop = 12, i32);
      let offset2 = arrcastint!($ws, start = 12, stop = 16, i32);
      let offset3 = arrcastint!($ws, start = 16, stop = 20, i32);

      let src1 = {
        let src = (topflags >> 8 as u8) & 0x0F;

        resolve_location_src!($task => src)
      };

      let src2 = {
        let src = (topflags as u8) >> 4;

        resolve_location_src!($task => src)
      };

      let target = {
        let src = (topflags as u8) & 0x0F;

        resolve_location_src!($task => src)
      };

      (instdefined, typetag, count, src1, src2, target, offset1, offset2, offset3)
      }
    };
  }

macro_rules! intop {
  (($c:ident $t:ty) $target:ident = $s1:ident $op:ident $s2:ident { $t1:ident, $t2:ident, $t3:ident }) => {
    unsafe {
      let target = ($target as *mut $t).offset($t3 as _);
      let s1_ = ($s1 as *mut $t).offset($t1 as _);
      let s2_ = ($s2 as *mut $t).offset($t2 as _);

      for i in 0..$c {
        let t = target.add(i as _);
        let s1 = ptr::read_unaligned(s1_.add(i as _));
        let s2 = ptr::read_unaligned(s2_.add(i as _));
        ptr::write_unaligned(t, s1.$op(s2));
      }
    }
  };
}

macro_rules! wide_mul {
  (($c:ident $t:ty) $target:ident = $s1:ident $op:ident $s2:ident { $t1:ident, $t2:ident, $t3:ident }) => {
    unsafe {
      let target = ($target as *mut $t).offset($t3 as _);
      let s1_ = ($s1 as *mut $t).offset($t1 as _);
      let s2_ = ($s2 as *mut $t).offset($t2 as _);

      for i in 0..$c {
        let t_1 = target.add(2 * (i as usize));
        let t_2 = target.add(2 * (i as usize) + 1);
        let s1 = ptr::read_unaligned(s1_.add(i as _));
        let s2 = ptr::read_unaligned(s2_.add(i as _));

        // (lo, hi)
        let (a, b) = (s1).$op(s2);
        ptr::write_unaligned(t_1, a as _);
        ptr::write_unaligned(t_2, b);
      }
    }
  };
}

macro_rules! high_mul {
  (($c:ident $t:ty) $target:ident = $s1:ident $op:ident $s2:ident { $t1:ident, $t2:ident, $t3:ident }) => {
    unsafe {
      let target = ($target as *mut $t).offset($t3 as _);
      let s1_ = ($s1 as *mut $t).offset($t1 as _);
      let s2_ = ($s2 as *mut $t).offset($t2 as _);

      for i in 0..$c {
        let t = target.add(i as _);
        let s1 = ptr::read_unaligned(s1_.add(i as _));
        let s2 = ptr::read_unaligned(s2_.add(i as _));

        // (lo, hi)
        let (_, b) = s1.$op(s2);
        ptr::write_unaligned(t, b);
      }
    }
  };
}

macro_rules! intop_carry {
  (($t:ty) $target:ident = $s1:ident $op:ident $s2:ident carries $carry:expr => { $t1:ident, $t2:ident, $t3:ident }) => {
    unsafe {
      let t = ($target as *mut $t).offset($t3 as _);
      let s1 = ptr::read_unaligned(($s1 as *mut $t).offset($t1 as _));
      let s2 = ptr::read_unaligned(($s2 as *mut $t).offset($t2 as _));

      let carry = ptr::read_unaligned($carry as *mut $t) != 0;

      let output = (s1).$op(s2, carry);

      ptr::write_unaligned(t, output.0);
      ptr::write_unaligned(($carry as *mut $t), if output.1 { !0 } else { 0 });
    }
  };
}

pub fn call_vadd(_: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (instdefined, typetag, count, src1, src2, target, t1, t2, t3) = arithprelude!(ws, taskstate);

  // [<Carry/Sigflow bit>] [<saturation bit>] [Padding (14bits)] (16b)
  let carry = (instdefined >> 15) == 1; // gets the last bit
  let saturate = (instdefined >> 14 & 0b01) == 1; // gets the saturation bit

  debug_assert!(!(carry && saturate));
  debug_assert!(count != 0);
  debug_assert!(!((carry || saturate) && count != 1));

  {
    match (carry, saturate, typetag) {
      (true, _, tag) => match tag {
        0 => {
          intop_carry!((u64) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        1 => {
          intop_carry!((u32) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        2 => {
          intop_carry!((u16) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        3 => {
          intop_carry!((u8) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        4 => {
          intop_carry!((i64) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        5 => {
          intop_carry!((i32) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        6 => {
          intop_carry!((i16) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        7 => {
          intop_carry!((i8) target = src1 carrying_add src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        _ => todo!(),
      },
      (_, true, tag) => match tag {
        0 => intop!((count u64) target = src1 saturating_add src2 { t1, t2, t3 }),
        1 => intop!((count u32) target = src1 saturating_add src2 { t1, t2, t3 }),
        2 => intop!((count u16) target = src1 saturating_add src2 { t1, t2, t3 }),
        3 => intop!((count u8) target = src1 saturating_add src2 { t1, t2, t3 }),
        4 => intop!((count i64) target = src1 saturating_add src2 { t1, t2, t3 }),
        5 => intop!((count i32) target = src1 saturating_add src2 { t1, t2, t3 }),
        6 => intop!((count i16) target = src1 saturating_add src2 { t1, t2, t3 }),
        7 => intop!((count i8) target = src1 saturating_add src2 { t1, t2, t3 }),
        _ => panic!(),
      },
      (_, _, tag) => match tag {
        0 => intop!((count u64) target = src1 wrapping_add src2 { t1, t2, t3 }),
        1 => intop!((count u32) target = src1 wrapping_add src2 { t1, t2, t3 }),
        2 => intop!((count u16) target = src1 wrapping_add src2 { t1, t2, t3 }),
        3 => intop!((count u8) target = src1 wrapping_add src2 { t1, t2, t3 }),
        4 => intop!((count i64) target = src1 wrapping_add src2 { t1, t2, t3 }),
        5 => intop!((count i32) target = src1 wrapping_add src2 { t1, t2, t3 }),
        6 => intop!((count i16) target = src1 wrapping_add src2 { t1, t2, t3 }),
        7 => intop!((count i8) target = src1 wrapping_add src2 { t1, t2, t3 }),
        _ => panic!(),
      },
    }
  }
}

pub fn call_vsub(_: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (instdefined, typetag, count, src1, src2, target, t1, t2, t3) = arithprelude!(ws, taskstate);

  // [<Carry/Sigflow  [SBB]>] [<saturation bit>] [Padding (14bits)] (16b)
  let carry = (instdefined >> 15) == 1; // gets the last bit
  let saturate = (instdefined >> 14 & 0b01) == 1; // gets the saturation bit

  debug_assert!(!(carry && saturate));
  debug_assert!(count != 0);
  debug_assert!(!((carry || saturate) && count != 1));

  {
    match (carry, saturate, typetag) {
      (true, _, tag) => match tag {
        0 => {
          intop_carry!((u64) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        1 => {
          intop_carry!((u32) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        2 => {
          intop_carry!((u16) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        3 => {
          intop_carry!((u8) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        4 => {
          intop_carry!((i64) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        5 => {
          intop_carry!((i32) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        6 => {
          intop_carry!((i16) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        7 => {
          intop_carry!((i8) target = src1 borrowing_sub src2 carries addr_of_mut!(taskstate.r5) => { t1, t2, t3 })
        }
        _ => todo!(),
      },
      (_, true, tag) => match tag {
        0 => intop!((count u64) target = src1 saturating_sub src2 { t1, t2, t3 }),
        1 => intop!((count u32) target = src1 saturating_sub src2 { t1, t2, t3 }),
        2 => intop!((count u16) target = src1 saturating_sub src2 { t1, t2, t3 }),
        3 => intop!((count u8) target = src1 saturating_sub src2 { t1, t2, t3 }),
        4 => intop!((count i64) target = src1 saturating_sub src2 { t1, t2, t3 }),
        5 => intop!((count i32) target = src1 saturating_sub src2 { t1, t2, t3 }),
        6 => intop!((count i16) target = src1 saturating_sub src2 { t1, t2, t3 }),
        7 => intop!((count i8) target = src1 saturating_sub src2 { t1, t2, t3 }),
        _ => panic!(),
      },
      (_, _, tag) => match tag {
        0 => intop!((count u64) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        1 => intop!((count u32) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        2 => intop!((count u16) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        3 => intop!((count u8) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        4 => intop!((count i64) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        5 => intop!((count i32) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        6 => intop!((count i16) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        7 => intop!((count i8) target = src1 wrapping_sub src2 { t1, t2, t3 }),
        _ => panic!(),
      },
    }
  }
}

pub fn call_vmul(_: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (instdefined, typetag, count, src1, src2, target, t1, t2, t3) = arithprelude!(ws, taskstate);

  // [<Extended Flags (2 bits)>] [Padding (14 bits)]
  // The extended flags:
  // - x0: Output the 1st 32-bits (i.e. low bits)
  // - x1: Output the 2nd 32-bit (i.e. high bits)
  // - 1x: we use Wide Multiplication (target must be able to store upto 2x the count)
  // - 0x: we use Lossy Multiplication (this is only time the other bit is read)
  let eflags = (instdefined >> 14) as u8;

  let wide = (eflags & 0x03) == 1;
  let lowbits = (eflags & 0x01) == 0;

  debug_assert!(count != 0);

  {
    match (wide, lowbits, typetag) {
      // Wide Multiplication
      (true, _, tag) => match tag {
        0 => {
          wide_mul!((count u64) target = src1 widening_mul src2 { t1, t2, t3 })
        }
        1 => {
          wide_mul!((count u32) target = src1 widening_mul src2 { t1, t2, t3 })
        }
        2 => {
          wide_mul!((count u16) target = src1 widening_mul src2 { t1, t2, t3 })
        }
        3 => wide_mul!((count u8) target = src1 widening_mul src2 { t1, t2, t3 }),
        4 => {
          wide_mul!((count i64) target = src1 widening_mul src2 { t1, t2, t3 })
        }
        5 => {
          wide_mul!((count i32) target = src1 widening_mul src2 { t1, t2, t3 })
        }
        6 => {
          wide_mul!((count i16) target = src1 widening_mul src2 { t1, t2, t3 })
        }
        7 => wide_mul!((count i8) target = src1 widening_mul src2 { t1, t2, t3 }),
        _ => todo!(),
      },
      (_, true, tag) => match tag {
        0 => intop!((count u64) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        1 => intop!((count u32) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        2 => intop!((count u16) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        3 => intop!((count u8) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        4 => intop!((count i64) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        5 => intop!((count i32) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        6 => intop!((count i16) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        7 => intop!((count i8) target = src1 wrapping_mul src2 { t1, t2, t3 }),
        _ => panic!(),
      },
      (_, _, tag) => match tag {
        0 => high_mul!((count u64) target = src1 widening_mul src2 { t1, t2, t3 }),
        1 => high_mul!((count u32) target = src1 widening_mul src2 { t1, t2, t3 }),
        2 => high_mul!((count u16) target = src1 widening_mul src2 { t1, t2, t3 }),
        3 => high_mul!((count u8) target = src1 widening_mul src2 { t1, t2, t3 }),
        4 => high_mul!((count i64) target = src1 widening_mul src2 { t1, t2, t3 }),
        5 => high_mul!((count i32) target = src1 widening_mul src2 { t1, t2, t3 }),
        6 => high_mul!((count i16) target = src1 widening_mul src2 { t1, t2, t3 }),
        7 => high_mul!((count i8) target = src1 widening_mul src2 { t1, t2, t3 }),
        _ => panic!(),
      },
    }
  }
}

macro_rules! divlikeprelude {
    ($pickle:ident, $ws:ident, $task:ident) => {
      {
        let args = u16::from_ne_bytes([$pickle.u1, $pickle.u2]);

        let typetag = (args >> 12) as u8;

        let t1 = arrcastint!($ws, start = 8, stop = 12, i32);
                let t2 = arrcastint!($ws, start = 12, stop = 16, i32);
        let t3 = arrcastint!($ws, start = 16, stop = 20, i32);

        let src1 = {
          let src = (args >> 8 as u8) & 0x0F;

          resolve_location_src!($task => src)
        };

        let src2 = {
          let src = (args as u8) >> 4;

          resolve_location_src!($task => src)
        };

        let target = {
          let src = (args as u8) & 0x0F;

          resolve_location_src!($task => src)
        };

        (typetag, src1, src2, target, t1, t2, t3)
      }
    };
  }

pub fn call_div(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (typetag, src1, src2, target, t1, t2, t3) = divlikeprelude!(pickle, ws, taskstate);

  let count = 1;

  match typetag {
    0 => intop!((count u64) target = src1 strict_div src2 { t1, t2, t3 }),
    1 => intop!((count u32) target = src1 strict_div src2 { t1, t2, t3 }),
    2 => intop!((count u16) target = src1 strict_div src2 { t1, t2, t3 }),
    3 => intop!((count u8) target = src1 strict_div src2 { t1, t2, t3 }),
    4 => intop!((count i64) target = src1 strict_div src2 { t1, t2, t3 }),
    5 => intop!((count i32) target = src1 strict_div src2 { t1, t2, t3 }),
    6 => intop!((count i16) target = src1 strict_div src2 { t1, t2, t3 }),
    7 => intop!((count i8) target = src1 strict_div src2 { t1, t2, t3 }),
    _ => panic!(),
  }
}

pub fn call_rem(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (typetag, src1, src2, target, t1, t2, t3) = divlikeprelude!(pickle, ws, taskstate);

  let count = 1;

  match typetag {
    0 => intop!((count u64) target = src1 strict_rem src2 { t1, t2, t3 }),
    1 => intop!((count u32) target = src1 strict_rem src2 { t1, t2, t3 }),
    2 => intop!((count u16) target = src1 strict_rem src2 { t1, t2, t3 }),
    3 => intop!((count u8) target = src1 strict_rem src2 { t1, t2, t3 }),
    4 => intop!((count i64) target = src1 strict_rem src2 { t1, t2, t3 }),
    5 => intop!((count i32) target = src1 strict_rem src2 { t1, t2, t3 }),
    6 => intop!((count i16) target = src1 strict_rem src2 { t1, t2, t3 }),
    7 => intop!((count i8) target = src1 strict_rem src2 { t1, t2, t3 }),
    _ => panic!(),
  }
}
