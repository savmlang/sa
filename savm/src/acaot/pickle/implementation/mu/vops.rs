use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve, resolve_location_src,
};
use sart::ctr::VMTaskState;
use std::{
  ops::Neg,
  ptr::{self, addr_of_mut},
};

macro_rules! prelude {
  ($pickle:ident, $ws:ident, $task:ident) => {
    {

    // `vneg <flags as u16 [2 bytes]> <count in u32> <base src1 as i32> <base target1 as i32>`
    //
    //   <type tag (4 bits)> [Src1 (4-bits)] [Target1 (4-bits)] <count bit> [Padding (3bits)]
    let f1 = $pickle.u1;
    let f2 = $pickle.u2;

    let flags = u16::from_ne_bytes([f1, f2]);

    let typetag = (flags >> 12) as u8;

    let countbit = ((flags >> 4) & 0x01) as u8;

    let count_data = arrcastint!($ws, start = 0, stop = 4, u32);

    let count = if (countbit == 0) {
      count_data
    } else {
      unsafe { $task.r1.u32 }
    };

    let offset1 = arrcastint!($ws, start = 4, stop = 8, i32);
    let offset2 = arrcastint!($ws, start = 8, stop = 12, i32);

    let src1 = unsafe {
      let src = (flags >> 8 as u8) & 0x0F;

      resolve_location_src!($task => src)
    };

    let target = unsafe {
      let src = ((flags >> 4) as u8) & 0x0F;

      resolve_location_src!($task => src)
    };

    (count, typetag, src1, target, offset1, offset2)
    }
  };
}

macro_rules! intop {
  (($c:ident $t:ty) $target:ident = $s1:ident $op:ident { offset1 = $of1:ident, offsetTarget = $oft:ident }) => {
    unsafe {
      for i in 0..$c {
        let t = ($target as *mut $t).offset($oft as _).add(i as _);
        let s1 = ptr::read_unaligned(($s1 as *mut $t).offset($of1 as _).add(i as _));

        ptr::write_unaligned(t, s1.$op());
      }
    }
  };
}

pub fn call_vneg(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (count, typetag, src1, target, offset1, offset2) = prelude!(pickle, ws, taskstate);

  match typetag {
    4 => {
      intop!((count i64) target = src1 strict_neg { offset1 = offset1, offsetTarget = offset2 })
    }
    5 => {
      intop!((count i32) target = src1 strict_neg { offset1 = offset1, offsetTarget = offset2 })
    }
    6 => {
      intop!((count i16) target = src1 strict_neg { offset1 = offset1, offsetTarget = offset2 })
    }
    7 => {
      intop!((count i8) target = src1 strict_neg { offset1 = offset1, offsetTarget = offset2 })
    }
    8 => intop!((count f64) target = src1 neg { offset1 = offset1, offsetTarget = offset2 }),
    9 => intop!((count f32) target = src1 neg { offset1 = offset1, offsetTarget = offset2 }),
    _ => panic!("Invalid type to neg"),
  }
}

pub fn call_vabs(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (count, typetag, src1, target, offset1, offset2) = prelude!(pickle, ws, taskstate);

  match typetag {
    4 => {
      intop!((count i64) target = src1 strict_abs { offset1 = offset1, offsetTarget = offset2 })
    }
    5 => {
      intop!((count i32) target = src1 strict_abs { offset1 = offset1, offsetTarget = offset2 })
    }
    6 => {
      intop!((count i16) target = src1 strict_abs { offset1 = offset1, offsetTarget = offset2 })
    }
    7 => {
      intop!((count i8) target = src1 strict_abs { offset1 = offset1, offsetTarget = offset2 })
    }
    8 => intop!((count f64) target = src1 abs { offset1 = offset1, offsetTarget = offset2 }),
    9 => intop!((count f32) target = src1 abs { offset1 = offset1, offsetTarget = offset2 }),
    _ => panic!("Invalid type to neg"),
  }
}
