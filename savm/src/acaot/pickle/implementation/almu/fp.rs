use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};
use sart::ctr::VMTaskState;
use std::{
  ops::{Add, Div, Mul, Sub},
  ptr,
};

macro_rules! arithprelude {
  ($pickle:ident, $ws:ident, $task:ident) => {
    {
    // `vaddf <flags as u16> <count in u32> <base src1 as i32> <base src2 as i32> <base target1 as i32>`
    // The flags is split like this into (4-bits + 3 x 4-bit parts):
    //   [0 <inst defined> <float type> <count bit>] [Src1] [Src2] [Target1]
    let f1 = $pickle.u1;
    let f2 = $pickle.u2;

    let flags = u16::from_ne_bytes([f1, f2]);

    let countbit = ((flags >> 12) & 0x01) as u8;
    let fptype = ((flags >> 13) & 0x01) as u8;
    let inst = ((flags >> 14) & 0x01) as u8;

    let count_data = arrcastint!($ws, start = 0, stop = 4, u32);

    let count = if (countbit == 0) {
      count_data
    } else {
      unsafe { $task.r1.u32 }
    };

    let offset1 = arrcastint!($ws, start = 4, stop = 8, i32);
    let offset2 = arrcastint!($ws, start = 8, stop = 12, i32);
    let offset3 = arrcastint!($ws, start = 12, stop = 16, i32);

    let src1 = {
      let src = (flags >> 8 as u8) & 0x0F;

      resolve_location_src!($task => src)
    };

    let src2 = {
      let src = (flags as u8) >> 4;

      resolve_location_src!($task => src)
    };

    let target = {
      let src = (flags as u8) & 0x0F;

      resolve_location_src!($task => src)
    };

    (inst, fptype, count, src1, src2, target, offset1, offset2, offset3)
    }
  };
}

macro_rules! intop {
  (($c:ident $t:ty) $target:ident = $s1:ident $op:ident $s2:ident { $offset1:ident, $offset2:ident, $offset_target:ident }) => {
    unsafe {
      let dest = ($target as *mut $t).offset($offset_target as _);
      let src1 = ($s1 as *mut $t).offset($offset1 as _);
      let src2 = ($s2 as *mut $t).offset($offset2 as _);

      for i in 0..$c {
        let t = dest.add(i as _);
        let s1 = ptr::read_unaligned(src1.add(i as _));
        let s2 = ptr::read_unaligned(src2.add(i as _));

        ptr::write_unaligned(t, s1.$op(s2));
      }
    }
  };
}

pub fn call_vaddf(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude!(pickle, ws, taskstate);

  {
    match fptype {
      0 => intop!((count f64) target = src1 add src2 { offset1, offset2, offset_target }),
      1 => intop!((count f32) target = src1 add src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}

pub fn call_vsubf(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude!(pickle, ws, taskstate);

  {
    match fptype {
      0 => intop!((count f64) target = src1 sub src2 { offset1, offset2, offset_target }),
      1 => intop!((count f32) target = src1 sub src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}

pub fn call_vmulf(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude!(pickle, ws, taskstate);

  {
    match fptype {
      0 => intop!((count f64) target = src1 mul src2 { offset1, offset2, offset_target }),
      1 => intop!((count f32) target = src1 mul src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}

pub fn call_vdivf(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude!(pickle, ws, taskstate);

  {
    match fptype {
      0 => intop!((count f64) target = src1 div src2 { offset1, offset2, offset_target }),
      1 => intop!((count f32) target = src1 div src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}
