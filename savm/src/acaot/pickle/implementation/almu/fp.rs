use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::fp::{VFP, parse_vfp},
  },
  resolve_location_src,
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::{
  ops::{Add, Div, Mul, Sub},
  ptr,
};

fn arithprelude(
  pickle: &PickleInstruction,
  ws: *mut WorkingSet,
  task: *mut VMTaskState,
) -> (
  u8,
  u8,
  u32,
  *mut QuadPackedData,
  *mut QuadPackedData,
  *mut QuadPackedData,
  i32,
  i32,
  i32,
) {
  let VFP {
    instdef,
    count,
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_vfp(pickle, unsafe { (*ws).arr });

  let src1 = { resolve_location_src!(task => src1) };
  let src2 = { resolve_location_src!(task => src2) };
  let target = { resolve_location_src!(task => tgt) };

  (
    instdef, datatype, count, src1, src2, target, of_src1, of_src2, of_tgt,
  )
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

pub fn call_vaddf(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude(pickle, ws, taskstate);

  {
    match fptype {
      8 => intop!((count f64) target = src1 add src2 { offset1, offset2, offset_target }),
      9 => intop!((count f32) target = src1 add src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}

pub fn call_vsubf(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude(pickle, ws, taskstate);

  {
    match fptype {
      8 => intop!((count f64) target = src1 sub src2 { offset1, offset2, offset_target }),
      9 => intop!((count f32) target = src1 sub src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}

pub fn call_vmulf(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude(pickle, ws, taskstate);

  {
    match fptype {
      8 => intop!((count f64) target = src1 mul src2 { offset1, offset2, offset_target }),
      9 => intop!((count f32) target = src1 mul src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}

pub fn call_vdivf(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let (_, fptype, count, src1, src2, target, offset1, offset2, offset_target) =
    arithprelude(pickle, ws, taskstate);

  {
    match fptype {
      8 => intop!((count f64) target = src1 div src2 { offset1, offset2, offset_target }),
      9 => intop!((count f32) target = src1 div src2 { offset1, offset2, offset_target }),
      _ => unreachable!(),
    }
  }
}
