use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::{
      cast::{VFCAST, parse_vfcast},
      vfop::{FOP_CEIL, FOP_FLOOR, FOP_ROUND, FOP_SQRT, FOP_TRUNC, VFOP, parse_vfop},
    },
  },
  resolve_location_src,
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::ptr;

macro_rules! intop {
  (($c:ident $t:ty) $target:ident = $s1:ident $op:ident { $offset1:ident, $offset_target:ident }) => {
    unsafe {
      let dest = ($target as *mut $t).offset($offset_target as _);
      let src1 = ($s1 as *mut $t).offset($offset1 as _);

      for i in 0..$c {
        let t = dest.add(i as _);
        let s1 = ptr::read_unaligned(src1.add(i as _));

        ptr::write_unaligned(t, s1.$op());
      }
    }
  };
}

// `vfop <flags as u16 [2 bytes]> <count in u32> <base src1 as i32> <base target1 as i32>`
//
// Flags are like this:
//   [padding (3-bits)] [float type (1 bit)] [Src1 (4-bits)] [Target1 (4-bits)] [count bit (1-bit)] [Sub-Op (3-bit)]
pub fn call_vfop(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let VFOP {
    src,
    target,
    offset_src: offset1,
    offset_target: offset2,
    count,
    subop,
    typetag,
  } = parse_vfop(pickle, ws.arr.as_ref());

  let src1 = resolve_location_src!(taskstate => src);
  let target1 = resolve_location_src!(taskstate => target);

  match typetag {
    // f64
    8 => match subop {
      FOP_CEIL => intop!((count f64) target1 = src1 ceil { offset1, offset2 }),
      FOP_FLOOR => intop!((count f64) target1 = src1 floor { offset1, offset2 }),
      FOP_TRUNC => intop!((count f64) target1 = src1 trunc { offset1, offset2 }),
      FOP_ROUND => intop!((count f64) target1 = src1 round { offset1, offset2 }),
      FOP_SQRT => intop!((count f64) target1 = src1 sqrt { offset1, offset2 }),
      _ => panic!(),
    },
    // f32
    9 => match subop {
      FOP_CEIL => intop!((count f32) target1 = src1 ceil { offset1, offset2 }),
      FOP_FLOOR => intop!((count f32) target1 = src1 floor { offset1, offset2 }),
      FOP_TRUNC => intop!((count f32) target1 = src1 trunc { offset1, offset2 }),
      FOP_ROUND => intop!((count f32) target1 = src1 round { offset1, offset2 }),
      FOP_SQRT => intop!((count f32) target1 = src1 sqrt { offset1, offset2 }),
      _ => panic!(),
    },
    _ => panic!(),
  }
}

// `vfcast <flags as u16 [2 bytes]> <count in u32> <base src1 as i32> <base target1 as i32>`
//
// Flags are like this:
//   [Padding] [count bit (1-bit)] [op (1-bit)] [f width (1-bit)] [int type tag (3 bits)] [Src1 (4-bits)] [Target1 (4-bits)]
pub fn call_vfcast(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let VFCAST {
    offset_src,
    offset_target,
    count,
    src,
    target,
    type_initial,
    type_final,
  } = parse_vfcast(pickle, ws.arr.as_ref());

  let f = match (type_initial, type_final) {
    // f64 -> iN
    (8, 0) => as_cast::<f64, u64>,
    (8, 1) => as_cast::<f64, u32>,
    (8, 2) => as_cast::<f64, u16>,
    (8, 3) => as_cast::<f64, u8>,
    (8, 4) => as_cast::<f64, i64>,
    (8, 5) => as_cast::<f64, i32>,
    (8, 6) => as_cast::<f64, i16>,
    (8, 7) => as_cast::<f64, i8>,

    // f32 -> iN
    (9, 0) => as_cast::<f32, u64>,
    (9, 1) => as_cast::<f32, u32>,
    (9, 2) => as_cast::<f32, u16>,
    (9, 3) => as_cast::<f32, u8>,
    (9, 4) => as_cast::<f32, i64>,
    (9, 5) => as_cast::<f32, i32>,
    (9, 6) => as_cast::<f32, i16>,
    (9, 7) => as_cast::<f32, i8>,

    // iN -> f32
    (0, 9) => as_cast::<u64, f32>,
    (1, 9) => as_cast::<u32, f32>,
    (2, 9) => as_cast::<u16, f32>,
    (3, 9) => as_cast::<u8, f32>,
    (4, 9) => as_cast::<i64, f32>,
    (5, 9) => as_cast::<i32, f32>,
    (6, 9) => as_cast::<i16, f32>,
    (7, 9) => as_cast::<i8, f32>,

    // iN -> f64
    (0, 8) => as_cast::<u64, f64>,
    (1, 8) => as_cast::<u32, f64>,
    (2, 8) => as_cast::<u16, f64>,
    (3, 8) => as_cast::<u8, f64>,
    (4, 8) => as_cast::<i64, f64>,
    (5, 8) => as_cast::<i32, f64>,
    (6, 8) => as_cast::<i16, f64>,
    (7, 8) => as_cast::<i8, f64>,

    _ => unreachable!(),
  };

  let src1 = resolve_location_src!(taskstate => src);
  let target1 = resolve_location_src!(taskstate => target);
  f(src1, target1, offset_src, offset_target, count);
}

trait CastTo<Target> {
  fn castto(&self) -> Target;
}

macro_rules! impl_casto {
  (
    $(
      $($a:ty),+ : $b:ty
    )*
  ) => {
    $($(
      impl CastTo<$b> for $a {
        fn castto(&self) -> $b {
          *self as _
        }
      }

      impl CastTo<$a> for $b {
        fn castto(&self) -> $a {
          *self as _
        }
      }
    )*)*
  };
}

impl_casto! {
  u8,u16,u32,u64,i8,i16,i32,i64 : f32
  u8,u16,u32,u64,i8,i16,i32,i64 : f64
}

fn as_cast<T, E>(
  src1: *mut QuadPackedData,
  target: *mut QuadPackedData,
  offsetsrc: i32,
  offsettgt: i32,
  count: u32,
) where
  T: CastTo<E>,
{
  unsafe {
    let src1 = (src1 as *mut T).offset(offsetsrc as _);
    let target = (target as *mut E).offset(offsettgt as _);

    for c in 0..count {
      let r = ptr::read_unaligned(src1.add(c as _));

      ptr::write_unaligned(target.add(c as _), r.castto());
    }
  }
}
