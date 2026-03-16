use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve, resolve_location_src,
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::ptr::{self, addr_of_mut};

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
pub fn vfop(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let flags = u16::from_le_bytes([pickle.u1, pickle.u2]);

  let count_data = arrcastint!(ws, start = 0, stop = 4, u32);

  let offset1 = arrcastint!(ws, start = 4, stop = 8, i32);
  let offset2 = arrcastint!(ws, start = 8, stop = 12, i32);

  let subop = (flags as u8) & 0x7;

  let countbit = ((flags as u8) >> 3) & 0x1;
  let count = if (countbit == 0) {
    count_data
  } else {
    unsafe { taskstate.r1.u32 }
  };

  let target1 = (flags as u8) >> 4;

  let src1 = (flags >> 8) as u8 & 0xF;

  let float_type = ((flags >> 12) as u8) & 0x1;

  match float_type {
    // f64
    0 => match subop {
      0 => intop!((count f64) target1 = src1 ceil { offset1, offset2 }),
      1 => intop!((count f64) target1 = src1 floor { offset1, offset2 }),
      2 => intop!((count f64) target1 = src1 trunc { offset1, offset2 }),
      3 => intop!((count f64) target1 = src1 round { offset1, offset2 }),
      _ => panic!(),
    },
    // f32
    1 => match subop {
      0 => intop!((count f32) target1 = src1 ceil { offset1, offset2 }),
      1 => intop!((count f32) target1 = src1 floor { offset1, offset2 }),
      2 => intop!((count f32) target1 = src1 trunc { offset1, offset2 }),
      3 => intop!((count f32) target1 = src1 round { offset1, offset2 }),
      _ => panic!(),
    },
    _ => panic!(),
  }
}

// `vfcast <flags as u16 [2 bytes]> <count in u32> <base src1 as i32> <base target1 as i32>`
//
// Flags are like this:
//   [Padding] [count bit (1-bit)] [op (1-bit)] [f width (1-bit)] [int type tag (3 bits)] [Src1 (4-bits)] [Target1 (4-bits)]
pub fn vfcast(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let flags = u16::from_le_bytes([pickle.u1, pickle.u2]);

  let target1 = {
    let tg = (flags as u8) & 0x0F;

    resolve_location_src!(taskstate => tg)
  };
  let src1 = {
    let s1 = ((flags >> 4) as u8) & 0x0F;

    resolve_location_src!(taskstate => s1)
  };
  let typetag = (flags >> 8) as u8 & 0x07;
  let fwidth = (flags >> 11) as u8 & 0x01;
  let op = (flags >> 12) as u8 & 0x01;
  let countbit = (flags >> 13) as u8 & 0x01;

  let count_data = arrcastint!(ws, start = 0, stop = 4, u32);

  let offset1 = arrcastint!(ws, start = 4, stop = 8, i32);
  let offset2 = arrcastint!(ws, start = 8, stop = 12, i32);

  let count = if (countbit == 0) {
    count_data
  } else {
    unsafe { taskstate.r1.u32 }
  };

  // Match and dispatch
  let f = match fwidth {
    // f64
    0 => match op {
      // f* to i*
      0 => 
        match typetag {
          0 => as_cast::<f64, u64>,
          1 => as_cast::<f64, u32>,
          2 => as_cast::<f64, u16>,
          3 => as_cast::<f64, u8>,
          4 => as_cast::<f64, i64>,
          5 => as_cast::<f64, i32>,
          6 => as_cast::<f64, i16>,
          7 => as_cast::<f64, i8>,
          _ => panic!(),
        }
      ,
      // i* to f*
      1 => match typetag {
          0 => as_cast::<u64, f64>,
          1 => as_cast::<u32, f64>,
          2 => as_cast::<u16, f64>,
          3 => as_cast::<u8, f64>,
          4 => as_cast::<i64, f64>,
          5 => as_cast::<i32, f64>,
          6 => as_cast::<i16, f64>,
          7 => as_cast::<i8, f64>,
          _ => panic!(),
        }
      _ => panic!(),
    },
    // f64
    1 => match op {
      // f* to i*
      0 => 
        match typetag {
          0 => as_cast::<f32, u64>,
          1 => as_cast::<f32, u32>,
          2 => as_cast::<f32, u16>,
          3 => as_cast::<f32, u8>,
          4 => as_cast::<f32, i64>,
          5 => as_cast::<f32, i32>,
          6 => as_cast::<f32, i16>,
          7 => as_cast::<f32, i8>,
          _ => panic!(),
        }
      ,
      // i* to f*
      1 => match typetag {
          0 => as_cast::<u64, f32>,
          1 => as_cast::<u32, f32>,
          2 => as_cast::<u16, f32>,
          3 => as_cast::<u8, f32>,
          4 => as_cast::<i64, f32>,
          5 => as_cast::<i32, f32>,
          6 => as_cast::<i16, f32>,
          7 => as_cast::<i8, f32>,
          _ => panic!(),
        }
      _ => panic!(),
    },
    _ => panic!(),
  };

  f(src1, target1, offset1, offset2, count);
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
)
where
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