use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::ptr;

trait Fma {
  /// Calculates `(self*a)+b` but rounding ONLY once
  ///
  /// Please note that FMA provides higher precision than simple MUL, ADD sequence
  fn fma(&self, a: Self, b: Self) -> Self;
}

impl Fma for f32 {
  fn fma(&self, a: Self, b: Self) -> Self {
    self.mul_add(a, b)
  }
}

impl Fma for f64 {
  fn fma(&self, a: Self, b: Self) -> Self {
    self.mul_add(a, b)
  }
}

const _DISPATCH: [fn(
  src1: *mut QuadPackedData,
  src2: *mut QuadPackedData,
  src3: *mut QuadPackedData,
  tg: *mut QuadPackedData,
  of1: i32,
  of2: i32,
  of3: i32,
  oft: i32,
  count: u32,
); 2] = [vop_fma::<f64>, vop_fma::<f32>];

fn vop_fma<T>(
  src1: *mut QuadPackedData,
  src2: *mut QuadPackedData,
  src3: *mut QuadPackedData,
  tg: *mut QuadPackedData,
  of1: i32,
  of2: i32,
  of3: i32,
  oft: i32,
  count: u32,
) where
  T: Fma,
{
  unsafe {
    let s1 = (src1 as *mut T).offset(of1 as _);
    let s2 = (src2 as *mut T).offset(of2 as _);
    let s3 = (src3 as *mut T).offset(of3 as _);
    let t = (tg as *mut T).offset(oft as _);

    for idx in 0..(count as usize) {
      let s = ptr::read_unaligned(s1.add(idx));
      let s_a = ptr::read_unaligned(s2.add(idx));
      let s_b = ptr::read_unaligned(s3.add(idx));

      let tg = t.add(idx);
      ptr::write_unaligned(tg, s.fma(s_a, s_b));
    }
  }
}

#[inline(always)]
const fn calc_offset(ty: u8) -> usize {
  ty as _
}

// ## Syntax
// `vfma <flags as u16> <padding [6bits]> <float type> <count bit> <count in u32> <base src1 as i32> <base src2 as i32> <base src3 as i32> <base target1 as i32>`
//
// The carry is stored exactly how `cmp` stores it, you can jif for overflow (and select your type, unsigned or unsigned) to get the carry bit
//
// # Type tag is defined above
// The flags is split like this into (4-bits + 4 x 4-bit parts):
//   [Src1] [Src2] [Src3] [Target1]
pub fn call_vfma(pickle: &PickleInstruction, ws: &mut WorkingSet, ts: &mut VMTaskState) {
  unsafe {
    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

    let floattype = (pickle.u3 >> 1) & 0x01;

    let countbit = pickle.u3 & 0x01;
    let count = {
      let countdata = arrcastint!(ws, start = 0, stop = 4, u32);

      if countbit == 0 { countdata } else { ts.r1.u32 }
    };

    let flags_src1 = (flags >> 12) as u8 & 0x0F;
    let flags_src2 = (flags >> 8) as u8 & 0x0F;
    let flags_src3 = (flags >> 4) as u8 & 0x0F;
    let flags_tg = flags as u8 & 0x0F;

    let src1 = resolve_location_src!(ts => flags_src1);
    let src2 = resolve_location_src!(ts => flags_src2);
    let src3 = resolve_location_src!(ts => flags_src3);
    let tg = resolve_location_src!(ts => flags_tg);

    let of_src1 = arrcastint!(ws, start = 4, stop = 8, i32);
    let of_src2 = arrcastint!(ws, start = 8, stop = 12, i32);
    let of_src3 = arrcastint!(ws, start = 12, stop = 16, i32);
    let of_tg = arrcastint!(ws, start = 16, stop = 20, i32);

    let offset = calc_offset(floattype);
    (_DISPATCH.get_unchecked(offset))(
      src1, src2, src3, tg, of_src1, of_src2, of_src3, of_tg, count,
    );
  }
}
