use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::ptr;

macro_rules! bitop {
  (
    $(
      { $($t:ty $(as $e:ty)?),* } $f:ident => |$a:ident| $exp:expr
    ),*
  ) => {
    pastey::paste! {
      $(
        $(
          fn [<vop_ $f _ $t>](src1: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i32, offset3: i32, count: u32) {
            unsafe {
              let s1 = (src1 as *mut $t).offset(offset1 as _);
              let t1 = (src3 as *mut $t).offset(offset3 as _);

              for idx in 0..count {
                let $a: $t = ptr::read_unaligned(s1.add(idx as _));

                let t2 = t1.add(idx as _);

                ptr::write_unaligned(t2, {
                  $exp
                });
              }
            }
          }
        )*
      )*

      const _DISPATCH: [
        fn(src1: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i32, offset3: i32, count: u32); 16
      ] = [
        $(
          $(
            [<vop_ $f _ $t>]
          ),*
        ),*
      ];
    }
  };
}

bitop! {
  { u64, u32, u16, u8} popcnt      => |a| {
    a.count_ones() as _
  },
  { u64, u32, u16, u8 } clz      => |a| {
    a.leading_zeros() as _
  },
  { u64, u32, u16, u8} cls      => |a| {
    a.leading_ones() as _
  },
  { u64, u32, u16, u8 } ctz      => |a| {
    a.trailing_zeros() as _
  }
}

const TYPE_COUNT: u8 = 4;

#[inline(always)]
const fn calc_offset(op: u8, ty: u8) -> usize {
  (op * TYPE_COUNT + ty) as _
}

// `vcnt <flags as u16 [2 bytes]> <count in u32> <base src1 as i32> <base target1 as i32>`
// Flags are like this:
//   [<width (2 bits)> <padding> <count bit>] [Src1 (4-bits)] [Target1 (4-bits)] [Op (4-bits)]
pub fn call_vcnt(pickle: &PickleInstruction, ws: &mut WorkingSet, ts: &mut VMTaskState) {
  unsafe {
    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

    let op = (flags as u8) & 0x0F;

    let typ = (flags >> 14) as u8;

    let countbit = (flags >> 12) as u8 & 0x01;

    let count = {
      let countdata = arrcastint!(ws, start = 0, stop = 4, u32);

      if countbit == 0 { countdata } else { ts.r1.u32 }
    };

    let flags_src1 = (flags >> 8) as u8 & 0x0F;
    let flags_tg = (flags >> 4) as u8 & 0x0F;

    let src1 = resolve_location_src!(ts => flags_src1);
    let tg = resolve_location_src!(ts => flags_tg);

    let of_src1 = arrcastint!(ws, start = 4, stop = 8, i32);
    let of_tg = arrcastint!(ws, start = 8, stop = 12, i32);

    let offset = calc_offset(op, typ);
    (_DISPATCH.get_unchecked(offset))(src1, tg, of_src1, of_tg, count);
  }
}
