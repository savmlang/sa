use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::vminimax::{VCNT, parse_vcnt},
  },
  resolve_location_src,
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

pub fn call_vcnt(pickle: &PickleInstruction, ws: *mut WorkingSet, ts: *mut VMTaskState) {
  unsafe {
    let VCNT {
      op,
      flags_src,
      flags_target,
      count,
      of_src,
      of_target,
      typ,
      ..
    } = parse_vcnt(pickle, unsafe { (*ws).arr });

    let src = resolve_location_src!(ts => flags_src);
    let target = resolve_location_src!(ts => flags_target);

    let offset = calc_offset(op, typ);
    (_DISPATCH.get_unchecked(offset))(src, target, of_src as _, of_target as _, count);
  }
}
