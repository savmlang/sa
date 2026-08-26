use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::vminimax::{VMINIMAX, parse_vminimax},
  },
  resolve_location_src,
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::ptr;

macro_rules! bitop {
  (
    $(
      { $($t:ty),* } $f:ident => |$a:ident, $b:ident| $exp:expr
    ),*
  ) => {
    pastey::paste! {
      $(
        $(
          fn [<vop_ $f _ $t>](src1: *mut QuadPackedData, src2: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i8, offset2: i8, offset3: i8, count: u32) {
            unsafe {
              let s1 = (src1 as *mut $t).add(offset1 as _);
              let s2 = (src2 as *mut $t).add(offset2 as _);
              let t1 = (src3 as *mut $t).add(offset3 as _);

              for idx in 0..count {
                let $a: $t = ptr::read_unaligned(s1.add(idx as _));
                let $b: $t = ptr::read_unaligned(s2.add(idx as _));

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
        fn(src1: *mut QuadPackedData, src2: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i8, offset2: i8, offset3: i8, count: u32); 20
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
  { u64, u32, u16, u8, i64, i32, i16, i8, f64, f32 } min      => |a, b| {
    a.min(b)
  },
  { u64, u32, u16, u8, i64, i32, i16, i8, f64, f32 } max      => |a, b| {
    a.max(b)
  }
}

const TYPE_COUNT: u8 = 10;

#[inline(always)]
const fn calc_offset(op: u8, ty: u8) -> usize {
  (op * TYPE_COUNT + ty) as _
}

// `vminimax <flags as u16> <padding (5-bits)> <aligned (1-bit)> <count bit (1-bit)> <Max (1-bit)> <count in u32> <base src1 as i32> <base src2 as i32> <base target1 as i32>`
//
// # Type tag is defined above
// The flags is split like this into (4-bits + 3 x 4-bit parts):
//   [Type Tag (4-bits)] [Src1] [Src2] [Target1]
pub fn call_vminimax(pickle: &PickleInstruction, ws: *mut WorkingSet, ts: *mut VMTaskState) {
  unsafe {
    let VMINIMAX {
      op,
      flags_src1,
      flags_src2,
      flags_target,
      count,
      of_src1,
      of_src2,
      of_target,
      typ,
      ..
    } = parse_vminimax(pickle, (*ws).arr);

    let src1 = resolve_location_src!(ts => flags_src1);
    let src2 = resolve_location_src!(ts => flags_src2);
    let tg = resolve_location_src!(ts => flags_target);

    let offset = calc_offset(op, typ);
    (_DISPATCH.get_unchecked(offset))(src1, src2, tg, of_src1, of_src2, of_target, count);
  }
}
