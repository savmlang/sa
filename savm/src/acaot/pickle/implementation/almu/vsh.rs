use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::vsh::{VSH, parse_vsh},
  },
  resolve_location_src,
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::ptr;

macro_rules! bitop {
  (
    $(
      { $($t:ty $(as $e:ty)?),* } $f:ident => |$a:ident, $b:ident| $exp:expr
    ),*
  ) => {
    pastey::paste! {
      $(
        $(
          fn [<vop_ $f _ $t>](src1: *mut QuadPackedData, src2: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i32, offset2: i32, offset3: i32, count: u32) {
            unsafe {
              let s1 = (src1 as *mut $t).offset(offset1 as _);
              let s2 = (src2 as *mut $t).offset(offset2 as _);

              $(
                let s2 = s2 as *mut $e;

                assert!(size_of::<$t>() == size_of::<$e>());
                assert!(align_of::<$t>() == align_of::<$e>());
              )?

              let t1 = (src3 as *mut $t).offset(offset3 as _);

              for idx in 0..count {
                let $a: $t = ptr::read_unaligned(s1.add(idx as _));
                let $b = ptr::read_unaligned(s2.add(idx as _));

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
        fn(src1: *mut QuadPackedData, src2: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i32, offset2: i32, offset3: i32, count: u32); 16
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
  { u64, u32, u16, u8, i64 as u64, i32 as u32, i16 as u16, i8 as u8 } shl      => |a, b| {
    a.wrapping_shl(b as _)
  },
  { u64, u32, u16, u8, i64 as u64, i32 as u32, i16 as u16, i8 as u8 } shr      => |a, b| {
    a.wrapping_shr(b as _)
  }
}

const TYPE_COUNT: u8 = 8;

#[inline(always)]
const fn calc_offset(op: u8, ty: u8) -> usize {
  (op * TYPE_COUNT + ty) as _
}

// `vsh <flags as u16> <padding (6-bits)> <op bit (1-bit)> <count bit (1-bit)> <count in u32> <base src1 as i32> <amount i.e. src2 as i32> <base target1 as i32>`
pub fn call_vsh(pickle: &PickleInstruction, ws: &mut WorkingSet, ts: &mut VMTaskState) {
  unsafe {
    let VSH {
      op,
      flags_src1,
      flags_src2,
      flags_target,
      count,
      of_src1,
      of_src2,
      of_target,
      typ,
    } = parse_vsh(pickle, &ws.arr);

    let src1 = resolve_location_src!(ts => flags_src1);
    let src2 = resolve_location_src!(ts => flags_src2);
    let tg = resolve_location_src!(ts => flags_target);

    let offset = calc_offset(op, typ);
    (_DISPATCH.get_unchecked(offset))(
      src1,
      src2,
      tg,
      of_src1 as _,
      of_src2 as _,
      of_target as _,
      count,
    );
  }
}
