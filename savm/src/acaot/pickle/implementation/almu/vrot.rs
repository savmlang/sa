use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
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
          fn [<vbitop_ $f _ $t>](src1: *mut QuadPackedData, src2: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i32, offset2: i32, offset3: i32, count: u32) {
            unsafe {
              let s1 = (src1 as *mut $t).offset(offset1 as _);
              let $b = ptr::read_unaligned((src2 as *mut $t).offset(offset2 as _));

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
        fn(src1: *mut QuadPackedData, src2: *mut QuadPackedData, src3: *mut QuadPackedData, offset1: i32, offset2: i32, offset3: i32, count: u32); 16
      ] = [
        $(
          $(
            [<vbitop_ $f _ $t>]
          ),*
        ),*
      ];
    }
  };
}

bitop! {
  { u64, u32, u16, u8, i64, i32, i16, i8 } rotl      => |a, b| {
    let w = (std::mem::size_of_val(&a) * 8) as i64;
    let amt = (b as i64).rem_euclid(w) as u32;
    a.rotate_left(amt)
  },
  { u64, u32, u16, u8, i64, i32, i16, i8 } rotr      => |a, b| {
    let w = (std::mem::size_of_val(&a) * 8) as i64;
    let amt = (b as i64).rem_euclid(w) as u32;
    a.rotate_right(amt)
  }
}

const TYPE_COUNT: u8 = 8;

#[inline(always)]
const fn calc_offset(op: u8, ty: u8) -> usize {
  (op * TYPE_COUNT + ty) as _
}

// `vrot <flags as u16> <padding (6-bits)> <rotation bit (1-bit)> <count bit (1-bit)> <count in u32> <base src1 as i32> <amount src i.e. src2 as i32> <base target1 as i32>`
pub fn call_vrot(pickle: &PickleInstruction, ws: *mut WorkingSet, ts: *mut VMTaskState) {
  unsafe {
    let rot = pickle.u3;

    let op = rot & 0x01;

    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

    let typ = (flags >> 12) as u8;
    let count = {
      let countdata = arrcastint!(ws, start = 0, stop = 4, u32);

      countdata
    };

    let flags_src1 = (flags as u8) & 0x0F;
    let flags_src2 = (flags as u8) >> 4 & 0x0F;
    let flags_tg = (flags >> 12) as u8 & 0x0F;

    let src1 = resolve_location_src!(ts => flags_src1);
    let src2 = resolve_location_src!(ts => flags_src2);
    let tg = resolve_location_src!(ts => flags_tg);

    let of_src1 = arrcastint!(ws, start = 4, stop = 8, i32);
    let of_src2 = arrcastint!(ws, start = 8, stop = 12, i32);
    let of_tg = arrcastint!(ws, start = 12, stop = 16, i32);

    let offset = calc_offset(op, typ);
    (_DISPATCH.get_unchecked(offset))(src1, src2, tg, of_src1, of_src2, of_tg, count);
  }
}
