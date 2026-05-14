use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::cast::{CAST, parse_cast},
  },
  resolve_location_src,
};
use sart::ctr::VMTaskState;
use std::ptr;

macro_rules! cast_grammar {
  (
    src1 = $src1:ident, tag_initial = $ti:ident
    target = $target:ident, tag_final = $tf:ident
    offset1 = $of1:ident, offset2 = $of2:ident

    // Repeat
    $(
      { $id:expr } $src:ty => {
        $(
          { $id2:expr } $typ:ty
        ),*
      }
    ),*
  ) => {
    match $ti {
      $(
        $id => {
          let s1 = ($src1 as *mut $src).offset($of1 as _);
          let src1 = ptr::read_unaligned(s1);

          match $tf {
            $(
              $id2 => {
                let target = ($target as *mut $typ).offset($of2 as _);

                // Do the cast using `as`
                ptr::write_unaligned(target, src1 as _);
              }
            ),*
            _ => panic!("Wrong tag_final")
          }
        }
      ),*
      _ => panic!("Wrong tag_initial")
    }
  };
}

pub fn call_cast(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  unsafe {
    // The 16-bit args are distributed as follows (4x4-bit slices):
    //   [Type tag Initial] [Type tag Final] [Src1] [Target1]
    let CAST {
      offset_src: offset1,
      offset_target: offset2,
      src,
      target,
      type_initial: tag_initial,
      type_final: tag_final,
    } = parse_cast(pickle, ws.arr);

    let src1 = resolve_location_src!(taskstate => src);
    let target = resolve_location_src!(taskstate => target);

    cast_grammar!(
      src1 = src1, tag_initial = tag_initial
      target = target, tag_final = tag_final
      offset1 = offset1, offset2 = offset2

      { 0 } u64 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },
      { 1 } u32 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },
      { 2 } u16 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },
      { 3 } u8 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },

      // Signed
      { 4 } i64 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },
      { 5 } i32 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },
      { 6 } i16 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },
      { 7 } i8 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8
      },

      // Floating
      { 8 } f64 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8,

        // Float Native
        { 8 } f64,
        { 9 } f32
      },
      { 9 } f32 => {
        { 0 } u64,
        { 1 } u32,
        { 2 } u16,
        { 3 } u8,
        { 4 } i64,
        { 5 } i32,
        { 6 } i16,
        { 7 } i8,

        // Float Native
        { 8 } f64,
        { 9 } f32
      }
    );
  }
}
