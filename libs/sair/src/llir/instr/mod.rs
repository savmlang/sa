use loc::LocSrc;
use sart::ctr::*;
use std::fmt::Formatter;

use crate::{
  llir::instr::flags::{Count, VCopyMemFlags},
  mir::block::instr::AHQF,
};

#[macro_use]
mod macros;
pub mod flags;
pub mod loc;

#[repr(u8)]
pub enum IntTy {
  U64 = 0,
  U32 = 1,
  U16 = 2,
  U8 = 3,

  I64 = 4,
  I32 = 5,
  I16 = 6,
  I8 = 7,

  F64 = 8,
  F32 = 9,
}

impl AHQF for IntTy {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::U64 => "u64",
        Self::U32 => "u32",
        Self::U16 => "u16",
        Self::U8 => "u8",

        Self::I64 => "i64",
        Self::I32 => "i32",
        Self::I16 => "i16",
        Self::I8 => "i8",

        Self::F64 => "f64",
        Self::F32 => "f32",
      }
    )
  }
}

instloader! {
  /// Vectored copy operation
  /// This helps to copy between two locations
  Vcopy { count: Count, memflags: VCopyMemFlags } (src) -> (target)
  lower { INSTRUCTION_VCOPY } (|buf: &mut Vec<u8>, count, flags: &VCopyMemFlags, src: &LocSrc, target: &LocSrc| {
    let (counttag, count) = match count {
      // Abs = counttag =false
      &Count::Abs { abs } => (false, abs),
      Count::ReadFromR1 => (true, 0)
    };

    // Counttag + MemFlags
    buf.push(flags.lower(counttag));

    // LocSrc
    buf.push({
      let mut out = 0;

      out |= src.get_loc_bits() << 4;
      out |= target.get_loc_bits();

      out
    });

    // Count (u32)
    buf.extend(count.to_le_bytes());

    // Offset1 (i32)
    buf.extend((src.offset as i32).to_le_bytes());
    // Offset2 (i32)
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// The MOV instruction
  ///
  /// The offset of src and target are ignored
  Mov {  } (src) -> (target)
  lower { INSTRUCTION_MOV } (|buf: &mut Vec<u8>, src: &LocSrc, target: &LocSrc| {
    buf.push({
      let mut out = 0;

      out |= src.get_loc_bits() << 4;
      out |= target.get_loc_bits();

      out
    });
  })

  /// Returns the pointer to largepad in register r1
  LargepadPtr {  } () -> ()
  lower { INSTRUCTION_MOV } (|buf: &mut Vec<u8>| {
    buf.push({
      let mut out = 0;

      out |= 12 << 4;
      out |= 12;

      out
    });
  })

  /// Returns the pointer to globalRWData in register r1
  GlobalRWPtr {  } () -> ()
  lower { INSTRUCTION_MOV } (|buf: &mut Vec<u8>| {
    buf.push({
      let mut out = 0;

      out |= 13 << 4;
      out |= 13;

      out
    });
  })

  /// The vadd instruction
  Vadd { .IntTy, count: u32 } (src1, src2) -> (out)
  lower { INSTRUCTION_VADD } (|_buf, _typedata, _count, _src1, _src2, _out| {

  })
}
