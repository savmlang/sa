use std::fmt::{self, Formatter};
use crate::format::LLFormat;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl IntTy {
  pub const fn byte_width(&self) -> usize {
    match self {
      Self::U64 | Self::I64 | Self::F64 => 8,
      Self::U32 | Self::I32 | Self::F32 => 4,
      Self::U16 | Self::I16 => 2,
      Self::U8 | Self::I8 => 1,
    }
  }

  pub const fn bit_width(&self) -> usize {
    self.byte_width() * 8
  }

  pub const fn is_float(&self) -> bool {
    matches!(self, Self::F64 | Self::F32)
  }

  pub const fn is_signed(&self) -> bool {
    matches!(self, Self::I64 | Self::I32 | Self::I16 | Self::I8)
  }

  pub const fn is_unsigned(&self) -> bool {
    matches!(self, Self::U64 | Self::U32 | Self::U16 | Self::U8)
  }
}

impl LLFormat for IntTy {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl fmt::Display for IntTy {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum FloatTy {
  #[default]
  F64 = 0,
  F32 = 1,
}

impl FloatTy {
  pub const fn byte_width(&self) -> usize {
    match self {
      Self::F64 => 8,
      Self::F32 => 4,
    }
  }

  pub const fn bit_width(&self) -> usize {
    self.byte_width() * 8
  }
}

impl LLFormat for FloatTy {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::F64 => "f64",
        Self::F32 => "f32",
      }
    )
  }
}

impl fmt::Display for FloatTy {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum Width {
  #[default]
  W64 = 0,
  W32 = 1,
  W16 = 2,
  W8 = 3,
}

impl Width {
  pub const fn byte_width(&self) -> usize {
    match self {
      Self::W64 => 8,
      Self::W32 => 4,
      Self::W16 => 2,
      Self::W8 => 1,
    }
  }

  pub const fn bit_width(&self) -> usize {
    self.byte_width() * 8
  }
}

impl LLFormat for Width {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::W64 => "w64",
        Self::W32 => "w32",
        Self::W16 => "w16",
        Self::W8 => "w8",
      }
    )
  }
}

impl fmt::Display for Width {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
