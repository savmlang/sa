use std::fmt::{self, Formatter};
use crate::format::LLFormat;

/// Bitwise operations supported by `vbit`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum BitOp {
  #[default]
  And = 0,
  Or = 1,
  Xor = 2,
  Not = 3,
  OrNot = 4,
  AndNot = 5,
  XorNot = 6,
  BitRev = 7,
  BSwap = 8,
}

impl LLFormat for BitOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::And => "and",
        Self::Or => "or",
        Self::Xor => "xor",
        Self::Not => "not",
        Self::OrNot => "or_not",
        Self::AndNot => "and_not",
        Self::XorNot => "xor_not",
        Self::BitRev => "bitrev",
        Self::BSwap => "bswap",
      }
    )
  }
}

impl fmt::Display for BitOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Bit rotation direction for `vrot`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum RotOp {
  #[default]
  RotL = 0,
  RotR = 1,
}

impl LLFormat for RotOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::RotL => write!(f, "rotl"),
      Self::RotR => write!(f, "rotr"),
    }
  }
}

impl fmt::Display for RotOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Bit shift direction for `vsh`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ShiftOp {
  #[default]
  Shl = 0,
  Shr = 1,
}

impl LLFormat for ShiftOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Shl => write!(f, "shl"),
      Self::Shr => write!(f, "shr"),
    }
  }
}

impl fmt::Display for ShiftOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Count operations supported by `vcnt`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum CountOp {
  #[default]
  Popcnt = 0,
  Clz = 1,
  Cls = 2,
  Ctz = 3,
}

impl LLFormat for CountOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Popcnt => "popcnt",
        Self::Clz => "clz",
        Self::Cls => "cls",
        Self::Ctz => "ctz",
      }
    )
  }
}

impl fmt::Display for CountOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
