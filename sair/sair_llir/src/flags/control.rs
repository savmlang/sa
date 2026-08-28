use std::fmt::{self, Formatter};
use crate::format::LLFormat;

/// Jump-If intent (JZ = Jump If Zero, JNZ = Jump If Not Zero).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum JifIntent {
  #[default]
  JZ = 0,
  JNZ = 1,
}

impl LLFormat for JifIntent {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::JZ => write!(f, "jz"),
      Self::JNZ => write!(f, "jnz"),
    }
  }
}

impl fmt::Display for JifIntent {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Comparison predicates for `vcmp`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CmpOp {
  // Integral ops
  Eq = 0,
  Neq = 1,
  SLt = 2,
  ULt = 3,
  SLe = 4,
  ULe = 5,
  SGt = 6,
  UGt = 7,
  SGe = 8,
  UGe = 9,

  // Floating ops
  FOrd = 10,
  FUnord = 11,
  FEq = 12,
  FNeq = 13,
  FOrdNeq = 14,
  FUnordOrEq = 15,
  FLt = 16,
  FLe = 17,
  FGt = 18,
  FGe = 19,
  FUnordOrLt = 20,
  FUnordOrLe = 21,
  FUnordOrGt = 22,
  FUnordOrGe = 23,
}

impl CmpOp {
  pub const fn as_u8(self) -> u8 {
    self as u8
  }

  pub const fn is_float(self) -> bool {
    (self as u8) >= 10
  }
}

impl LLFormat for CmpOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Eq => "eq",
        Self::Neq => "neq",
        Self::SLt => "s_lt",
        Self::ULt => "u_lt",
        Self::SLe => "s_le",
        Self::ULe => "u_le",
        Self::SGt => "s_gt",
        Self::UGt => "u_gt",
        Self::SGe => "s_ge",
        Self::UGe => "u_ge",
        Self::FOrd => "f_ord",
        Self::FUnord => "f_unord",
        Self::FEq => "f_eq",
        Self::FNeq => "f_neq",
        Self::FOrdNeq => "f_ord_neq",
        Self::FUnordOrEq => "f_unord_eq",
        Self::FLt => "f_lt",
        Self::FLe => "f_le",
        Self::FGt => "f_gt",
        Self::FGe => "f_ge",
        Self::FUnordOrLt => "f_unord_lt",
        Self::FUnordOrLe => "f_unord_le",
        Self::FUnordOrGt => "f_unord_gt",
        Self::FUnordOrGe => "f_unord_ge",
      }
    )
  }
}

impl fmt::Display for CmpOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
