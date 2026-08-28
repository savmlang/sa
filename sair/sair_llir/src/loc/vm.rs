use std::fmt::{self, Formatter};
use crate::format::LLFormat;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VMLoc {
  R1 = 0,
  R2 = 1,
  R3 = 2,
  R4 = 3,
  R5 = 4,
  R6 = 5,
  R7 = 6,
  R8 = 7,

  Scratchpad = 8,
  Largepad = 9,

  PtrFromR2 = 10,
  PtrFromR3 = 11,

  LargepadPtr = 12,
  GlobalRWPtr = 13,
}

impl VMLoc {
  pub const fn from_u8(val: u8) -> Option<Self> {
    match val {
      0 => Some(Self::R1),
      1 => Some(Self::R2),
      2 => Some(Self::R3),
      3 => Some(Self::R4),
      4 => Some(Self::R5),
      5 => Some(Self::R6),
      6 => Some(Self::R7),
      7 => Some(Self::R8),
      8 => Some(Self::Scratchpad),
      9 => Some(Self::Largepad),
      10 => Some(Self::PtrFromR2),
      11 => Some(Self::PtrFromR3),
      12 => Some(Self::LargepadPtr),
      13 => Some(Self::GlobalRWPtr),
      _ => None,
    }
  }

  pub const fn is_gpr(&self) -> bool {
    (*self as u8) <= 7
  }
}

impl LLFormat for VMLoc {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::R1 => "r1",
        Self::R2 => "r2",
        Self::R3 => "r3",
        Self::R4 => "r4",
        Self::R5 => "r5",
        Self::R6 => "r6",
        Self::R7 => "r7",
        Self::R8 => "r8",
        Self::Scratchpad => "*pad_s",
        Self::Largepad => "*pad_l",
        Self::PtrFromR2 => "*r2",
        Self::PtrFromR3 => "*r3",
        Self::LargepadPtr => "largepad_ptr",
        Self::GlobalRWPtr => "global_rw_ptr",
      }
    )
  }
}

impl fmt::Display for VMLoc {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
