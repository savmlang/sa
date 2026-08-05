use std::fmt::Debug;

use crate::mir::block::instr::AHQF;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocSrc {
  pub loc: VMLoc,
  pub offset: i8,
}

impl LocSrc {
  pub fn get_loc_bits(&self) -> u8 {
    self.loc as u8
  }
}

impl AHQF for LocSrc {
  fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.loc.f(f)?;
    write!(f, ".of({})", self.offset)
  }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
}

impl AHQF for VMLoc {
  fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

        Self::Largepad => "*pad_l",
        Self::Scratchpad => "*pad_s",
        Self::PtrFromR2 => "*r2",
        Self::PtrFromR3 => "*r3",
      }
    )
  }
}
