use std::fmt::Formatter;
use crate::{
  llir::instr::loc::VMLoc,
  mir::block::instr::{AHQF, Internal, Register},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocSrc {
  pub reg: VMLoc,
  pub offset: i8,

  /// What's the canonical width
  pub width: usize,
  /// Upto how much does this span
  pub count: u8,
}

impl LocSrc {
  pub const fn new(reg: VMLoc, offset: i8, width: usize, count: u8) -> Self {
    Self {
      reg,
      offset,
      width,
      count,
    }
  }

  pub const fn reg_scalar(reg: VMLoc, offset: i8, width: usize) -> Self {
    Self {
      reg,
      offset,
      width,
      count: 1,
    }
  }

  pub const fn r1(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R1, offset, width)
  }
  pub const fn r2(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R2, offset, width)
  }
  pub const fn r3(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R3, offset, width)
  }
  pub const fn r4(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R4, offset, width)
  }
  pub const fn r5(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R5, offset, width)
  }
  pub const fn r6(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R6, offset, width)
  }
  pub const fn r7(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R7, offset, width)
  }
  pub const fn r8(offset: i8, width: usize) -> Self {
    Self::reg_scalar(VMLoc::R8, offset, width)
  }

  pub const fn scratchpad(offset: i8, width: usize, count: u8) -> Self {
    Self {
      reg: VMLoc::Scratchpad,
      offset,
      width,
      count,
    }
  }

  pub const fn largepad(offset: i8, width: usize, count: u8) -> Self {
    Self {
      reg: VMLoc::Largepad,
      offset,
      width,
      count,
    }
  }

  pub fn byte_offset(&self) -> usize {
    (self.offset as usize) * self.width
  }
}

impl Internal for LocSrc {}

impl Register for LocSrc {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    self.reg.f(f)?;
    if self.offset != 0 {
      write!(f, ".of({})", self.offset)?;
    }
    Ok(())
  }
}

impl AHQF for LocSrc {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    Register::f(self, f)
  }
}

