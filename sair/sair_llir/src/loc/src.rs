use std::fmt::{self, Debug, Formatter};
use crate::format::LLFormat;
use super::vm::VMLoc;

/// Represents an operand location with an optional displacement offset.
///
/// # Offset Width & Phasing-Out Notice
/// - **`i32` Offsets (Legacy - Phasing Out Soon)**: Used in instructions such as `vcopy`, `vadd`, `vcmp`, `vfma`.
///   These 32-bit wide offsets are planned for phase-out in favor of normalized base pointer arithmetic and compact 8-bit offsets.
/// - **`i8` Offsets (Standard Compact)**: Used in `vsh`, `vcnt`, `vminimax`, and `atomic`.
/// - **`u8` Offsets**: Used in `reg` for immediate entity indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocSrc {
  pub loc: VMLoc,
  pub offset: i32,
}

impl LocSrc {
  #[inline]
  pub const fn new(loc: VMLoc, offset: i32) -> Self {
    Self { loc, offset }
  }

  #[inline]
  pub const fn reg(loc: VMLoc) -> Self {
    Self { loc, offset: 0 }
  }

  /// Sets a displacement offset.
  ///
  /// > [!NOTE]
  /// > 32-bit offsets are currently supported for legacy compatibility but will be phased out soon.
  #[inline]
  pub const fn with_offset(self, offset: i32) -> Self {
    Self {
      loc: self.loc,
      offset,
    }
  }

  /// Explicit alias highlighting that this uses an `i32` offset slated for phase-out.
  #[inline]
  pub const fn with_legacy_i32_offset(self, offset: i32) -> Self {
    Self {
      loc: self.loc,
      offset,
    }
  }

  /// Sets a compact 8-bit displacement offset (`i8`).
  #[inline]
  pub const fn with_offset_i8(self, offset: i8) -> Self {
    Self {
      loc: self.loc,
      offset: offset as i32,
    }
  }

  #[inline]
  pub const fn r1() -> Self {
    Self::reg(VMLoc::R1)
  }

  #[inline]
  pub const fn r2() -> Self {
    Self::reg(VMLoc::R2)
  }

  #[inline]
  pub const fn r3() -> Self {
    Self::reg(VMLoc::R3)
  }

  #[inline]
  pub const fn r4() -> Self {
    Self::reg(VMLoc::R4)
  }

  #[inline]
  pub const fn r5() -> Self {
    Self::reg(VMLoc::R5)
  }

  #[inline]
  pub const fn r6() -> Self {
    Self::reg(VMLoc::R6)
  }

  #[inline]
  pub const fn r7() -> Self {
    Self::reg(VMLoc::R7)
  }

  #[inline]
  pub const fn r8() -> Self {
    Self::reg(VMLoc::R8)
  }

  #[inline]
  pub const fn scratchpad() -> Self {
    Self::reg(VMLoc::Scratchpad)
  }

  #[inline]
  pub const fn largepad() -> Self {
    Self::reg(VMLoc::Largepad)
  }

  #[inline]
  pub const fn ptr_r2() -> Self {
    Self::reg(VMLoc::PtrFromR2)
  }

  #[inline]
  pub const fn ptr_r3() -> Self {
    Self::reg(VMLoc::PtrFromR3)
  }

  #[inline]
  pub const fn get_loc_bits(&self) -> u8 {
    self.loc as u8
  }

  #[inline]
  pub const fn offset_i32(&self) -> i32 {
    self.offset
  }

  #[inline]
  pub const fn offset_i8(&self) -> i8 {
    self.offset as i8
  }

  #[inline]
  pub const fn offset_u8(&self) -> u8 {
    self.offset as u8
  }

  #[inline]
  pub const fn is_i8_compatible(&self) -> bool {
    self.offset >= -128 && self.offset <= 127
  }
}

impl LLFormat for LocSrc {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.loc.f(f)?;
    if self.offset != 0 {
      write!(f, ".of({})", self.offset)?;
    }
    Ok(())
  }
}

impl fmt::Display for LocSrc {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
