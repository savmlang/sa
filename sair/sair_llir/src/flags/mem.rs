use std::fmt::{self, Formatter};
use crate::format::LLFormat;

/// Representation of count operand (either an absolute count or dynamic count from r1).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Count {
  Abs { abs: u32 },
  ReadFromR1,
}

impl Count {
  #[inline]
  pub const fn abs(abs: u32) -> Self {
    Self::Abs { abs }
  }

  #[inline]
  pub const fn from_r1() -> Self {
    Self::ReadFromR1
  }

  #[inline]
  pub const fn is_r1(&self) -> bool {
    matches!(self, Self::ReadFromR1)
  }

  #[inline]
  pub const fn raw_value(&self) -> u32 {
    match self {
      Self::Abs { abs } => *abs,
      Self::ReadFromR1 => 0,
    }
  }
}

impl LLFormat for Count {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Abs { abs } => write!(f, "{abs}"),
      Self::ReadFromR1 => write!(f, "@count:r1"),
    }
  }
}

impl fmt::Display for Count {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Memory alignment specifier.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AlignData {
  #[default]
  Unknown = 0,
  B16 = 1,
  B32 = 2,
  B64 = 3,
}

impl AlignData {
  pub const fn lower_vadd_style(self) -> u8 {
    match self {
      Self::Unknown => 0b00,
      Self::B16 => 0b01,
      Self::B32 => 0b10,
      Self::B64 => 0b11,
    }
  }

  pub const fn lower_vfma_mrx(self) -> u8 {
    match self {
      Self::B16 => 0,
      Self::B32 => 1,
      Self::B64 => 2,
      Self::Unknown => 0,
    }
  }
}

impl LLFormat for AlignData {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Unknown => "align(def)",
        Self::B16 => "align(16)",
        Self::B32 => "align(32)",
        Self::B64 => "align(64)",
      }
    )
  }
}

impl fmt::Display for AlignData {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Bitflags for vectored memory copy (`vcopy`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VCopyMemFlags {
  pub volatile: bool,
  pub nonoverlapping: bool,
  pub srcalign: AlignData,
  pub tgtalign: AlignData,
}

impl VCopyMemFlags {
  pub const fn new(volatile: bool, nonoverlapping: bool, srcalign: AlignData, tgtalign: AlignData) -> Self {
    Self {
      volatile,
      nonoverlapping,
      srcalign,
      tgtalign,
    }
  }

  pub const fn volatile_only() -> Self {
    Self {
      volatile: true,
      nonoverlapping: false,
      srcalign: AlignData::Unknown,
      tgtalign: AlignData::Unknown,
    }
  }

  pub const fn aligned(srcalign: AlignData, tgtalign: AlignData) -> Self {
    Self {
      volatile: true,
      nonoverlapping: false,
      srcalign,
      tgtalign,
    }
  }

  pub fn lower(self, counttag: bool) -> u8 {
    let mut out = 0u8;

    if counttag {
      out |= 1 << 7;
    }
    if self.volatile {
      out |= 1 << 5;
    }
    if self.nonoverlapping {
      out |= 1 << 4;
    }

    out |= self.srcalign.lower_vadd_style() << 2;
    out |= self.tgtalign.lower_vadd_style();

    out
  }
}

impl Default for VCopyMemFlags {
  fn default() -> Self {
    Self {
      volatile: true,
      nonoverlapping: false,
      srcalign: AlignData::Unknown,
      tgtalign: AlignData::Unknown,
    }
  }
}

impl LLFormat for VCopyMemFlags {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let mut printed = false;
    if self.volatile {
      write!(f, "volatile")?;
      printed = true;
    }
    if self.nonoverlapping {
      if printed {
        write!(f, " ")?;
      }
      write!(f, "nonoverlapping")?;
      printed = true;
    }
    if printed {
      write!(f, " ")?;
    }
    write!(f, "src:")?;
    self.srcalign.f(f)?;
    write!(f, " tgt:")?;
    self.tgtalign.f(f)
  }
}

impl fmt::Display for VCopyMemFlags {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Bitflags for FMA memory and alignment encoding across the 4 operand containers.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VFmaMemFlags {
  pub align_src1: AlignData,
  pub align_src2: AlignData,
  pub align_src3: AlignData,
  pub align_target: AlignData,
}

impl VFmaMemFlags {
  pub const fn none() -> Self {
    Self {
      align_src1: AlignData::Unknown,
      align_src2: AlignData::Unknown,
      align_src3: AlignData::Unknown,
      align_target: AlignData::Unknown,
    }
  }

  pub const fn all(align: AlignData) -> Self {
    Self {
      align_src1: align,
      align_src2: align,
      align_src3: align,
      align_target: align,
    }
  }

  /// Lowers alignment into 7-bit Mixed-Radix container (4 containers, 3 states each: 3^4 = 81).
  pub fn lower(&self) -> u8 {
    let a1 = self.align_src1.lower_vfma_mrx();
    let a2 = self.align_src2.lower_vfma_mrx();
    let a3 = self.align_src3.lower_vfma_mrx();
    let at = self.align_target.lower_vfma_mrx();

    let radix_val = a1 + (a2 * 3) + (a3 * 9) + (at * 27);
    radix_val & 0x7F
  }
}

impl LLFormat for VFmaMemFlags {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "s1:{} s2:{} s3:{} tgt:{}",
      self.align_src1, self.align_src2, self.align_src3, self.align_target
    )
  }
}

impl fmt::Display for VFmaMemFlags {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
