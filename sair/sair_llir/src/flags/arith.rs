use std::fmt::{self, Formatter};
use crate::format::LLFormat;

/// Bitflags for vectored integer addition and subtraction (`vadd`, `vsub`).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VAddFlags {
  pub carry: bool,
  pub saturation: bool,
  pub aligned: bool,
}

impl VAddFlags {
  pub const fn new(carry: bool, saturation: bool, aligned: bool) -> Self {
    Self {
      carry,
      saturation,
      aligned,
    }
  }

  pub const fn none() -> Self {
    Self {
      carry: false,
      saturation: false,
      aligned: false,
    }
  }

  pub const fn carry() -> Self {
    Self {
      carry: true,
      saturation: false,
      aligned: false,
    }
  }

  pub const fn saturating() -> Self {
    Self {
      carry: false,
      saturation: true,
      aligned: false,
    }
  }

  pub const fn aligned() -> Self {
    Self {
      carry: false,
      saturation: false,
      aligned: true,
    }
  }

  pub const fn lower(self) -> u8 {
    let mut out = 0u8;
    if self.carry {
      out |= 1 << 7;
    }
    if self.saturation {
      out |= 1 << 6;
    }
    if self.aligned {
      out |= 1 << 5;
    }
    out
  }
}

impl LLFormat for VAddFlags {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let mut printed = false;
    if self.carry {
      write!(f, "carry")?;
      printed = true;
    }
    if self.saturation {
      if printed {
        write!(f, " ")?;
      }
      write!(f, "sat")?;
      printed = true;
    }
    if self.aligned {
      if printed {
        write!(f, " ")?;
      }
      write!(f, "aligned")?;
    }
    Ok(())
  }
}

impl fmt::Display for VAddFlags {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Bitflags for vectored integer multiplication (`vmul`).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VMulFlags {
  pub high_bits: bool,
  pub wide: bool,
}

impl VMulFlags {
  pub const fn low() -> Self {
    Self {
      high_bits: false,
      wide: false,
    }
  }

  pub const fn high() -> Self {
    Self {
      high_bits: true,
      wide: false,
    }
  }

  pub const fn wide() -> Self {
    Self {
      high_bits: false,
      wide: true,
    }
  }

  pub const fn lower(self) -> u8 {
    let mut out = 0u8;
    if self.wide {
      out |= 1 << 1;
    }
    if self.high_bits {
      out |= 1;
    }
    out
  }
}

impl LLFormat for VMulFlags {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    if self.wide {
      write!(f, "wide")
    } else if self.high_bits {
      write!(f, "hi")
    } else {
      write!(f, "lo")
    }
  }
}

impl fmt::Display for VMulFlags {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Floating-point transcendental and rounding sub-operations (`vfop`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum VfopSubOp {
  #[default]
  Ceil = 0,
  Floor = 1,
  Trunc = 2,
  Nearest = 3,
  Sqrt = 4,
}

impl LLFormat for VfopSubOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Ceil => "ceil",
        Self::Floor => "floor",
        Self::Trunc => "trunc",
        Self::Nearest => "nearest",
        Self::Sqrt => "sqrt",
      }
    )
  }
}

impl fmt::Display for VfopSubOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Float to integer conversion direction (`vfcast`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum VFCastOp {
  #[default]
  FloatToInt = 0,
  IntToFloat = 1,
}

impl LLFormat for VFCastOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::FloatToInt => write!(f, "f2i"),
      Self::IntToFloat => write!(f, "i2f"),
    }
  }
}

impl fmt::Display for VFCastOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Minimum / Maximum operation selector (`vminimax`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum MinMaxOp {
  #[default]
  Min = 0,
  Max = 1,
}

impl LLFormat for MinMaxOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Min => write!(f, "min"),
      Self::Max => write!(f, "max"),
    }
  }
}

impl fmt::Display for MinMaxOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
