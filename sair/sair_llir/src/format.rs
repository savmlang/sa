use std::fmt::Formatter;

/// Formatter trait for LLIR representations, mirroring the AHQF trait from sair.
pub trait LLFormat {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
}

impl LLFormat for u64 {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl LLFormat for u32 {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl LLFormat for u16 {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl LLFormat for u8 {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl LLFormat for i32 {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl LLFormat for i8 {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl LLFormat for bool {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl LLFormat for &str {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}
