use std::fmt::{self, Formatter};
use crate::format::LLFormat;

/// Atomic memory operations (`atomic`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum AtomicOp {
  #[default]
  Cas = 0,
  Load = 1,
  Rmw = 2,
  Store = 3,
}

impl LLFormat for AtomicOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Cas => "cas",
        Self::Load => "load",
        Self::Rmw => "rmw",
        Self::Store => "store",
      }
    )
  }
}

impl fmt::Display for AtomicOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Atomic memory orderings.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum AtomicOrdering {
  #[default]
  SeqCst = 0,
  Relaxed = 1,
  Acquire = 2,
  Release = 3,
  AcqRel = 4,
}

impl LLFormat for AtomicOrdering {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::SeqCst => "seqcst",
        Self::Relaxed => "relaxed",
        Self::Acquire => "acquire",
        Self::Release => "release",
        Self::AcqRel => "acq_rel",
      }
    )
  }
}

impl fmt::Display for AtomicOrdering {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Atomic Read-Modify-Write sub-operations.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum AtomicRmwOp {
  #[default]
  Add = 0,
  Sub = 1,
  And = 2,
  Or = 3,
  Xor = 4,
  Xchg = 5,
  Nand = 6,
  Max = 7,
  Min = 8,
}

impl LLFormat for AtomicRmwOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Add => "add",
        Self::Sub => "sub",
        Self::And => "and",
        Self::Or => "or",
        Self::Xor => "xor",
        Self::Xchg => "xchg",
        Self::Nand => "nand",
        Self::Max => "max",
        Self::Min => "min",
      }
    )
  }
}

impl fmt::Display for AtomicRmwOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Scratchpad management protocols (`scratch`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ScratchClass {
  #[default]
  Alloc = 0,
  Dealloc = 1,
  DeallocAligned = 2,
}

impl LLFormat for ScratchClass {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Alloc => "alloc",
        Self::Dealloc => "dealloc",
        Self::DeallocAligned => "dealloc_aligned",
      }
    )
  }
}

impl fmt::Display for ScratchClass {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
