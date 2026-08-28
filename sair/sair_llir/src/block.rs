use std::fmt::{self, Formatter};
use crate::{format::LLFormat, instr::LLInstruction};

/// The JIT-Up / hot loop checkpoint bit (63rd bit).
pub const JIT_HOT_BIT: u64 = 1 << 63;

/// A basic block in the Low-Level Intermediate Representation.
/// Contains an auto-assigned block identifier, optional name, hot loop metadata, and instructions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LLBlock {
  pub id: u64,
  pub name: Option<String>,
  pub hot: bool,
  pub instructions: Vec<LLInstruction>,
}

impl LLBlock {
  pub fn new(id: u64) -> Self {
    Self {
      id,
      name: None,
      hot: false,
      instructions: Vec::new(),
    }
  }

  pub fn with_name(id: u64, name: impl Into<String>, hot: bool) -> Self {
    Self {
      id,
      name: Some(name.into()),
      hot,
      instructions: Vec::new(),
    }
  }

  /// Computes the 64-bit runtime marker ID.
  /// If `hot` (or `jit_jmp`), sets the 63rd bit (sign bit < 0) for JIT check / OSR triggers.
  pub fn marker_id(&self) -> u64 {
    if self.id == 0 {
      0
    } else if self.hot {
      self.id | JIT_HOT_BIT
    } else {
      self.id
    }
  }

  pub fn push(&mut self, inst: LLInstruction) {
    self.instructions.push(inst);
  }

  pub fn len(&self) -> usize {
    self.instructions.len()
  }

  pub fn is_empty(&self) -> bool {
    self.instructions.is_empty()
  }

  /// Lowers all instructions in the block sequentially to raw SaVM bytecode bytes.
  pub fn lower(&self, buf: &mut Vec<u8>) {
    for inst in &self.instructions {
      inst.lower(buf);
    }
  }

  pub fn to_bytes(&self) -> Vec<u8> {
    let mut buf = Vec::new();
    self.lower(&mut buf);
    buf
  }
}

impl LLFormat for LLBlock {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let hot_tag = if self.hot { " [hot/jit]" } else { "" };
    if let Some(ref name) = self.name {
      writeln!(f, "block_{} ({}{hot_tag}):", self.id, name)?;
    } else {
      writeln!(f, "block_{}{hot_tag}:", self.id)?;
    }

    for inst in &self.instructions {
      write!(f, "    ")?;
      inst.f(f)?;
      writeln!(f)?;
    }
    Ok(())
  }
}

impl fmt::Display for LLBlock {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
