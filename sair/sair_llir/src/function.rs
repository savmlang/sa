use std::fmt::{self, Formatter};
use crate::{block::LLBlock, format::LLFormat, instr::LLInstruction};

/// Represents a function compiled in SaVM LLIR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LLFunction {
  pub section_id: u64,
  pub name: String,
  pub blocks: Vec<LLBlock>,
}

impl LLFunction {
  pub fn new(section_id: u64, name: impl Into<String>) -> Self {
    Self {
      section_id,
      name: name.into(),
      blocks: Vec::new(),
    }
  }

  pub fn add_block(&mut self, block: LLBlock) -> &mut LLBlock {
    self.blocks.push(block);
    self.blocks.last_mut().unwrap()
  }

  pub fn push_instruction(&mut self, block_idx: usize, inst: LLInstruction) {
    if block_idx < self.blocks.len() {
      self.blocks[block_idx].push(inst);
    }
  }

  pub fn lower(&self, buf: &mut Vec<u8>) {
    for block in &self.blocks {
      block.lower(buf);
    }
  }

  pub fn to_bytes(&self) -> Vec<u8> {
    let mut buf = Vec::new();
    self.lower(&mut buf);
    buf
  }
}

impl LLFormat for LLFunction {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(f, "fn @{}(section: 0x{:x}) {{", self.name, self.section_id)?;
    for block in &self.blocks {
      block.f(f)?;
    }
    writeln!(f, "}}")
  }
}

impl fmt::Display for LLFunction {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
