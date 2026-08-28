use std::fmt::{self, Formatter};
use crate::{format::LLFormat, function::LLFunction};

/// Represents an entire compiled module containing functions and bytecode definitions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LLModule {
  pub name: String,
  pub functions: Vec<LLFunction>,
  pub entry_section: Option<u64>,
}

impl LLModule {
  pub fn new(name: impl Into<String>) -> Self {
    Self {
      name: name.into(),
      functions: Vec::new(),
      entry_section: None,
    }
  }

  pub fn set_entry(&mut self, section_id: u64) {
    self.entry_section = Some(section_id);
  }

  pub fn add_function(&mut self, func: LLFunction) -> &mut LLFunction {
    self.functions.push(func);
    self.functions.last_mut().unwrap()
  }

  pub fn find_function(&self, name: &str) -> Option<&LLFunction> {
    self.functions.iter().find(|f| f.name == name)
  }

  pub fn find_function_by_section(&self, section_id: u64) -> Option<&LLFunction> {
    self.functions.iter().find(|f| f.section_id == section_id)
  }

  /// Lowers all functions in the module to raw SaVM bytecode bytes.
  pub fn lower(&self, buf: &mut Vec<u8>) {
    for func in &self.functions {
      func.lower(buf);
    }
  }

  /// Lowers the module into a newly allocated `Vec<u8>`.
  pub fn to_bytes(&self) -> Vec<u8> {
    let mut buf = Vec::new();
    self.lower(&mut buf);
    buf
  }
}

impl LLFormat for LLModule {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(f, "; Module: {}", self.name)?;
    if let Some(entry) = self.entry_section {
      writeln!(f, "; Entry Section: 0x{:x}", entry)?;
    }
    writeln!(f)?;
    for func in &self.functions {
      func.f(f)?;
      writeln!(f)?;
    }
    Ok(())
  }
}

impl fmt::Display for LLModule {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
