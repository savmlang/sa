use crate::{
  StringRef, StringStore,
  mir::{
    Module,
    block::Block,
    function::{
      builder::FunctionBuilder,
      ssa::{SSA, ValueId},
    },
    value::{ValueTypeRef, sig::SignatureRef},
  },
};

pub mod builder;
pub mod ir;
pub mod ssa;

pub struct Function<'a, T: StringStore> {
  pub store: &'a T,

  pub(crate) name: StringRef<'a, T>,
  sig: SignatureRef,

  pub(crate) ssa: Vec<SSA<'a, T>>,
  pub(crate) blocks: Vec<Block<'a, T>>,
}

impl<'a, T: StringStore> Function<'a, T> {
  pub(crate) fn new(store: &'a T, name: StringRef<'a, T>, sig: SignatureRef) -> Self {
    Self {
      store,
      name,
      sig,
      ssa: Vec::with_capacity(16),
      blocks: Vec::with_capacity(16),
    }
  }

  pub fn get_ssa(&self, ssa: ValueId) -> Option<&SSA<'a, T>> {
    self.ssa.get(ssa.0)
  }

  pub fn sig(&self) -> SignatureRef {
    self.sig
  }

  pub fn return_type(&self, module: &Module<'a, T>) -> Option<ValueTypeRef> {
    module.signature_data(self.sig).and_then(|s| s.returns)
  }

  pub fn builder<'b>(&'b mut self, module: &'b Module<'a, T>) -> FunctionBuilder<'a, 'b, T> {
    FunctionBuilder::new(self, module)
  }

  pub fn blocks(&self) -> &[Block<'a, T>] {
    &self.blocks
  }

  pub fn regalloc(&self, module: &Module<'a, T>) -> crate::mir::regalloc::RegAllocResult {
    crate::mir::regalloc::allocate(self, module)
  }
}

pub(crate) mod internal {
  use crate::{
    StringStore,
    mir::{Module, function::Function, value::ValueType},
  };
  use std::fmt::{Formatter, Result};

  impl<'a, T: StringStore> Function<'a, T> {
    pub(crate) fn print(&self, f: &mut Formatter, module: &Module<T>) -> Result {
      writeln!(
        f,
        "  fun {} (@sig:#{}):",
        self.store.resolve(self.name).as_ref(),
        self.sig.0 + 1
      )?;

      for (id, block) in self.blocks.iter().enumerate() {
        // Print the signature
        {
          write!(f, "    @sig (")?;

          for &param in &block.params {
            let typetag = self.get_ssa(param).unwrap().typetag;
            match module.type_data(typetag).unwrap() {
              ValueType::Base { base, .. } => {
                write!(f, " ")?;
                base.format(f)?
              }
              _ => write!(f, " @type:{}", typetag.0.get())?,
            }
          }

          writeln!(f, " )")?;
        }

        if block.v0 {
          writeln!(f, "    @entry")?;
        }

        // Write Preds
        if !block.v0 {
          if block.preds.is_empty() {
            writeln!(f, "    @orphan")?;
          } else {
            write!(f, "    @preds (")?;

            for &param in &block.preds {
              write!(f, " #{}", param.0)?
            }

            writeln!(f, " )")?;
          }
        }
        // Write Succs
        if !block.succ.is_empty() {
          write!(f, "    @succs (")?;

          for &param in &block.succ {
            write!(f, " #{}", param.0)?
          }

          writeln!(f, " )")?;
        }

        write!(f, "    block #{}", id)?;

        if !block.params.is_empty() {
          write!(f, "(")?;

          for &param in &block.params {
            write!(f, " v{}", param.0)?;
          }

          write!(f, " )")?;
        }

        writeln!(f, ":")?;

        // Print Instructors
        for inst in &block.instr {
          write!(f, "      ")?;
          inst.format(f)?;
          writeln!(f)?;
        }

        writeln!(f)?;
      }

      Ok(())
    }
  }
}
