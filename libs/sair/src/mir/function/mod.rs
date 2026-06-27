use crate::{
  StringRef, StringStore,
  mir::{
    Module,
    block::Block,
    function::{
      builder::FunctionBuilder,
      ssa::{SSA, ValueId},
    },
    value::sig::SignatureRef,
  },
};

pub mod builder;
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

  pub fn builder<'b>(&'b mut self, module: &'b Module<'a, T>) -> FunctionBuilder<'a, 'b, T> {
    FunctionBuilder::new(self, module)
  }
}

pub(crate) mod internal {
  use crate::{
    StringStore,
    mir::{Module, function::Function},
  };
  use std::fmt::{Formatter, Result};

  impl<'a, T: StringStore> Function<'a, T> {
    pub(crate) fn print(&self, f: &mut Formatter, _module: &Module<T>) -> Result {
      writeln!(
        f,
        "  fun {} (@sig:#{}):",
        self.store.resolve(self.name).as_ref(),
        self.sig.0 + 1
      )?;

      for (id, block) in self.blocks.iter().enumerate() {
        write!(f, "    block #{}", id)?;

        if !block.params.is_empty() {
          write!(f, "(")?;

          for &param in &block.params {
            write!(f, " v{}", param.0)?;
          }

          write!(f, " )")?;
        }

        writeln!(f, ":")?;
      }

      Ok(())
    }
  }
}
