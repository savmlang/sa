use std::{fmt::Debug, marker::PhantomData};

use crate::{
  StringStore,
  mir::{Module, function::Function},
  saemit::machine::{TargetVM, sabi_map, v0::blockalloc::BlockAlloc},
};

mod blockalloc;

/// Targets the Instruction Set Architecture of `v0` of the SaVM Language
///
/// Triple : `savm64le-sa-v0`
pub struct IsaV0<T: StringStore> {
  _inner: PhantomData<T>,
}

impl<T: StringStore> IsaV0<T> {
  pub fn generate() -> Self {
    Self {
      _inner: PhantomData,
    }
  }
}

impl<T: StringStore> TargetVM for IsaV0<T> {
  type T = T;

  fn regalloc(&self, func: &Function<'_, Self::T>, module: &Module<'_, Self::T>) {
    let mut blockalloc = BlockAlloc::default();

    for block in &func.blocks {
      blockalloc.clear();
      let sabimap = sabi_map(&block.params, &func, &module);

      println!("{sabimap:?}");
    }
  }
}

impl<T: StringStore> Debug for IsaV0<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "savm64le-savm-sabi")
  }
}
