use std::{fmt::Debug, marker::PhantomData};

use crate::{
  StringStore,
  mir::{Module, function::Function, regalloc::RegAllocResult},
  saemit::machine::TargetVM,
};

pub struct V0VM<T: StringStore>(pub PhantomData<T>);

impl<T: StringStore> V0VM<T> {
  pub fn new() -> Self {
    Self(PhantomData)
  }
}

impl<T: StringStore> Debug for V0VM<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "v0")
  }
}

impl<T: StringStore> TargetVM for V0VM<T> {
  type T = T;

  fn regalloc(
    &self,
    func: &Function<'_, Self::T>,
    module: &Module<'_, Self::T>,
  ) -> RegAllocResult {
    crate::mir::regalloc::allocate(func, module)
  }
}
