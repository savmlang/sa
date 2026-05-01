use std::{collections::HashMap, marker::PhantomData, sync::Arc};

use sart::structures::serde;
use sart::structures::serde::{Deserialize, Serialize};

#[cfg(feature = "cranelift")]
use crate::acaot::native::cranelift::SaVMCranelift;
use crate::{CacheData, CacheLevel, acaot::pickle::def::PickleInstruction};

#[cfg(feature = "cranelift")]
pub mod cranelift;

pub use super::*;

pub trait NativeCompiler {
  fn prime(
    &mut self,
    pickle: Arc<[PickleInstruction]>,
    jumps: Arc<HashMap<u64, usize, ahash::RandomState>>,
  ) -> CacheData {
    self.compile(pickle.as_ref(), jumps.as_ref())
  }

  fn codegen_internal_trampoline(&mut self) -> Box<[u8]>;

  fn compile(
    &mut self,
    pickle: &[PickleInstruction],
    jumps: &HashMap<u64, usize, ahash::RandomState>,
  ) -> CacheData;
}

pub trait NativeCompilerBuilder: Send + Sync {
  fn cache(&self) -> CacheLevel;

  fn get(&self) -> Box<dyn NativeCompiler>;
}

#[derive(Debug, Clone, Copy)]
pub struct CompilerBuilder<T: NativeCompiler + Sized + Send + Sync>(
  PhantomData<T>,
  fn() -> Box<dyn NativeCompiler>,
  CacheLevel,
);

impl<T: NativeCompiler + Sized + Sync + Send> NativeCompilerBuilder for CompilerBuilder<T> {
  fn cache(&self) -> CacheLevel {
    self.2
  }

  fn get(&self) -> Box<dyn NativeCompiler> {
    (self.1)()
  }
}

pub fn compiler_infra() -> &'static [&'static dyn NativeCompilerBuilder] {
  &[
    #[cfg(feature = "cranelift")]
    &CompilerBuilder(
      PhantomData::<SaVMCranelift>,
      SaVMCranelift::create_abs8,
      CacheLevel::CraneliftCrafter,
    ),
  ]
}
