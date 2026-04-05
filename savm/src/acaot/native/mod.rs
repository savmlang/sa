use std::{collections::HashMap, marker::PhantomData, sync::Arc};

#[cfg(feature = "cranelift")]
use crate::acaot::native::cranelift::SaVMCranelift;
use crate::{CacheData, CacheLevel, acaot::pickle::def::PickleInstruction};

#[cfg(feature = "cranelift")]
pub mod cranelift;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum CompilerId {
  /// It means the compiler used was [cranelift-codegen]
  ///
  /// It is SaVM ``
  Cranelift,
  LLVM,
}

pub trait NativeCompiler {
  fn create_abs8() -> Box<dyn NativeCompiler>
  where
    Self: Sized;

  fn create_rel() -> Option<Box<dyn NativeCompiler>>
  where
    Self: Sized;

  fn compiler_id(&self) -> CompilerId;

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
  fn get_abs8(&self) -> Box<dyn NativeCompiler>;

  fn get_rel(&self) -> Option<Box<dyn NativeCompiler>>;

  fn abs_cache(&self) -> CacheLevel;
  fn rel_cache(&self) -> CacheLevel;
}

#[derive(Debug, Clone, Copy)]
pub struct CompilerBuilder<T: NativeCompiler + Sized + Send + Sync>(PhantomData<T>);

impl<T: NativeCompiler + Sized + Sync + Send> NativeCompilerBuilder for CompilerBuilder<T> {
  fn get_abs8(&self) -> Box<dyn NativeCompiler> {
    T::create_abs8()
  }

  fn get_rel(&self) -> Option<Box<dyn NativeCompiler>> {
    T::create_rel()
  }

  fn abs_cache(&self) -> CacheLevel {
    CacheLevel::CraneliftAbs8
  }

  fn rel_cache(&self) -> CacheLevel {
    CacheLevel::CraneliftRel
  }
}

pub fn compiler_infra() -> &'static [&'static dyn NativeCompilerBuilder] {
  &[
    #[cfg(feature = "cranelift")]
    &CompilerBuilder(PhantomData::<SaVMCranelift>),
  ]
}
