use std::{collections::HashMap, marker::PhantomData, sync::Arc};

#[cfg(feature = "cranelift")]
use crate::acaot::native::cranelift::SaVMCranelift;
use crate::acaot::pickle::def::PickleInstruction;

#[cfg(feature = "cranelift")]
pub mod cranelift;

pub trait NativeCompiler {
  fn create_abs8() -> Box<dyn NativeCompiler>
  where
    Self: Sized;

  fn create_rel() -> Option<Box<dyn NativeCompiler>>
  where
    Self: Sized;

  fn prime(
    &mut self,
    pickle: Arc<[PickleInstruction]>,
    jmps: Arc<HashMap<u64, usize, ahash::RandomState>>,
  );
}

pub trait NativeCompilerBuilder: Send + Sync {
  fn get_abs8(&self) -> Box<dyn NativeCompiler>;

  fn get_rel(&self) -> Option<Box<dyn NativeCompiler>>;
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
}

pub fn compiler_infra() -> &'static [&'static dyn NativeCompilerBuilder] {
  &[
    #[cfg(feature = "cranelift")]
    &CompilerBuilder(PhantomData::<SaVMCranelift>),
  ]
}
