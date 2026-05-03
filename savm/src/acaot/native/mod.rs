use std::{collections::HashMap, sync::Arc};

use sart::structures::serde;
use sart::structures::serde::{Deserialize, Serialize};

#[cfg(feature = "cranelift")]
use crate::acaot::native::cranelift::SaVMCranelift;
#[cfg(feature = "llvm")]
use crate::acaot::native::llvm_compiler::{SaVMLLVM, SaVMLLVMBuilder};
use crate::{CacheData, CacheLevel, acaot::pickle::def::PickleInstruction};

#[cfg(feature = "cranelift")]
pub mod cranelift;

#[cfg(feature = "llvm")]
pub mod llvm_compiler;

pub use super::*;

pub trait NativeCompiler {
  fn compile(
    &mut self,
    pickle: &[PickleInstruction],
    jumps: &HashMap<u64, usize, ahash::RandomState>,
  ) -> CacheData;
}

pub trait NativeCompilerBuilder: Send {
  fn cache(&self) -> CacheLevel;

  fn get(&self) -> Box<dyn NativeCompiler>;
}

#[derive(Debug, Clone, Copy)]
pub struct CompilerBuilder(fn() -> Box<dyn NativeCompiler>, CacheLevel);

impl NativeCompilerBuilder for CompilerBuilder {
  fn cache(&self) -> CacheLevel {
    self.1
  }

  fn get(&self) -> Box<dyn NativeCompiler> {
    (self.0)()
  }
}

pub fn compiler_infra() -> &'static [&'static dyn NativeCompilerBuilder] {
  &[
    #[cfg(all(feature = "llvm", not(feature = "cranelift")))]
    &CompilerBuilder(SaVMLLVMBuilder::create_cinder, CacheLevel::LLVMCinder),
    #[cfg(feature = "cranelift")]
    &CompilerBuilder(SaVMCranelift::create_abs8, CacheLevel::CraneliftCrafter),
    #[cfg(feature = "llvm")]
    &CompilerBuilder(SaVMLLVMBuilder::create_crater, CacheLevel::LLVMCrater),
  ]
}

pub fn epitier_compiler() -> impl NativeCompilerBuilder {
  #[cfg(feature = "llvm")]
  return CompilerBuilder(SaVMLLVMBuilder::create_epitome, CacheLevel::LLVMEpitome);

  #[cfg(all(feature = "cranelift", not(feature = "llvm")))]
  return CompilerBuilder(
    SaVMCranelift::create_rel_optimized,
    CacheLevel::CraneliftEpicenter,
  );

  #[cfg(not(any(feature = "cranelift", feature = "llvm")))]
  compile_error!("`native` feature must not be enabled without any accompanying backends");
}
