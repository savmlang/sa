#[cfg(all(
  feature = "native",
  any(target_arch = "x86_64"),
  any(target_os = "windows", target_os = "linux")
))]
use crate::acaot::cinder::ACAoTCinder;
#[cfg(feature = "cranelift")]
use crate::acaot::native::cranelift::SaVMCranelift;
#[cfg(feature = "llvm")]
use crate::acaot::native::llvm_compiler::SaVMLLVMBuilder;
use crate::{
  BytecodeResolver, CacheData, CacheLevel, acaot::pickle::def::PickleInstruction,
  kvwrap::SaVMJumpWrapRef,
};

#[cfg(feature = "cranelift")]
pub mod cranelift;

#[cfg(feature = "llvm")]
pub mod llvm_compiler;

pub mod store;

pub use super::*;

pub trait NativeCompiler<const SENDBACK: bool> {
  fn compile(&mut self, pickle: &[PickleInstruction], jumps: SaVMJumpWrapRef) -> CacheData;
}

pub trait NativeCompilerBuilder<const SENDBACK: bool>: Send {
  fn cache(&self) -> CacheLevel;

  fn get(&self) -> Box<dyn NativeCompiler<SENDBACK>>;
}

#[derive(Debug, Clone, Copy)]
pub struct CompilerBuilder<const SENDBACK: bool>(
  fn() -> Box<dyn NativeCompiler<SENDBACK>>,
  CacheLevel,
);

impl<const T: bool> NativeCompilerBuilder<T> for CompilerBuilder<T> {
  fn cache(&self) -> CacheLevel {
    self.1
  }

  fn get(&self) -> Box<dyn NativeCompiler<T>> {
    (self.0)()
  }
}

pub fn testing_compiler_infra<const SENDBACK: bool, T: BytecodeResolver + Send + Sync + 'static>()
-> &'static [(&'static str, &'static dyn NativeCompilerBuilder<SENDBACK>)] {
  &[
    #[cfg(all(
      feature = "native",
      any(target_arch = "x86_64"),
      any(target_os = "windows", target_os = "linux")
    ))]
    (
      "Cinder - ACAoT JIT",
      &CompilerBuilder(ACAoTCinder::<T>::create, CacheLevel::ACAoTCinder),
    ),
    #[cfg(feature = "cranelift")]
    (
      "Crafter - Cranelift JIT",
      &CompilerBuilder(SaVMCranelift::create_abs8, CacheLevel::CraneliftCrafter),
    ),
    #[cfg(feature = "llvm")]
    (
      "Crater - LLVM JIT",
      &CompilerBuilder(SaVMLLVMBuilder::create_crater, CacheLevel::LLVMCrater),
    ),
  ]
}

pub fn testing_epitier_compilers<const SENDBACK: bool>()
-> &'static [(&'static str, &'static dyn NativeCompilerBuilder<SENDBACK>)] {
  &[
    #[cfg(feature = "cranelift")]
    (
      "Epicenter - Cranelift JIT",
      &CompilerBuilder(
        SaVMCranelift::create_rel_optimized,
        CacheLevel::CraneliftEpicenter,
      ),
    ),
    #[cfg(feature = "llvm")]
    (
      "Epitome - LLVM JIT",
      &CompilerBuilder(SaVMLLVMBuilder::create_epitome, CacheLevel::LLVMEpitome),
    ),
  ]
}

pub fn compiler_infra<const SENDBACK: bool, T: BytecodeResolver + Send + Sync + 'static>()
-> &'static [&'static dyn NativeCompilerBuilder<SENDBACK>] {
  &[
    #[cfg(all(
      feature = "native",
      any(target_arch = "x86_64"),
      any(target_os = "windows", target_os = "linux")
    ))]
    &CompilerBuilder(ACAoTCinder::<T>::create, CacheLevel::ACAoTCinder),
    #[cfg(feature = "cranelift")]
    &CompilerBuilder(SaVMCranelift::create_abs8, CacheLevel::CraneliftCrafter),
    #[cfg(feature = "llvm")]
    &CompilerBuilder(SaVMLLVMBuilder::create_crater, CacheLevel::LLVMCrater),
  ]
}

pub fn epitier_compiler<const SENDBACK: bool>() -> impl NativeCompilerBuilder<SENDBACK> {
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
