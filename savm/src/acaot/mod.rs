//! # SaVM ACAoT
//!
//! Adaptive Cached Ahead-of-Time (and Just-in-Time) Compiler
//!
//! ACAoT is a compiler & Optimizer collection aimed at empowering codebase
//! with static deterministic optimization.
//!
//! ACAoT has the [pickle] subsystem to convert Sa Bytecode to its own Pickle format
//! (which is used by chocolate interpreter)
//!
//! ACAoT Features compilers:
//! - Cranelift [cranelift]: Crafter, Epicenter
//! - LLVM [llvm_sys]: Crater, Epitome
//!
//! ACAoT Featured Copy-Patch JIT:
//! - ACAoT Cinder: Cinder
//!
//! # Meet ACAoT
//! The compiler toolchain backend for SaVM
//! Revolutionalize compilation, featuring IR Generation
//! - LLVM IR
//! - Cranelift IR
//!
//! and Bytecode Parsing
//! - Pickle IR
//!
//! Powering Chocolate, Crafter and Crater!
//!

use std::sync::Arc;

use serde::{Deserialize, Serialize};

#[cfg(all(
  feature = "native",
  any(target_arch = "x86_64", target_arch = "x86"),
  any(target_os = "windows", target_os = "linux")
))]
use crate::acaot::cinder::{StencilMap, StencilVec};

#[cfg(feature = "dag")]
pub mod acdag;

#[cfg(all(
  feature = "native",
  any(target_arch = "x86_64", target_arch = "x86"),
  any(target_os = "windows", target_os = "linux")
))]
pub mod cinder;

#[cfg(all(
  feature = "native",
  any(target_arch = "x86_64", target_arch = "x86"),
  any(target_os = "windows", target_os = "linux")
))]
pub type Stencils = Arc<[StencilVec<StencilMap>]>;

#[cfg(not(all(
  feature = "native",
  any(target_arch = "x86_64", target_arch = "x86"),
  any(target_os = "windows", target_os = "linux")
)))]
pub type Stencils = Arc<[()]>;

#[cfg(feature = "native")]
pub mod native;
pub mod pickle;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum LocSrc {
  VCopyNoAlias,
  VCopyOverlapping,

  VMScratchAction,

  VMSectionDispatch,
  VMLibcallSection,

  VMSpawn,

  NativeLibCall(u64),
  SaLibCall(u64),

  CLIRLibCall(ClirLC),
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ClirLC {
  Ceil32,
  Ceil64,
  Floor32,
  Floor64,
  Fma32,
  Fma64,
  Trunc32,
  Trunc64,
  Nearest32,
  Nearest64,
  Memcpy,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SigStore {
  VCopyCommon,
  VMScratch,

  SaVMLibcallDispatch,
  JITCall,

  SaFFICall,
  SaFFICallAsyncQ,
  SaFFICallAsyncO,

  VMSpawn,

  LibDefined(u64),
}

#[repr(C)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JITReloc {
  pub addend: i64,
  pub loc: LocSrc,
  pub offset: u32,
}
