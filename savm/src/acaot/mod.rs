//! # SaVM ACAoT
//!
//! Adaptive Cached Ahead-of-Time (and Just-in-Time) Compiler
//!
//! ACAoT is a compiler & Optimizer collection aimed at empowering codebase
//! with static deterministic optimization.
//!
//! ACAoT has the [pickle] subsystem to convert Sa Bytecode to its own Pickle format
//! (which is used by chocolate interpreter) and compilers like Crafter [cranelift]
//! and Crater [llvm-sys] converts that to reality for SaVMJIT Tiers like Crafter, Crater, Epicenter, Epitome.
//!
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
//! ## Meet the project
//! Designed for years, written in days!

use serde::{Deserialize, Serialize};

#[cfg(feature = "dag")]
pub mod acdag;
#[cfg(feature = "native")]
pub mod native;
pub mod pickle;

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
}

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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JITReloc {
  pub addend: i64,
  pub loc: LocSrc,
  pub offset: u32,
}
