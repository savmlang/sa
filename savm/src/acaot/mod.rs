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

pub mod pickle;

#[cfg(feature = "native")]
pub mod native;
