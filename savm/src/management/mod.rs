use crate::BytecodeResolver;
use std::sync::Arc;

#[cfg(all(
  feature = "native",
  any(target_arch = "x86_64"),
  any(target_os = "windows", target_os = "linux")
))]
pub mod cinder;

#[cfg(feature = "native")]
pub mod compiler_thread;
#[cfg(feature = "native")]
pub mod jit;
#[cfg(feature = "native")]
pub mod jitmem;
pub mod pickler;
#[cfg(feature = "native")]
pub mod polyfills;
#[cfg(feature = "native")]
pub mod processjit;
#[cfg(feature = "native")]
pub mod schedule;

#[cfg(feature = "native")]
pub use jit::run_jit_orchestrator;
pub use pickler::{ProcessResult, preprocess_sections};
#[cfg(feature = "native")]
pub use processjit::process_jit;
#[cfg(feature = "native")]
pub use schedule::schedule;

/// Main management orchestrator for SaVM runtime.
///
/// 1. Runs bytecode preprocessing & pickle transformations across all sections in parallel.
/// 2. (When `native` is enabled) Spawns and coordinates background JIT workers and links native code.
pub fn management_main<T: BytecodeResolver + Send + Sync + 'static>(resolve: Arc<T>) {
  preprocess_sections(&resolve);

  #[cfg(feature = "native")]
  {
    run_jit_orchestrator(resolve);
  }
}
