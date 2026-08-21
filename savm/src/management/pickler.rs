use crate::{
  BytecodeResolver, CODE_CACHE, CacheData, SaVMJumps, SymbolMapTable, ThreadSafe,
  acaot::pickle::{PickleWorker, def::PickleInstruction},
};
use ahash::{HashMap, HashMapExt};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::sync::Arc;

#[cfg(feature = "libffi")]
use crate::FNCALL_DISPATCH;
#[cfg(feature = "libffi")]
use sart::structures::ffi::CallSig;

#[derive(Clone)]
pub enum ProcessResult {
  Pickle(u64, Arc<[PickleInstruction]>, SaVMJumps, Arc<[u64]>),
  #[cfg(feature = "libffi")]
  Native(u64, ThreadSafe<*const ()>, CallSig),
  None,
}

/// Preprocesses all bytecode sections in parallel, generates pickle instructions,
/// updates the code cache and initializes native dispatch table if libffi is enabled.
pub fn preprocess_sections<T: BytecodeResolver + Send + Sync + 'static>(resolve: &Arc<T>) {
  let last = resolve.as_ref().last_section_id();

  #[cfg(feature = "libffi")]
  let mut nativeptr = HashMap::new();

  (0..=last)
    .into_par_iter()
    .map(|id| match resolve.as_ref().resolve_data(id) {
      SymbolMapTable::MixedSizedBytecode { bytecode } => {
        match resolve.as_ref().get_best_cache(id) {
          // Pickle urgently!
          CacheData::None => {
            let mut worker = PickleWorker {
              bytecode,
              libcalls: Default::default(),
              out: vec![],
              jump: Default::default(),
            };
            worker.pass1();

            let out: Arc<[PickleInstruction]> = Arc::from(worker.out.into_boxed_slice());
            let jumps: Arc<[_]> = Arc::from(worker.jump);
            let libcalls = Arc::from(worker.libcalls);

            CODE_CACHE.insert(id, (out.clone(), jumps.clone()));
            ProcessResult::Pickle(id, out, jumps, libcalls)
          }
          _ => ProcessResult::None,
        }
      }
      #[cfg(feature = "libffi")]
      SymbolMapTable::NativePointer { fnptr, cdecl } => {
        ProcessResult::Native(id, ThreadSafe(fnptr), cdecl)
      }
    })
    .filter_map(|x| match x {
      ProcessResult::None => None,
      e => Some(e),
    })
    .collect::<Box<[_]>>()
    .into_iter()
    .for_each(|outdata| match outdata {
      ProcessResult::Pickle(section, cache, jumps, libcalls) => {
        resolve.as_ref().update_cache(
          section,
          CacheData::Pickle {
            out: cache,
            jumps,
            libcalls: Some(libcalls),
          },
        );
      }
      #[cfg(feature = "libffi")]
      ProcessResult::Native(module, fnptr, csig) => {
        _ = nativeptr.insert(module, (fnptr, csig));
      }
      _ => {}
    });

  #[cfg(feature = "libffi")]
  {
    _ = FNCALL_DISPATCH.get_or_init(|| nativeptr);
  }
}
