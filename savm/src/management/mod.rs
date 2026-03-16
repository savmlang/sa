use crate::{BytecodeResolver, CacheData, SymbolMapTable, acaot::pickle::PickleWorker};
use evmap::handles::WriteHandle;
use std::sync::Arc;

use rayon::iter::{IntoParallelIterator, ParallelIterator};

pub fn management_main<T: BytecodeResolver + Send + Sync + 'static>(
  writer: WriteHandle<u64, usize>,
  resolve: Arc<T>,
) {
  (0..=resolve.as_ref().last_section_id())
    .into_par_iter()
    .for_each(|id| match resolve.as_ref().resolve_data(id) {
      SymbolMapTable::MixedSizedBytecode { bytecode } => {
        match resolve.as_ref().get_best_cache(id) {
          // Pickle urgently!
          CacheData::None => {
            let mut worker = PickleWorker {
              bytecode,
              out: vec![],
              jump: Default::default(),
            };
            worker.pass1();

            let out = worker.out.into_boxed_slice();
            resolve.as_ref().update_cache(id, CacheData::Pickle { out });
          }
          _ => {}
        }
      }
      SymbolMapTable::NativePointer { .. } => {}
    });
}
