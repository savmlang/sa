use crate::{
  BytecodeResolver, CODE_CACHE, CacheData, SymbolMapTable,
  acaot::pickle::{PickleWorker, def::PickleInstruction},
};
use evmap::handles::WriteHandle;
use std::sync::Arc;

use rayon::iter::{IntoParallelIterator, ParallelIterator};

pub fn management_main<T: BytecodeResolver + Send + Sync + 'static>(
  _: WriteHandle<u64, usize>,
  resolve: Arc<T>,
) {
  (0..=resolve.as_ref().last_section_id())
    .into_par_iter()
    .map(|id| match resolve.as_ref().resolve_data(id) {
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

            let out: Arc<[PickleInstruction]> = Arc::from(worker.out.into_boxed_slice());
            let jumps = Arc::new(worker.jump);

            CODE_CACHE.insert(id, (out.clone(), jumps.clone()));
            Some((id, out, jumps))
          }
          _ => None,
        }
      }
      SymbolMapTable::NativePointer { .. } => None,
    })
    .filter_map(|x| x)
    .collect::<Box<[_]>>()
    .into_iter()
    .for_each(|(section, cache, jumps)| {
      resolve
        .as_ref()
        .update_cache(section, CacheData::Pickle { out: cache, jumps });
    });
}
