use std::sync::Arc;

use crossbeam_channel::{Receiver, Sender};

use crate::{
  BytecodeResolver, CODE_CACHE, CacheData, CacheLevel,
  acaot::native::{NativeCompilerBuilder, compiler_infra},
};

pub enum JITOut {
  JITData { abs8: CacheData, rel: CacheData },
  Stopped,
}

pub fn compiler(
  resolve: Arc<dyn BytecodeResolver + Send + Sync + 'static>,
  rx: Receiver<(u64, usize, bool)>,
  tx: Sender<JITOut>,
) {
  let compilers = compiler_infra();
  while let Ok((moduleid, compilerindex, stop)) = rx.recv() {
    if stop {
      _ = tx.send(JITOut::Stopped);
      break;
    }

    let builder = compilers
      .get(compilerindex)
      .expect("Could not fetch compiler noted by the index, please note that it is indeed correct");

    let bytecode = CODE_CACHE.entry(moduleid).or_insert_with(|| {
      match resolve.as_ref().get_cache(moduleid, CacheLevel::Pickle) {
        CacheData::Pickle { out, jumps } => (out, jumps),
        _ => panic!("SaVM Critical Error : SaVM Runtime did not assume cache protocol."),
      }
    });

    let bytecode = bytecode.value();

    let (inst, jmp) = bytecode.clone();
    {
      let mut c_abs8 = builder.get_abs8();

      c_abs8.prime(inst, jmp);
    }

    {
      if let Some(mut c_rel) = builder.get_rel() {
        let (inst, jmp) = bytecode.clone();
        c_rel.prime(inst, jmp);
      }
    }
  }
}
