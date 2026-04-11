use std::{hint::cold_path, process::abort, sync::Arc};

use crossbeam_channel::{Receiver, Sender};

use crate::{BytecodeResolver, CODE_CACHE, CacheData, CacheLevel, acaot::native::compiler_infra};

pub enum JITOut {
  JITData { moduleid: u64, jitdata: CacheData },
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

    let builder = compilers.get(compilerindex).unwrap_or_else(|| {
      use std::io::{self, Write};

      cold_path();
      let _ = writeln!(
        io::stderr(),
        "SaVM Critical Error: IMPOSSIBLE - Compiler found false"
      );
      let _ = io::stderr().flush();

      abort();
    });

    // Lets not hit it badly
    let bytecode = CODE_CACHE.get(&moduleid).unwrap_or_else(|| {
      match resolve.as_ref().get_cache(moduleid, CacheLevel::Pickle) {
        CacheData::Pickle { out, jumps } => (out, jumps),
        _ => {
          use std::io::{self, Write};

          cold_path();
          let _ = writeln!(io::stderr(), "SaVM Critical Error: flawed cache protocol.");
          let _ = io::stderr().flush();

          abort();
        }
      }
    });

    let jitdata;
    let (inst, jmp) = bytecode;
    {
      match resolve.as_ref().get_cache(moduleid, builder.abs_cache()) {
        CacheData::None => {
          let mut compiler = builder.get();

          jitdata = compiler.compile(inst.as_ref(), jmp.as_ref());
        }
        e => {
          jitdata = e;
        }
      }
    }

    _ = tx.send(JITOut::JITData { moduleid, jitdata });
  }
}
