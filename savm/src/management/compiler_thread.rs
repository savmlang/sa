use std::{hint::cold_path, process::abort, sync::Arc};

use crossbeam_channel::{Receiver, Sender};

use crate::{
  BytecodeResolver, CODE_CACHE, CacheData, CacheLevel, acaot::native::compiler_infra,
  kvwrap::SaVMJumpWrapRef,
};

pub enum JITOut {
  JITData { moduleid: u64, jitdata: CacheData },
  Stopped,
}

pub fn compiler<const SENDBACK: bool, E: BytecodeResolver + Send + Sync + 'static>(
  resolve: Arc<E>,
  rx: Receiver<(u64, usize, bool)>,
  tx: Sender<JITOut>,
) {
  let compilers = compiler_infra::<SENDBACK, E>();
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
        CacheData::Pickle { out, jumps, .. } => (out, jumps),
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
      let compile = || {
        let mut compiler = builder.get();

        compiler.compile(inst.as_ref(), SaVMJumpWrapRef(&jmp))
      };

      match resolve.as_ref().get_cache(moduleid, builder.cache()) {
        CacheData::None => {
          jitdata = compile();
        }
        CacheData::JITCache { level, .. } if matches!(level, CacheLevel::ACAoTCinder) => {
          jitdata = compile();
        }
        e => {
          jitdata = e;
        }
      }
    }

    _ = tx.send(JITOut::JITData { moduleid, jitdata });
  }
}
