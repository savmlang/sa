use crate::{
  BytecodeResolver, CODE_CACHE, CacheData, SymbolMapTable,
  acaot::pickle::{PickleWorker, def::PickleInstruction},
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::sync::Arc;

#[cfg(feature = "native")]
use crate::{
  SafeSwappableCodeStore,
  acaot::{
    LocSrc,
    pickle::reader::corevm::{
      jitcall_scratch_ffi, jitcall_vcopy_noalias, jitcall_vcopy_overlapping,
    },
  },
  executor::corevm_libcall,
  management::jitmem::calculate_relocation_abs,
};
#[cfg(feature = "native")]
use evmap::handles::WriteHandle;
#[cfg(feature = "native")]
use sart::code::SwappableCodeStore;
#[cfg(feature = "native")]
use std::{mem::transmute, process::abort};

#[cfg(feature = "native")]
pub mod compiler_thread;

#[cfg(feature = "native")]
pub mod jitmem;

#[cfg(feature = "native")]
use jitmem::JITMemoryManager;

enum ProcessResult {
  Pickle(
    u64,
    Arc<[PickleInstruction]>,
    Arc<ahash::HashMap<u64, usize>>,
  ),
  Native(u64),
  None,
}

#[allow(unused_macros)]
macro_rules! schedule {
  ($tx_critical:ident, $tx_fastlane:ident, $tx_public:ident, $critical:ident, $important:ident, $others:ident, $compiler_fastlane:ident, $compiler_public:ident, $compilers:ident, $important_s:ident, $others_iter:ident) => {
    /*
      Schedule more work thu each sector
    */
    'a: while let Some(x) = $critical.peek() {
      if $tx_critical
        .try_send((**x, $compilers.len() - 1, false))
        .is_ok()
      {
        _ = $critical.next();
      } else {
        break 'a;
      }
    }

    'a: while let Some(x) = $important.peek() {
      if $tx_fastlane
        .try_send((**x, $compiler_fastlane, false))
        .is_ok()
      {
        _ = $important.next();
      } else {
        break 'a;
      }
    }

    'a: while let Some(x) = $others.peek() {
      if $tx_public.try_send((*x, $compiler_public, false)).is_ok() {
        _ = $others.next();
      } else {
        break 'a;
      }
    }

    /*
      Sanity Checking
    */

    // If the list is empty
    // Else - end
    if $critical.peek().is_none() {
      // Send shutdown signal to the thread
      _ = $tx_critical.try_send((0, 0, true));
    }

    // If the list is empty & compiler can be increased, increment
    // Else - end fastlane
    if $important.peek().is_none() {
      if $compiler_fastlane + 1 == $compilers.len() {
        $compiler_fastlane += 1;
        $important = $important_s.into_iter().peekable();
      } else {
        // Send shutdown signal to the fastlane thread
        _ = $tx_fastlane.try_send((0, 0, true));
      }
    }

    if $others.peek().is_none() {
      if $compiler_public + 1 == $compilers.len() {
        $compiler_public += 1;
        $others = $others_iter();
      } else {
        // Send shutdown signal
        _ = $tx_public.try_send((0, 0, true));
      }
    }
  };
}

pub fn management_main(
  #[cfg(feature = "native")] mut evmap: WriteHandle<u64, SafeSwappableCodeStore>,
  resolve: Arc<dyn BytecodeResolver + Send + Sync + 'static>,
) {
  let last = resolve.as_ref().last_section_id();

  let mut _nativeptr = vec![];
  (0..=last)
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
            ProcessResult::Pickle(id, out, jumps)
          }
          _ => ProcessResult::None,
        }
      }
      SymbolMapTable::NativePointer { .. } => ProcessResult::Native(id),
    })
    .filter_map(|x| match x {
      ProcessResult::None => None,
      e => Some(e),
    })
    .collect::<Box<[_]>>()
    .into_iter()
    .for_each(|outdata| match outdata {
      ProcessResult::Pickle(section, cache, jumps) => {
        resolve
          .as_ref()
          .update_cache(section, CacheData::Pickle { out: cache, jumps });
      }
      ProcessResult::Native(m) => _nativeptr.push(m),
      _ => {}
    });

  #[cfg(feature = "native")]
  {
    let nativeptr = _nativeptr.into_boxed_slice();

    use std::{collections::HashMap, time::Duration};

    use crossbeam_channel::{bounded, select, tick};

    use crate::{
      acaot::native::{CompilerId, compiler_infra},
      management::{compiler_thread::JITOut, jitmem::JITMemoryManager},
    };

    let mut samgr = JITMemoryManager::new();

    let mut compiler_trampoline = HashMap::<CompilerId, Box<[u8]>>::new();
    for b in compiler_infra() {
      let mut c = b.get();

      let id = c.compiler_id();
      compiler_trampoline.insert(id, c.codegen_internal_trampoline());
    }

    // Compiler
    {
      let rs = resolve.as_ref();
      let compilers = compiler_infra();

      if compilers.is_empty() {
        return;
      }

      let [critical_s, important_s] = rs.heuristic_pgo();
      let mut critical = critical_s.into_iter().peekable();
      let mut important = important_s.into_iter().peekable();

      let others_iter = || {
        (0..=last)
          .filter(|x| !(critical_s.contains(x) || important_s.contains(x) || nativeptr.contains(x)))
          .peekable()
      };
      let mut others = others_iter();

      let (update, recv) = bounded::<JITOut>(20);

      let timer = tick(Duration::from_millis(200));

      let mut threads = 0usize;
      // critical node
      let tx_critical = {
        use std::thread;

        use crossbeam_channel::bounded;

        use crate::management::compiler_thread::compiler;

        let (tx, rx) = bounded::<(u64, usize, bool)>(20);

        while !tx.is_full()
          && let Some(x) = critical.next()
        {
          tx.try_send((*x, compilers.len() - 1, false))
            .expect("This cannot actually error");
        }

        let upd = update.clone();
        let rb = resolve.clone();

        threads += 1;
        thread::spawn(move || compiler(rb, rx, upd));

        tx
      };

      // fastlane node
      let tx_fastlane = {
        use std::thread;

        use crossbeam_channel::bounded;

        use crate::management::compiler_thread::compiler;

        let (tx, rx) = bounded::<(u64, usize, bool)>(20);

        while !tx.is_full()
          && let Some(x) = important.next()
        {
          tx.try_send((*x, compilers.len() - 1, false))
            .expect("This cannot actually error");
        }

        let upd = update.clone();
        let rb = resolve.clone();

        threads += 1;
        thread::spawn(move || compiler(rb, rx, upd));

        tx
      };
      let mut compiler_fastlane = 0;

      // Public node
      let tx_public = {
        use std::thread;

        use crossbeam_channel::bounded;

        use crate::management::compiler_thread::compiler;

        let (tx, rx) = bounded::<(u64, usize, bool)>(20);

        while !tx.is_full()
          && let Some(x) = others.next()
        {
          tx.try_send((x, compilers.len() - 1, false))
            .expect("This cannot actually error");
        }

        let upd = update.clone();
        let rb = resolve.clone();

        threads += 1;
        thread::spawn(move || compiler(rb, rx, upd));

        tx
      };
      let mut compiler_public = 0;

      loop {
        select! {
          recv(recv) -> val => {
            if let Ok(jitout) = val {
              match jitout {
                JITOut::Stopped => {
                  threads -= 1;
                }
                // We've gotten jitted output
                // Commit it & Update new JIT Data
                JITOut::JITData { moduleid, jitdata } => {
                  process_jit(resolve.as_ref(), &mut samgr, &mut evmap, moduleid, jitdata);
                }
              }
            }

            schedule!(tx_critical, tx_fastlane, tx_public, critical, important, others, compiler_fastlane, compiler_public, compilers, important_s, others_iter);
          }


          recv(timer) -> _ => {
            // Redundant, but JustInCase
            schedule!(tx_critical, tx_fastlane, tx_public, critical, important, others, compiler_fastlane, compiler_public, compilers, important_s, others_iter);

            // Break JIT if all modules are processes
            // Now we get into well - nicely linking all of them
            if threads == 0 && critical.peek().is_none()
              && important.peek().is_none()
              && others.peek().is_none()
            {
              break;
            }
          }
        }
      }
    }
  }
}

#[cfg(feature = "native")]
fn process_jit(
  resolver: &dyn BytecodeResolver,
  sajit: &mut JITMemoryManager,
  evmap: &mut WriteHandle<u64, SafeSwappableCodeStore>,
  moduleid: u64,
  cache: CacheData,
) {
  // Upload
  match cache {
    CacheData::None => {}
    cache => {
      resolver.update_cache(moduleid, cache.clone());

      match cache {
        CacheData::None | CacheData::Pickle { .. } => {}
        CacheData::CraneliftRel { .. } | CacheData::LLVMRel { .. } => {
          abort();
        }
        CacheData::CraneliftAbs8 { binary, reloc } | CacheData::LLVMAbs8 { binary, reloc } => {
          let relocs = calculate_relocation_abs(&reloc);

          let bin = sajit.write_quick(&binary, &relocs);

          if let Some(jitblob) = evmap.get_one(&moduleid) {
            // Case `usize` back into the `*mut JIT`
            let mgr = unsafe { &**jitblob };

            _ = unsafe { mgr.set(0, bin, None) };
          } else {
            let mgr = Box::new(SwappableCodeStore::new(bin));

            _ = unsafe { mgr.set(0, bin, None) };

            evmap.insert(moduleid, Box::into_raw(mgr));
            evmap.publish();
          }
        }
      }
    }
  }
}
