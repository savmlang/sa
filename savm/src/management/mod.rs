use crate::{
  BytecodeResolver, CODE_CACHE, CacheData, FNCALL_DISPATCH, SaVMJumps, SymbolMapTable, ThreadSafe,
  acaot::pickle::{PickleWorker, def::PickleInstruction},
};
use ahash::{HashMap, HashMapExt};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
#[cfg(feature = "native")]
use sajit::Executable;
use sart::structures::ffi::CallSig;
use std::sync::Arc;

// Native (JIT) layers
#[cfg(feature = "native")]
use crate::{
  CacheLevel, JIT_CACHE,
  acaot::native::{NativeCompilerBuilder, store::SwappableCodeSpace},
  management::jitmem::calculate_relocation_abs,
};
#[cfg(feature = "native")]
use crossbeam_channel::Sender;
#[cfg(feature = "native")]
use sart::code::SwappableCodeStore;
#[cfg(feature = "native")]
use std::{iter::Peekable, process::abort};
#[cfg(feature = "native")]
pub mod compiler_thread;
#[cfg(feature = "native")]
pub mod jitmem;
#[cfg(feature = "native")]
pub mod polyfills;
#[cfg(feature = "native")]
use jitmem::JITMemoryManager;

enum ProcessResult {
  Pickle(u64, Arc<[PickleInstruction]>, SaVMJumps, Arc<[u64]>),
  Native(u64, ThreadSafe<*const ()>, CallSig),
  None,
}

#[cfg(feature = "native")]
pub fn schedule<
  'a,
  'b,
  F: Fn() -> Peekable<I2>,
  E: Fn() -> Peekable<I3>,
  I1: Iterator<Item = &'a u64>,
  I2: Iterator<Item = &'b u64>,
  I3: Iterator<Item = u64>,
>(
  tx_critical: &Sender<(u64, usize, bool)>,
  tx_fastlane: &Sender<(u64, usize, bool)>,
  tx_public: &Sender<(u64, usize, bool)>,
  critical: &mut Peekable<I1>,
  important: &mut Peekable<I2>,
  others: &mut Peekable<I3>,
  compiler_fastlane: &mut usize,
  compiler_public: &mut usize,
  compilers: &[&dyn NativeCompilerBuilder<false>],
  important_s: F,
  others_iter: E,
) {
  /*
    Schedule more work through each sector
  */

  'critical_loop: while let Some(x) = critical.peek() {
    // Note: **x implies x is a reference to a reference/pointer
    if tx_critical
      .try_send((**x, compilers.len() - 1, false))
      .is_ok()
    {
      _ = critical.next();
    } else {
      break 'critical_loop;
    }
  }

  'important_loop: while let Some(x) = important.peek() {
    if tx_fastlane
      .try_send((**x, *compiler_fastlane, false))
      .is_ok()
    {
      _ = important.next();
    } else {
      break 'important_loop;
    }
  }

  'others_loop: while let Some(x) = others.peek() {
    if tx_public.try_send((*x, *compiler_public, false)).is_ok() {
      _ = others.next();
    } else {
      break 'others_loop;
    }
  }

  /*
    Sanity Checking
  */

  if critical.peek().is_none() {
    _ = tx_critical.try_send((0, 0, true));
  }

  if important.peek().is_none() {
    if *compiler_fastlane + 1 == compilers.len() {
      _ = tx_fastlane.try_send((0, 0, true));
    } else {
      *compiler_fastlane += 1;
      *important = important_s();
    }
  }

  if others.peek().is_none() {
    if *compiler_public + 1 == compilers.len() {
      _ = tx_public.try_send((0, 0, true));
    } else {
      *compiler_public += 1;
      *others = others_iter();
    }
  }
}

pub fn management_main<T: BytecodeResolver + Send + Sync + 'static>(resolve: Arc<T>) {
  let last = resolve.as_ref().last_section_id();

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
      ProcessResult::Native(module, fnptr, csig) => {
        _ = nativeptr.insert(module, (fnptr, csig));
      }
      _ => {}
    });

  let _nptr = FNCALL_DISPATCH.get_or_init(|| nativeptr);

  #[cfg(feature = "native")]
  {
    use crate::{
      acaot::native::compiler_infra,
      management::{compiler_thread::JITOut, jitmem::JITMemoryManager},
    };
    use crossbeam_channel::{bounded, select, tick};
    use std::time::Duration;

    let mut samgr = JITMemoryManager::new();

    // Compiler
    {
      use crate::permute::ShuffledSliceIter;
      use std::collections::HashSet;

      let evmap = unsafe { JIT_CACHE.get().unwrap_unchecked() };

      let rs = resolve.as_ref();
      let compilers = compiler_infra();

      if compilers.is_empty() {
        return;
      }

      let [critical_s, important_s] = rs.heuristic_pgo();

      let important_critical_nptr_hset = critical_s
        .iter()
        .copied()
        .chain(important_s.iter().copied())
        .chain(_nptr.iter().map(|x| *x.0))
        .collect::<HashSet<u64, ahash::RandomState>>();

      let mut critical = ShuffledSliceIter::new_panicking(critical_s).peekable();
      let mut important = ShuffledSliceIter::new_panicking(important_s).peekable();

      let others_iter = || {
        use crate::permute::HashedPermutation;
        use std::num::NonZeroU64;

        // Length: Last Index + 1
        HashedPermutation::new_panicking(NonZeroU64::new(last + 1).unwrap())
          .into_iter()
          .filter(|x| !important_critical_nptr_hset.contains(x))
          .peekable()
      };
      let mut others = others_iter();

      let (update, recv) = bounded::<JITOut>(20);

      let timer = tick(Duration::from_millis(200));

      let mut threads = 0usize;
      // critical node
      let tx_critical = {
        use crate::management::compiler_thread::compiler;
        use crossbeam_channel::bounded;
        use std::thread;

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
        thread::spawn(move || compiler::<false, _>(rb, rx, upd));

        tx
      };

      // fastlane node
      let tx_fastlane = {
        use crate::management::compiler_thread::compiler;
        use crossbeam_channel::bounded;
        use std::thread;

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
        thread::spawn(move || compiler::<false, _>(rb, rx, upd));

        tx
      };
      let mut compiler_fastlane = 0;

      // Public node
      let tx_public = {
        use crate::management::compiler_thread::compiler;
        use crossbeam_channel::bounded;
        use std::thread;

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
        thread::spawn(move || compiler::<false, _>(rb, rx, upd));

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
                  process_jit(resolve.as_ref(), evmap, &mut samgr, moduleid, jitdata);
                }
              }
            }

            schedule(&tx_critical, &tx_fastlane, &tx_public, &mut critical, &mut important, &mut others, &mut compiler_fastlane, &mut compiler_public, compilers, || ShuffledSliceIter::new_panicking(important_s).peekable(), others_iter);
          }


          recv(timer) -> _ => {
            // Redundant, but JustInCase
            schedule(&tx_critical, &tx_fastlane, &tx_public, &mut critical, &mut important, &mut others, &mut compiler_fastlane, &mut compiler_public, compilers, || ShuffledSliceIter::new_panicking(important_s).peekable(), others_iter);

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

    loop {
      use std::thread::sleep;

      sleep(Duration::MAX);
    }
  }
}

#[cfg(feature = "native")]
fn process_jit<T: BytecodeResolver + Send + Sync + 'static>(
  resolver: &T,
  evmap: &SwappableCodeSpace,
  sajit: &mut JITMemoryManager,
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
        CacheData::JITCache {
          level,
          binary,
          reloc,
        } => match level {
          CacheLevel::Pickle => {
            // How did Jesus allow this honestly?
            abort();
          }
          level => {
            // Logical Safety : To the same thread that has `set` the value - it will never ever
            // be out of date on that exact core.
            // If it gets context switched to a different core - the updates will still be flushed.
            let write = |bin: *const Executable, parent_counter: *mut usize| {
              if let Some(jitblob) = evmap.get(moduleid) {
                // Case `usize` back into the `*mut JIT`

                _ = unsafe { jitblob.set(0, bin, parent_counter, None) };
              } else {
                let mgr = SwappableCodeStore::new(bin, parent_counter);

                _ = unsafe { mgr.set(0, bin, parent_counter, None) };

                unsafe { evmap.set(moduleid, mgr) };
              }
            };

            match level {
              CacheLevel::CraneliftEpicenter => {
                todo!("Soon")
              }
              CacheLevel::LLVMEpitome => {
                todo!("Soon");
              }
              CacheLevel::LLVMCinder | CacheLevel::LLVMCrater => {
                let (bin, parent_counter) = sajit
                  .write_llvm(&binary, |_| {
                    panic!("Crater and Cinder need not resolve pointers.");
                  })
                  .expect("Unable to write LLVM JIT Memory");
                write(bin, parent_counter);
              }
              CacheLevel::CraneliftCrafter => {
                let relocs = calculate_relocation_abs(&reloc);

                let (bin, parent_counter) = sajit.write_quick(&binary, &relocs);
                write(bin, parent_counter);
              }
              _ => {}
            }
          }
        },
      }
    }
  }
}
