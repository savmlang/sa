#[cfg(feature = "native")]
use crate::{
  BytecodeResolver, JIT_CACHE,
  acaot::native::compiler_infra,
  management::{
    compiler_thread::{JITOut, compiler},
    jitmem::JITMemoryManager,
    processjit::process_jit,
    schedule::schedule,
  },
  permute::{HashedPermutation, ShuffledSliceIter},
};
#[cfg(feature = "native")]
use crossbeam_channel::{Sender, bounded, select, tick};
#[cfg(feature = "native")]
use std::{collections::HashSet, num::NonZeroU64, sync::Arc, thread, time::Duration};

/// Spawns a dedicated JIT compiler worker thread and primes its initial queue.
#[cfg(feature = "native")]
fn spawn_worker_ref<
  'a,
  T: BytecodeResolver + Send + Sync + 'static,
  I: Iterator<Item = &'a u64>,
>(
  name: &str,
  resolve: Arc<T>,
  update: Sender<JITOut>,
  queue: &mut std::iter::Peekable<I>,
  compilers_len: usize,
) -> Sender<(u64, usize, bool)> {
  let (tx, rx) = bounded::<(u64, usize, bool)>(20);

  while !tx.is_full()
    && let Some(x) = queue.next()
  {
    tx.try_send((*x, compilers_len - 1, false))
      .expect("This cannot actually error");
  }

  thread::Builder::new()
    .name(name.into())
    // 2MiB stack for thread
    .stack_size(2 * 1024 * 1024)
    .spawn(move || compiler::<false, _>(resolve, rx, update))
    .unwrap_or_else(|e| panic!("Unable to start thread for {name}: {e}"));

  tx
}

/// Spawns a dedicated JIT compiler worker thread for owned item iterators.
#[cfg(feature = "native")]
fn spawn_worker_owned<T: BytecodeResolver + Send + Sync + 'static, I: Iterator<Item = u64>>(
  name: &str,
  resolve: Arc<T>,
  update: Sender<JITOut>,
  queue: &mut std::iter::Peekable<I>,
  compilers_len: usize,
) -> Sender<(u64, usize, bool)> {
  let (tx, rx) = bounded::<(u64, usize, bool)>(20);

  while !tx.is_full()
    && let Some(x) = queue.next()
  {
    tx.try_send((x, compilers_len - 1, false))
      .expect("This cannot actually error");
  }

  thread::Builder::new()
    .name(name.into())
    // 2MiB stack for thread
    .stack_size(2 * 1024 * 1024)
    .spawn(move || compiler::<false, _>(resolve, rx, update))
    .unwrap_or_else(|e| panic!("Unable to start thread for {name}: {e}"));

  tx
}

/// Runs the JIT compiler orchestrator, spawning workers and managing the scheduling event loop.
#[cfg(feature = "native")]
pub fn run_jit_orchestrator<T: BytecodeResolver + Send + Sync + 'static>(resolve: Arc<T>) {
  let mut samgr = JITMemoryManager::new();

  let evmap = unsafe { JIT_CACHE.get().unwrap_unchecked() };

  let rs = resolve.as_ref();
  let last = rs.last_section_id();
  let compilers_len = compiler_infra::<false, T>().len();

  if compilers_len == 0 {
    return;
  }

  let [critical_s, important_s] = rs.heuristic_pgo();

  #[cfg(feature = "libffi")]
  let nptr_keys: Vec<u64> = crate::FNCALL_DISPATCH
    .get()
    .map(|m| m.keys().copied().collect())
    .unwrap_or_default();

  #[cfg(not(feature = "libffi"))]
  let nptr_keys: Vec<u64> = Vec::new();

  let important_critical_nptr_hset = critical_s
    .iter()
    .copied()
    .chain(important_s.iter().copied())
    .chain(nptr_keys.into_iter())
    .collect::<HashSet<u64, ahash::RandomState>>();

  let mut critical = ShuffledSliceIter::new_panicking(critical_s).peekable();
  let mut important = ShuffledSliceIter::new_panicking(important_s).peekable();

  let others_iter = || {
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

  // Critical node
  let tx_critical = spawn_worker_ref(
    "JIT Worker #0 - Critical",
    resolve.clone(),
    update.clone(),
    &mut critical,
    compilers_len,
  );
  threads += 1;

  // Fastlane node
  let tx_fastlane = spawn_worker_ref(
    "JIT Worker #1 - Important",
    resolve.clone(),
    update.clone(),
    &mut important,
    compilers_len,
  );
  threads += 1;
  let mut compiler_fastlane = 0;

  // Public node
  let tx_public = spawn_worker_owned(
    "JIT Worker #2 - Others",
    resolve.clone(),
    update.clone(),
    &mut others,
    compilers_len,
  );
  threads += 1;
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

        schedule(
          &tx_critical,
          &tx_fastlane,
          &tx_public,
          &mut critical,
          &mut important,
          &mut others,
          &mut compiler_fastlane,
          &mut compiler_public,
          compilers_len,
          || ShuffledSliceIter::new_panicking(important_s).peekable(),
          others_iter,
        );
      }

      recv(timer) -> _ => {
        // Redundant, but JustInCase
        schedule(
          &tx_critical,
          &tx_fastlane,
          &tx_public,
          &mut critical,
          &mut important,
          &mut others,
          &mut compiler_fastlane,
          &mut compiler_public,
          compilers_len,
          || ShuffledSliceIter::new_panicking(important_s).peekable(),
          others_iter,
        );

        // Break JIT if all modules are processes
        // Now we get into well - nicely linking all of them
        if threads == 0
          && critical.peek().is_none()
          && important.peek().is_none()
          && others.peek().is_none()
        {
          break;
        }
      }
    }
  }

  loop {
    thread::sleep(Duration::MAX);
  }
}
