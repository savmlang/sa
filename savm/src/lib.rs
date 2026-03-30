#![feature(
  signed_bigint_helpers,
  nonpoison_rwlock,
  sync_nonpoison,
  read_array,
  widening_mul,
  adt_const_params
)]

pub mod acaot;

use std::{
  any::Any,
  hash::Hash,
  io::{Read, Seek},
  mem::zeroed,
  sync::{Arc, LazyLock, OnceLock, nonpoison::RwLock},
  thread::{self, available_parallelism},
  time::Duration,
};

use ahash::HashMap;
use evmap::handles::ReadHandle;
use moka::sync::{CacheBuilder, SegmentedCache};
use sart::{code::SwappableCodeStore, ctr::CVMTaskState};

pub use sart;
use tokio::runtime::{Builder, Runtime};

use crate::{acaot::pickle::def::PickleInstruction, management::management_main};

pub mod executor;
pub(crate) mod management;
pub mod sync;

pub static TOTAL_THREADS: LazyLock<usize> =
  LazyLock::new(|| available_parallelism().unwrap().into());
static VMMADE: OnceLock<()> = OnceLock::new();

pub enum SymbolMapTable<T> {
  NativePointer {
    fnptr: extern "C" fn(vm: *mut CVMTaskState),
  },
  MixedSizedBytecode {
    bytecode: T,
  },
}

pub enum SymbolMapTableInfo {
  NativePointer,
  MixedSizedBytecode,
}
pub enum CacheData {
  None,
  Pickle {
    out: Arc<[PickleInstruction]>,
    jumps: Arc<HashMap<u64, usize>>,
  },
  CraneliftAbs8 {},
  CraneliftRel {},
  LLVMAbs8 {},
  LLVMRel {},
}

pub enum CacheLevel {
  Pickle,
  CraneliftAbs8,
  CraneliftRel,
  LLVMAbs8,
  LLVMRel,
}

pub trait ResolvedData: Read + Seek {}

impl<T: Read + Seek> ResolvedData for T {}

pub trait BytecodeResolver: Any {
  /// Return the id of the LAST VALID section
  /// We use this to prevent unnecessary [u64] allocation
  fn last_section_id(&self) -> u64;

  /// Returns an heuristic list upto 500 elements in size over 2 clusters
  ///
  /// Cluster 1 (idx = 0)
  /// - Absolute top-notch priority entitled to DIRECT upgrade the the highest JIT Level
  ///
  /// Cluster 2 (idx = 1)
  /// - Priority over other modules
  fn heuristic_pgo(&self) -> [&[u64]; 2];

  /// Resolve the symbol map table
  fn resolve_data(&self, section: u64) -> SymbolMapTable<Box<dyn ResolvedData>>;

  /// Learn about the data present
  fn learn_data(&self, section: u64) -> SymbolMapTableInfo;

  /// Checks if the cache is available!
  fn get_best_cache(&self, section: u64) -> CacheData;

  /// Checks if the cache is available!
  fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData;

  /// Updates the cache
  ///
  /// We hope the callee only updates the tier of cache this produces
  ///
  /// eg. we hope it does not replace Pickle code with Cranelift code as that'll lead to performance losses next round
  fn update_cache(&self, section: u64, cache: CacheData);
}

impl BytecodeResolver for Box<dyn BytecodeResolver + Send + Sync + 'static> {
  fn get_best_cache(&self, section: u64) -> CacheData {
    BytecodeResolver::get_best_cache(self.as_ref(), section)
  }

  fn heuristic_pgo(&self) -> [&[u64]; 2] {
    BytecodeResolver::heuristic_pgo(self.as_ref())
  }

  fn resolve_data(&self, section: u64) -> SymbolMapTable<Box<dyn ResolvedData>> {
    BytecodeResolver::resolve_data(self.as_ref(), section)
  }

  fn learn_data(&self, section: u64) -> SymbolMapTableInfo {
    BytecodeResolver::learn_data(self.as_ref(), section)
  }

  fn last_section_id(&self) -> u64 {
    BytecodeResolver::last_section_id(self.as_ref())
  }

  fn update_cache(&self, section: u64, cache: CacheData) {
    BytecodeResolver::update_cache(self.as_ref(), section, cache)
  }

  fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData {
    BytecodeResolver::get_cache(self.as_ref(), section, level)
  }
}

pub static GLOBAL_RUNTIME: LazyLock<Runtime> =
  LazyLock::new(|| Builder::new_multi_thread().enable_all().build().unwrap());

pub static VMCONF: RwLock<VmConfig> = RwLock::new(unsafe { zeroed() });

// This only and only stores Subroutine-Threaded instructions
pub(crate) static CODE_CACHE: LazyLock<
  SegmentedCache<u64, (Arc<[PickleInstruction]>, Arc<HashMap<u64, usize>>), ahash::RandomState>,
> = LazyLock::new(|| {
  CacheBuilder::new(1 << 10) // 2^10 = 1024
    .segments(available_parallelism().map(|x| x.get()).unwrap_or(4))
    .time_to_live(Duration::from_mins(20))
    .time_to_idle(Duration::from_mins(5))
    .build_with_hasher(ahash::RandomState::default())
});

pub type JITStorage = *mut SwappableCodeStore<()>;

// This only and only stores JIT instructions
pub(crate) static JIT_CACHE: OnceLock<ThreadSafe<ReadHandle<u64, usize>>> = OnceLock::new();

#[derive(Debug, Clone, Copy)]
pub(crate) struct ThreadSafe<T>(pub T);

unsafe impl<T> Send for ThreadSafe<T> {}
unsafe impl<T> Sync for ThreadSafe<T> {}

impl<T: Hash> Hash for ThreadSafe<T> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.0.hash(state)
  }
}

impl<T: PartialEq> PartialEq for ThreadSafe<T> {
  fn eq(&self, other: &Self) -> bool {
    self.0.eq(&other.0)
  }

  fn ne(&self, other: &Self) -> bool {
    self.0.ne(&other.0)
  }
}

#[derive(Debug)]
#[repr(C)]
pub struct VmConfig {
  pub jit: bool,
  pub cooperative: bool,
}

/// We create a VM for each thread executed
#[repr(C)]
pub struct VM {
  pub resolve: Arc<dyn BytecodeResolver + Send + Sync + 'static>,
}

unsafe impl Send for VM {}
unsafe impl Sync for VM {}

pub fn pack_u32(high_u32: u32, low_u32: u32) -> u64 {
  let high_u64 = high_u32 as u64;
  let shifted_high = high_u64 << 32;
  let low_u64 = low_u32 as u64;

  shifted_high | low_u64
}

pub fn pack_u64(high_u64: u64, low_u64: u64) -> u128 {
  let high_u64 = high_u64 as u128;
  let shifted_high = high_u64 << 64;
  let low_u64 = low_u64 as u128;

  shifted_high | low_u64
}

pub fn unpack_u64(packed: u64) -> (u32, u32) {
  let high_u32 = (packed >> 32) as u32;

  let low_u32 = packed as u32;

  (high_u32, low_u32)
}

impl VM {
  /// Please note that module id `0` represents the main module
  pub fn new<T: BytecodeResolver + Send + Sync + 'static>(data: T) -> Self {
    CODE_CACHE.run_pending_tasks();
    VMMADE.set(()).expect("Each process can only have 1 VM");

    let resolver = Arc::new(data);

    // Start Management Thread
    {
      let resolve = resolver.clone();

      let (writer, reader) = evmap::new::<u64, usize>();
      JIT_CACHE.set(ThreadSafe(reader)).expect("impossible");

      thread::spawn(move || management_main(writer, resolve));
    }

    Self { resolve: resolver }
  }
}

pub enum MaybeBoxed<T> {
  Boxed(Box<T>),
  Unboxed(T),
}
