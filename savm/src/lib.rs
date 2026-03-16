#![allow(unused)]
#![feature(
  seek_stream_len,
  signed_bigint_helpers,
  exact_div,
  int_roundings,
  nonpoison_rwlock,
  sync_nonpoison,
  unsafe_cell_access,
  read_array,
  widening_mul
)]

pub mod acaot;

use std::{
  fs::File,
  hash::Hash,
  io::{Read, Seek},
  mem::zeroed,
  os::raw::c_void,
  sync::{Arc, LazyLock, OnceLock, atomic::Ordering, nonpoison::RwLock},
  thread::{self, available_parallelism},
  time::Duration,
};

use evmap::{StableHashEq, handles::ReadHandle};
use moka::sync::{CacheBuilder, SegmentedCache};
use sart::{
  code::SwappableCodeStore,
  ctr::{CVMTaskState, Instruction},
};

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

pub enum CacheData {
  None,
  Pickle {
    out: Box<[PickleInstruction]>,
  },
  #[cfg(feature = "cranelift")]
  CraneliftAbs8 {},
  #[cfg(feature = "cranelift")]
  CraneliftRel {},
  #[cfg(feature = "llvm")]
  LLVMAbs8 {},
  #[cfg(feature = "llvm")]
  LLVMRel {},
}

pub enum CacheLevel {
  Pickle,
  #[cfg(feature = "cranelift")]
  CraneliftAbs8,
  #[cfg(feature = "cranelift")]
  CraneliftRel,
  #[cfg(feature = "llvm")]
  LLVMAbs8,
  #[cfg(feature = "llvm")]
  LLVMRel,
}

pub trait BytecodeResolver {
  type Output: Read + Seek;

  /// Return the id of the LAST VALID section
  /// We use this to prevent unnecessary [u64] allocation
  fn last_section_id(&self) -> u64;

  /// Resolve the symbol map table
  fn resolve_data(&self, section: u64) -> SymbolMapTable<Self::Output>;

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

impl BytecodeResolver for Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static> {
  type Output = File;

  fn get_best_cache(&self, section: u64) -> CacheData {
    BytecodeResolver::get_best_cache(self.as_ref(), section)
  }

  fn resolve_data(&self, section: u64) -> SymbolMapTable<Self::Output> {
    BytecodeResolver::resolve_data(self.as_ref(), section)
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
  SegmentedCache<u64, Arc<Box<[PickleInstruction]>>, ahash::RandomState>,
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
pub struct VM<T: BytecodeResolver + Send + Sync + 'static> {
  pub resolve: Arc<T>,
}

unsafe impl<T: BytecodeResolver + Send + Sync + 'static> Send for VM<T> {}
unsafe impl<T: BytecodeResolver + Send + Sync + 'static> Sync for VM<T> {}

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

impl<T: BytecodeResolver + Send + Sync + 'static> VM<T> {
  /// Please note that module id `0` represents the main module
  pub fn new(data: T) -> Self {
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
