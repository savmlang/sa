pub mod acaot;
pub mod ints;

pub use ahash;
use serde::{Deserialize, Serialize};
use std::{
  any::Any,
  hash::Hash,
  io::Read,
  sync::{Arc, LazyLock, OnceLock},
  thread::{self, available_parallelism},
  time::Duration,
};

use ahash::HashMap;
use moka::sync::{CacheBuilder, SegmentedCache};

#[cfg(feature = "libffi")]
use sart::structures::ffi::CallSig;

pub use sart;

#[cfg(feature = "native")]
use crate::acaot::native::store::SwappableCodeSpace;
use crate::{
  acaot::{JITReloc, Stencils, pickle::def::PickleInstruction},
  management::management_main,
};

pub mod executor;
pub mod kvwrap;
pub mod management;
pub mod permute;
pub mod sync;

pub static TOTAL_THREADS: LazyLock<usize> =
  LazyLock::new(|| available_parallelism().unwrap().into());
static VMMADE: OnceLock<()> = OnceLock::new();

pub enum SymbolMapTable<T> {
  #[cfg(feature = "libffi")]
  NativePointer {
    fnptr: *const (),
    cdecl: CallSig,
  },
  MixedSizedBytecode {
    bytecode: T,
  },
}

#[repr(C)]
pub enum SymbolMapTableInfo {
  #[cfg(feature = "libffi")]
  NativePointer,
  MixedSizedBytecode,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct PickleJumpData {
  pub marker: u64,
  pub loc: usize,
}

pub type JITRelocs = Arc<[JITReloc]>;
pub type LibCalls = Arc<[u64]>;
pub type SaVMJumps = Arc<[PickleJumpData]>;

#[derive(Debug, Clone)]
pub enum CacheData {
  None,
  Pickle {
    out: Arc<[PickleInstruction]>,
    jumps: SaVMJumps,
    /// This should be None for returned CacheData
    libcalls: Option<LibCalls>,
  },
  JITCache {
    level: CacheLevel,
    binary: Arc<[u8]>,
    reloc: JITRelocs,
  },
  CinderTempCache {
    entrymap: Arc<[Box<[u8]>]>,
    binary: Stencils,
  },
}

unsafe impl Send for CacheData {}
unsafe impl Sync for CacheData {}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum CacheLevel {
  Pickle,
  ACAoTCinder,
  CraneliftCrafter,
  CraneliftEpicenter,
  LLVMCrater,
  LLVMEpitome,
}

pub const OPTLEVEL_PICKLE: i64 = 0;

impl CacheLevel {
  pub fn to_int(&self) -> u8 {
    match self {
      Self::Pickle => 0,
      Self::ACAoTCinder => 1,
      Self::CraneliftCrafter => 2,
      Self::CraneliftEpicenter => 3,
      Self::LLVMCrater => 4,
      Self::LLVMEpitome => 5,
    }
  }

  pub fn from_int(cachelevel: i64) -> Option<Self> {
    Some(match cachelevel {
      0 => Self::Pickle,
      1 => Self::ACAoTCinder,
      2 => Self::CraneliftCrafter,
      3 => Self::CraneliftEpicenter,
      4 => Self::LLVMCrater,
      5 => Self::LLVMEpitome,
      _ => return None,
    })
  }
}

pub trait ResolvedData: Read {}

impl<T: Read> ResolvedData for T {}

pub trait BytecodeResolver: Any {
  type T<'a>: ResolvedData
  where
    Self: 'a;

  /// Read Only data - this is the part of SaVM Global Data
  /// that is loaded as READ-ONLY
  fn rodata(&self) -> &[u8];

  /// Read Write data - this is the part of SaVM Global Data
  /// that is both readable and writable.
  ///
  /// These methods are expected to be zero cost
  /// and the location of storage should NOT change
  fn rwdata(&self) -> &mut [u8];

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
  fn heuristic_pgo<'a>(&'a self) -> [&'a [u64]; 2];

  /// Resolve the symbol map table
  fn resolve_data<'a>(&'a self, section: u64) -> SymbolMapTable<Self::T<'a>>;

  /// Learn about the data present
  fn learn_data(&self, section: u64) -> SymbolMapTableInfo;

  /// Checks if the cache is available!
  fn get_best_cache(&self, section: u64) -> CacheData;

  /// Checks if the cache is available!
  fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData;

  /// Gets the SaVM libraries it depends on
  fn get_libcalls(&self, section: u64) -> Option<LibCalls>;

  /// Updates the cache
  ///
  /// We hope the callee only updates the tier of cache this produces
  ///
  /// eg. we hope it does not replace Pickle code with Cranelift code as that'll lead to performance losses next round
  fn update_cache(&self, section: u64, cache: CacheData);
}

#[cfg(feature = "libffi")]
pub(crate) static FNCALL_DISPATCH: OnceLock<HashMap<u64, (ThreadSafe<*const ()>, CallSig)>> =
  OnceLock::new();

// This only and only stores Subroutine-Threaded instructions and their associated jumps
// We dont directly store libcalls
pub(crate) static CODE_CACHE: LazyLock<
  SegmentedCache<u64, (Arc<[PickleInstruction]>, SaVMJumps), ahash::RandomState>,
> = LazyLock::new(|| {
  CacheBuilder::new(1 << 10) // 2^10 = 1024
    .segments(available_parallelism().map(|x| x.get()).unwrap_or(4))
    .time_to_live(Duration::from_mins(20))
    .time_to_idle(Duration::from_mins(5))
    .build_with_hasher(ahash::RandomState::default())
});

#[cfg(feature = "native")]
pub use sajit::Executable;

// This only and only stores JIT instructions
#[cfg(feature = "native")]
pub static JIT_CACHE: OnceLock<SwappableCodeSpace> = OnceLock::new();

#[derive(Debug, Clone, Copy)]
pub struct ThreadSafe<T>(pub T);

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
  pub fn new(data: T) -> Self {
    unsafe { Self::new_unsafe::<true>(data) }
  }
  /// Please note that module id `0` represents the main module
  ///
  /// This is not really `unsafe`
  /// This is **unsafe** by intent
  pub unsafe fn new_unsafe<const MGNTHTREAD: bool>(data: T) -> Self {
    CODE_CACHE.run_pending_tasks();
    VMMADE.set(()).expect("Each process can only have 1 VM");

    let resolver = Arc::new(data);

    // Start Management Thread
    if MGNTHTREAD {
      let resolve = resolver.clone();

      #[cfg(feature = "native")]
      JIT_CACHE
        .set(SwappableCodeSpace::create(resolve.as_ref().last_section_id() as usize + 1).unwrap())
        .expect("impossible");

      thread::Builder::new()
        .name("JIT Management".into())
        // 32KiB stack space
        .stack_size(32 * 1024)
        .spawn(move || management_main(resolve))
        .expect("Unable to spawn management thread");
    }

    Self { resolve: resolver }
  }
}

pub enum MaybeBoxed<T> {
  Boxed(Box<T>),
  Unboxed(T),
}
