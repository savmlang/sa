#![feature(
  seek_stream_len,
  portable_simd,
  unchecked_shifts,
  exact_div,
  int_roundings,
  nonpoison_rwlock,
  sync_nonpoison,
  unsafe_cell_access
)]

pub mod acaot;

use std::{
  fs::File,
  io::{Read, Seek},
  mem::zeroed,
  sync::{
    Arc, LazyLock, OnceLock,
    atomic::{AtomicUsize, Ordering},
    mpsc::{Receiver, channel},
    nonpoison::RwLock,
  },
  thread::available_parallelism,
};

use crate::acaot::sync_compile;
use dashmap::DashMap;
use sart::{
  ctr::{CVMTaskState, DispatchFn, FnInstr, Instruction},
  saffi::boxed::{
    RTSafeBoxWrapper,
    spawn::{SendWrapper, ThreadSpawnContext, send},
  },
  structures::CompiledCode,
};

pub use sart;
use tokio::runtime::{Builder, Runtime};

pub mod executor;
pub mod sync;

pub(crate) static VMS: AtomicUsize = AtomicUsize::new(1);

static TOTAL_THREADS: LazyLock<usize> = LazyLock::new(|| available_parallelism().unwrap().into());
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
  Pickle {},
  #[cfg(feature = "llvm")]
  Cranelift {},
  #[cfg(feature = "cranelift")]
  LLVM {},
}

pub trait BytecodeResolver {
  type Output: Read + Seek;

  fn sections(&self) -> &[u64];

  fn resolve_data(&self, section: u64) -> SymbolMapTable<Self::Output>;

  fn get_best_cache(&self, section: u64) -> CacheData;
}

impl BytecodeResolver for Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static> {
  type Output = File;

  fn get_best_cache(&self, section: u64) -> CacheData {
    BytecodeResolver::get_best_cache(self, section)
  }

  fn resolve_data(&self, section: u64) -> SymbolMapTable<Self::Output> {
    BytecodeResolver::resolve_data(self, section)
  }

  fn sections(&self) -> &[u64] {
    BytecodeResolver::sections(self)
  }
}

pub static GLOBAL_RUNTIME: LazyLock<Runtime> =
  LazyLock::new(|| Builder::new_multi_thread().enable_all().build().unwrap());

pub static VMCONF: RwLock<VmConfig> = RwLock::new(unsafe { zeroed() });

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
  pub code: CompiledCode,
  pub recv: Option<Receiver<SendWrapper>>,
  pub counter: usize,
  /// This is the 1st pointer to the heap structure
  /// Ofc there are total `256` distinct addresses
  pub heapmap: *mut HeapStructure,
}

pub const INNERBYTES: usize = 2 * size_of::<Arc<()>>() + size_of::<Option<Receiver<SendWrapper>>>();

#[repr(C)]
/// VM but optimized to use from C/FFI Boundaries
pub struct CVM {
  pub _inner: [u8; INNERBYTES],
  pub counter: usize,
  pub heapmap: *mut HeapStructure,
}

const _PASS1: bool = size_of::<CVM>()
  == size_of::<VM<Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static>>>();
const _VERIFY1: () = assert!(_PASS1);

const _PASS2: bool = align_of::<CVM>()
  == align_of::<VM<Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static>>>();
const _VERIFY2: () = assert!(_PASS2);

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
    VMMADE.set(()).expect("Cell must be initialized only once. We know there will be morons and that's why for the LOVE OF GOD, don't try this trick again");

    let resolver = Arc::new(data);

    let resolve = resolver.clone();

    Self {
      resolve: resolve,
      counter: 0,
      recv: None,
      heapmap: unsafe { zeroed() },
      code: {
        let out: CompiledCode = Arc::new(DashMap::with_hasher(ahash::RandomState::new()));

        let refsolver = resolver.as_ref();
        let refsolver_ptr = resolver.clone();

        refsolver.modules().iter().for_each(|id| {
          let modid = *id;

          let refsolver_ptr = refsolver_ptr.clone();

          match refsolver.get_regions(modid) {
            Some(regions) => regions.iter().for_each(|region| {
              let region = *region;

              let res = refsolver_ptr.clone();
              out.insert(
                pack_u32(modid, region),
                LazyLock::new(Box::new(move || sync_compile(res.as_ref(), modid, region))),
              );
            }),
            None => {
              refsolver
                .get_native_regions(modid)
                .iter()
                .for_each(|region| {
                  let region = *region;

                  let output = refsolver_ptr.resolve_native(modid, region);

                  out.insert(
                    pack_u32(modid, region),
                    LazyLock::new(Box::new(move || {
                      Box::new([Instruction {
                        fn_: FnInstr {
                          arg: 0,
                          dispatch: output,
                        },
                      }])
                    })),
                  );
                });
            }
          }
        });

        out
      },
    }
  }

  /// This returns a Boxed copy is there are more than 5 VMs already
  pub fn create_copy(&self) -> (*mut RTSafeBoxWrapper, MaybeBoxed<Self>) {
    let old = VMS.fetch_add(1, Ordering::SeqCst);

    let (tx, rx) = channel::<SendWrapper>();

    let tx = unsafe { RTSafeBoxWrapper::new_raw(tx) };

    let tx = unsafe { RTSafeBoxWrapper::new_raw(ThreadSpawnContext { send, sender: tx }) };

    if old >= *TOTAL_THREADS {
      return (
        tx,
        MaybeBoxed::Boxed(Box::new(Self {
          code: self.code.clone(),
          heapmap: unsafe { zeroed() },
          counter: 0,
          recv: Some(rx),
          resolve: self.resolve.clone(),
        })),
      );
    }

    (
      tx,
      MaybeBoxed::Unboxed(Self {
        code: self.code.clone(),
        heapmap: unsafe { zeroed() },
        counter: 0,
        recv: Some(rx),
        resolve: self.resolve.clone(),
      }),
    )
  }
}

impl<T: BytecodeResolver + Send + Sync + 'static> Drop for VM<T> {
  fn drop(&mut self) {
    VMS.fetch_sub(1, Ordering::SeqCst);
  }
}

pub enum MaybeBoxed<T> {
  Boxed(Box<T>),
  Unboxed(T),
}
