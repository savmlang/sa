#![feature(
  seek_stream_len,
  portable_simd,
  unchecked_shifts,
  exact_div,
  int_roundings,
  core_intrinsics
)]

pub mod acaot;

use std::{
  io::{Read, Seek},
  sync::{
    Arc, LazyLock, OnceLock,
    atomic::{AtomicUsize, Ordering},
  },
  thread::available_parallelism,
};

use crate::acaot::sync_compile;
use sart::{
  ctr::{DispatchFn, Instruction},
  map::{CompiledCode, HashMap, Heap},
};

pub mod executor;
pub mod sync;

pub(crate) static VMS: AtomicUsize = AtomicUsize::new(1);

static TOTAL_THREADS: LazyLock<usize> = LazyLock::new(|| available_parallelism().unwrap().into());
static VMMADE: OnceLock<()> = OnceLock::new();

pub trait BytecodeResolver {
  fn modules(&self) -> &[u32];

  /// We'll outselves parse it
  /// Return `NONE` only when it is not a bytecode module
  /// No other error can be accepted
  fn get_regions(&self, module: u32) -> Option<&[u32]>;

  /// We'll outselves parse it
  fn get_native_regions(&self, module: u32) -> &[u32];

  /// We'll outselves compile it
  ///
  /// Return `None` ONLY IF it is a native module (like a dylib)
  fn resolve_bytecode_exact(&self, module: u32, region: u32) -> Option<impl Read + Seek>;

  /// No other error can be accepted
  fn resolve_native(&self, module: u32, func: u32) -> DispatchFn;
}

/// We create a VM for each thread executed
pub struct VM<T: BytecodeResolver + Send + Sync + 'static> {
  pub(crate) resolve: Arc<T>,
  pub(crate) heap: Heap,
  pub(crate) counter: usize,
  code: Arc<CompiledCode>,
}

pub fn pack_u32(high_u32: u32, low_u32: u32) -> u64 {
  let high_u64 = high_u32 as u64;
  let shifted_high = high_u64 << 32;
  let low_u64 = low_u32 as u64;

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
      heap: HashMap::default(),
      code: {
        let mut out: CompiledCode = HashMap::default();

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
                      Box::new([Instruction { fn_: (0, output) }])
                    })),
                  );
                });
            }
          }
        });

        Arc::new(out)
      },
    }
  }

  /// This returns a Boxed copy is there are more than 5 VMs already
  pub fn create_copy(&self) -> MaybeBoxed<Self> {
    let old = VMS.fetch_add(1, Ordering::SeqCst);

    if old >= *TOTAL_THREADS {
      return MaybeBoxed::Boxed(Box::new(Self {
        code: self.code.clone(),
        heap: HashMap::default(),
        counter: 0,
        resolve: self.resolve.clone(),
      }));
    }

    MaybeBoxed::Unboxed(Self {
      code: self.code.clone(),
      heap: HashMap::default(),
      counter: 0,
      resolve: self.resolve.clone(),
    })
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
