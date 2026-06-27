cprelude::cprelude! {
  SAVMC
}

pub mod resolvedata;
pub mod vm;

use crate::resolvedata::{SAVMC_IResolveData, SAVMC_IStream};
use cprelude::{Slicable, SlicableMut};
pub use savm::CacheLevel;
use savm::{
  acaot::{pickle::def::PickleInstruction, JITReloc},
  BytecodeResolver, CacheData, PickleJumpData, SymbolMapTable, SymbolMapTableInfo,
};
use std::{os::raw::c_void, sync::Arc};

#[repr(C)]
#[allow(non_camel_case_types)]
pub enum SAVMC_ISymbolMapTable {
  NativePointer {
    fnptr: *const (),
    cdecl: SAVMC_ISlice,
  },
  MixedSizedBytecode_TypeSlice {
    /// This MUST be valid until the `free`-ing of this block.
    ///
    /// Also, it is expected to be cached so that it can be effectively reused
    /// We assume the implementor holds the variants or prefer `MixedSizedBytecode_`
    bytecode: SAVMC_ISlice,
  },
  MixedSizedBytecode_ {
    /// For better locality - we expect this structure is cached.
    /// The above is only a recommendation though
    bytecode: SAVMC_IResolveData,
  },
}

type ISymbolMapTable = SAVMC_ISymbolMapTable;

#[repr(C)]
/// The PGO Data - This is a `'VM` lifetime object.
///
/// This means that the 2 arrays returned via here must be valid for the entire lifecycle of the VM
///
/// id0  =  critical priority
/// id1  =  high priority
pub struct SAVMC_PGOData {
  pub id0: SAVMC_ISlice_Impl<u64>,
  pub id1: SAVMC_ISlice_Impl<u64>,
}

#[repr(C)]
pub struct SAVMC_ISaVMJump {
  pub marker: u64,
  pub sectid: usize,
}

#[repr(C)]
/// ## If received:
/// This is a temporary cache data with a `0`-lifetime
/// This must be immediately used or else pointer becomes dangling.
///
/// You are supposed to copy it all and keep it safe.
///
/// ## If Sending
/// This will be faithfully copied by the library - so it also is
/// a `0`-lifetime while sending.
pub enum SAVMC_ICacheData {
  None,
  Pickle {
    pickle: SAVMC_ISlice_Impl<PickleInstruction>,

    libcalls: SAVMC_Maybe<SAVMC_ISlice_Impl<u64>>,
    jmps: SAVMC_ISlice_Impl<PickleJumpData>,
  },
  JITCache {
    level: CacheLevel,

    binary: SAVMC_ISlice,
    relocs: SAVMC_ISlice_Impl<JITReloc>,
  },
}

type ICacheData = SAVMC_ICacheData;

#[repr(C)]
/// The VM may invoke any callback concurrently from multiple threads.
///
/// The implementation MUST ensure that:
/// - state is thread-safe
/// - callbacks are thread-safe
/// - returned data remains valid according to the documented lifetime
///
/// Failure to do so results in undefined behaviour.
pub struct SAVMC_IBytecodeResolver {
  /// Caller stored data
  pub state: *mut c_void,

  /// Fetches the ROData section
  ///
  /// This slice must be vvalid for the whole lifetime
  /// of this Resolver!
  pub get_rodata: extern "C" fn(*mut c_void) -> SAVMC_ISlice_Impl<u8>,

  /// Fetches the RWData section (mutable)
  ///
  /// This slice must be vvalid for the whole lifetime
  /// of this Resolver!
  pub get_rwdata: extern "C" fn(*mut c_void) -> SAVMC_IMSlice_Impl<u8>,

  /// Get the last section id of the VM
  pub last_section_id_ptr: extern "C" fn(*mut c_void) -> u64,
  /// Resolve a section to [`SAVMC_ISymbolMapTable`]
  pub resolve_data_ptr: extern "C" fn(*mut c_void, section: u64) -> SAVMC_ISymbolMapTable,
  /// Consume a [`SAVMC_ICacheData`] for the section id
  pub update_cache_ptr: extern "C" fn(*mut c_void, section: u64, cache: SAVMC_ICacheData),
  /// Resolve a section's [`SymbolMapTableInfo`]
  pub learn_data_ptr: extern "C" fn(*mut c_void, section: u64) -> SymbolMapTableInfo,
  /// Get the [`SAVMC_PGOData`]
  pub heuristic_pgo_ptr: extern "C" fn(*mut c_void) -> SAVMC_PGOData,
  /// Get the libcalls
  ///
  /// We allow you to give us a stream which we eagerly collect.
  pub get_libcalls_ptr: extern "C" fn(*mut c_void, section: u64) -> SAVMC_Maybe<SAVMC_IStream<u64>>,

  /// Gets the best cache level available, an [`SAVMC_ICacheData`]
  pub get_best_cache_ptr: extern "C" fn(*mut c_void, section: u64) -> SAVMC_ICacheData,
  /// Gets the cache for the given cache level, an [`SAVMC_ICacheData`]
  pub get_cache_ptr:
    extern "C" fn(*mut c_void, section: u64, level: CacheLevel) -> SAVMC_ICacheData,

  /// Fully free the [`IBytecodeResolver`]
  pub free: extern "C" fn(*mut c_void),

  /// Clear the temporary allocated data for the current callback
  pub clear_allocated: extern "C" fn(*mut c_void),
}

unsafe impl Send for SAVMC_IBytecodeResolver {}
unsafe impl Sync for SAVMC_IBytecodeResolver {}

impl Drop for SAVMC_IBytecodeResolver {
  fn drop(&mut self) {
    (self.free)(self.state)
  }
}

fn map_cache(ccache: ICacheData) -> CacheData {
  match ccache {
    ICacheData::None => CacheData::None,
    ICacheData::Pickle {
      pickle,
      jmps,
      libcalls,
    } => {
      let jumps = jmps.to_slice().iter().copied().collect::<_>();

      let libcalls = match libcalls {
        SAVMC_Maybe::Some(x) => Some(x.to_slice().iter().copied().collect::<_>()),
        _ => None,
      };

      CacheData::Pickle {
        out: Arc::from(pickle.to_slice()),
        jumps: jumps,
        libcalls,
      }
    }
    ICacheData::JITCache {
      level,
      binary,
      relocs,
    } => CacheData::JITCache {
      level,
      binary: Arc::from(binary.to_slice()),
      reloc: Arc::from(relocs.to_slice()),
    },
  }
}

impl BytecodeResolver for SAVMC_IBytecodeResolver {
  type T<'a> = SAVMC_IResolveData;

  fn rodata(&self) -> &[u8] {
    let out = unsafe { (self.get_rodata)(self.state).to_slice_raw() };

    (self.clear_allocated)(self.state);

    out
  }

  fn rwdata(&self) -> &mut [u8] {
    let out = unsafe { (self.get_rwdata)(self.state).to_slice_mut() };

    (self.clear_allocated)(self.state);

    out
  }

  fn last_section_id(&self) -> u64 {
    let output = (self.last_section_id_ptr)(self.state);

    (self.clear_allocated)(self.state);

    output
  }

  fn learn_data(&self, section: u64) -> savm::SymbolMapTableInfo {
    let output = (self.learn_data_ptr)(self.state, section);

    (self.clear_allocated)(self.state);

    output
  }

  fn resolve_data(&self, section: u64) -> savm::SymbolMapTable<SAVMC_IResolveData> {
    let output = match (self.resolve_data_ptr)(self.state, section) {
      ISymbolMapTable::NativePointer { fnptr, cdecl } => SymbolMapTable::NativePointer {
        fnptr,
        cdecl: postcard::from_bytes(cdecl.to_slice()).expect("Unable to parse as CDECL"),
      },
      ISymbolMapTable::MixedSizedBytecode_TypeSlice { bytecode } => {
        SymbolMapTable::MixedSizedBytecode {
          bytecode: SAVMC_IResolveData::usedata(bytecode),
        }
      }
      ISymbolMapTable::MixedSizedBytecode_ { bytecode } => {
        SymbolMapTable::MixedSizedBytecode { bytecode: bytecode }
      }
    };

    (self.clear_allocated)(self.state);

    output
  }

  fn update_cache(&self, section: u64, cache: CacheData) {
    let cache = match &cache {
      CacheData::None => ICacheData::None,
      CacheData::JITCache {
        level,
        binary,
        reloc,
      } => ICacheData::JITCache {
        level: *level,
        binary: SAVMC_ISlice_Impl {
          data: binary.as_ptr(),
          len: binary.len(),
        },
        relocs: SAVMC_ISlice_Impl {
          data: reloc.as_ptr(),
          len: reloc.len(),
        },
      },
      CacheData::Pickle {
        out,
        jumps,
        libcalls,
      } => {
        let libcalls = libcalls.as_ref().map_or(SAVMC_Maybe::None, |x| {
          SAVMC_Maybe::Some(SAVMC_ISlice_Impl {
            data: x.as_ptr(),
            len: x.len(),
          })
        });
        let jmps = SAVMC_ISlice_Impl {
          data: jumps.as_ptr(),
          len: jumps.len(),
        };

        ICacheData::Pickle {
          pickle: SAVMC_ISlice_Impl {
            data: out.as_ptr(),
            len: out.len(),
          },
          libcalls,
          jmps,
        }
      }
    };

    (self.update_cache_ptr)(self.state, section, cache);

    (self.clear_allocated)(self.state);
  }

  fn get_libcalls(&self, section: u64) -> Option<Arc<[u64]>> {
    let sol = match (self.get_libcalls_ptr)(self.state, section) {
      SAVMC_Maybe::Some(dat) => Some(dat.collect::<_>()),
      _ => None,
    };

    (self.clear_allocated)(self.state);

    sol
  }

  fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData {
    let out = map_cache((self.get_cache_ptr)(self.state, section, level));

    (self.clear_allocated)(self.state);

    out
  }

  fn heuristic_pgo(&self) -> [&[u64]; 2] {
    let pgo = (self.heuristic_pgo_ptr)(self.state);

    let output = unsafe { [pgo.id0.to_slice_raw(), pgo.id1.to_slice_raw()] };

    (self.clear_allocated)(self.state);

    output
  }

  fn get_best_cache(&self, section: u64) -> savm::CacheData {
    let out = map_cache((self.get_best_cache_ptr)(self.state, section));

    (self.clear_allocated)(self.state);

    out
  }
}
