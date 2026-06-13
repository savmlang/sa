cprelude::cprelude! {
  SAVMC
}

pub mod resolvedata;
pub mod vm;

use crate::resolvedata::IResolveData;
use cprelude::Slicable;
pub use savm::CacheLevel;
use savm::{
  acaot::{pickle::def::PickleInstruction, JITReloc},
  ahash, BytecodeResolver, CacheData, ResolvedData, SymbolMapTable, SymbolMapTableInfo,
};
use std::{io::Cursor, os::raw::c_void, sync::Arc};

#[repr(C)]
#[allow(non_camel_case_types)]
pub enum ISymbolMapTable {
  NativePointer {
    fnptr: *const (),
    cdecl: SAVMC_ISlice,
  },
  MixedSizedBytecode_TypeSlice {
    bytecode: SAVMC_ISlice,
  },
  MixedSizedBytecode_ {
    bytecode: IResolveData,
  },
}

#[repr(C)]
pub struct PGOData {
  pub id0: SAVMC_ISlice_Impl<u64>,
  pub id1: SAVMC_ISlice_Impl<u64>,
}

#[repr(C)]
pub enum ICacheData {
  None,
  Pickle {
    pickle: SAVMC_ISlice_Impl<PickleInstruction>,

    meta: SAVMC_ISlice,
  },
  JITCache {
    level: CacheLevel,

    binary: SAVMC_ISlice,
    payload: SAVMC_ISlice_Impl<JITReloc>,
  },
}

#[repr(C)]
pub struct IBytecodeResolver {
  pub state: *mut c_void,

  pub last_section_id_ptr: extern "C" fn(*mut c_void) -> u64,
  pub resolve_data_ptr: extern "C" fn(*mut c_void, section: u64) -> ISymbolMapTable,
  pub update_cache_ptr: extern "C" fn(*mut c_void, section: u64, cache: ICacheData),
  pub learn_data_ptr: extern "C" fn(*mut c_void, section: u64) -> SymbolMapTableInfo,
  pub heuristic_pgo_ptr: extern "C" fn(*mut c_void) -> PGOData,
  pub get_licalls_ptr:
    extern "C" fn(*mut c_void, section: u64) -> SAVMC_Maybe<SAVMC_ISlice_Impl<u64>>,
  pub get_best_cache_ptr: extern "C" fn(*mut c_void, section: u64) -> ICacheData,
  pub get_cache_ptr: extern "C" fn(*mut c_void, section: u64, level: CacheLevel) -> ICacheData,
  pub free: extern "C" fn(*mut c_void),

  pub clear_allocated: extern "C" fn(*mut c_void),
}

unsafe impl Send for IBytecodeResolver {}
unsafe impl Sync for IBytecodeResolver {}

impl Drop for IBytecodeResolver {
  fn drop(&mut self) {
    (self.free)(self.state)
  }
}

type InternalSerializable = (savm::ahash::HashSet<u64>, savm::ahash::HashMap<u64, usize>);

fn map_cache(ccache: ICacheData) -> CacheData {
  match ccache {
    ICacheData::None => CacheData::None,
    ICacheData::Pickle { pickle, meta } => {
      let data: InternalSerializable = postcard::from_bytes(meta.to_slice())
        .expect("Please ensure you are using a valid serializable meta");

      CacheData::Pickle {
        out: Arc::from(pickle.to_slice()),
        jumps: Arc::new(data.1),
        libcalls: None,
      }
    }
    ICacheData::JITCache {
      level,
      binary,
      payload,
    } => CacheData::JITCache {
      level,
      binary: Arc::from(binary.to_slice()),
      reloc: Arc::from(payload.to_slice()),
    },
  }
}

impl BytecodeResolver for IBytecodeResolver {
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

  fn resolve_data(&self, section: u64) -> savm::SymbolMapTable<Box<dyn savm::ResolvedData>> {
    let output = match (self.resolve_data_ptr)(self.state, section) {
      ISymbolMapTable::NativePointer { fnptr, cdecl } => SymbolMapTable::NativePointer {
        fnptr,
        cdecl: postcard::from_bytes(cdecl.to_slice()).expect("Unable to parse as CDECL"),
      },
      ISymbolMapTable::MixedSizedBytecode_TypeSlice { bytecode } => {
        SymbolMapTable::MixedSizedBytecode {
          bytecode: Box::new(Cursor::new(Box::from(bytecode.to_slice()) as Box<[u8]>))
            as Box<dyn ResolvedData>,
        }
      }
      ISymbolMapTable::MixedSizedBytecode_ { bytecode } => SymbolMapTable::MixedSizedBytecode {
        bytecode: Box::new(bytecode) as Box<dyn ResolvedData>,
      },
    };

    (self.clear_allocated)(self.state);

    output
  }

  fn update_cache(&self, section: u64, cache: CacheData) {
    let mut alc = None;
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
        payload: SAVMC_ISlice_Impl {
          data: reloc.as_ptr(),
          len: reloc.len(),
        },
      },
      CacheData::Pickle {
        out,
        jumps,
        libcalls,
      } => ICacheData::Pickle {
        pickle: SAVMC_ISlice_Impl {
          data: out.as_ptr(),
          len: out.len(),
        },
        meta: {
          let parse = postcard::to_allocvec(&(
            &libcalls.as_ref().unwrap() as &ahash::HashSet<u64>,
            jumps.as_ref() as &ahash::HashMap<u64, usize>,
          ))
          .expect("Unable to parse");
          alc = Some(parse);

          SAVMC_ISlice_Impl {
            data: alc.as_ref().unwrap().as_ptr(),
            len: alc.as_ref().unwrap().len(),
          }
        },
      },
    };

    (self.update_cache_ptr)(self.state, section, cache);

    (self.clear_allocated)(self.state);

    drop(alc);
  }

  fn get_libcalls(&self, section: u64) -> Option<Arc<ahash::HashSet<u64>>> {
    let sol = match (self.get_licalls_ptr)(self.state, section) {
      SAVMC_Maybe::Some(dat) => Some(Arc::new(dat.to_slice().iter().map(|x| *x).collect())),
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
