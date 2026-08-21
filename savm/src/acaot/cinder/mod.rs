//! ## ACAoT Cinder
//! A copy-patch JIT compiler designed to be faster than the current
//! tier 1 compiler (Cranelift)

use core::slice;
use std::{ffi::c_void, fmt::Debug, iter, marker::PhantomData, sync::Arc};

use crate::{
  BytecodeResolver, CacheData,
  acaot::{
    native::NativeCompiler,
    pickle::{
      def::{CRTFn, PickleInstruction, c_pickle_generate_table},
      implementation::WorkingSet,
    },
  },
  kvwrap::SaVMJumpWrapRef,
};

#[allow(nonstandard_style)]
pub(crate) mod emit {
  include!(concat!(env!("OUT_DIR"), "/cinderjit.rs"));
}

use ahash::{HashMap, HashMapExt};
use emit::Stencil;
use indexmap::{IndexSet, set::MutableValues};
use sart::ctr::VMTaskState;

mod emitter;

#[derive(Copy, Clone)]
pub struct StencilMap {
  pub stencil: &'static Stencil,
  pub resolve: StencilVec<(&'static str, Resolved)>,
}

impl Debug for StencilMap {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("StencilMap")
      .field("stencil", &self.stencil.name)
      .field("resolve", &self.resolve)
      .finish()
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Resolved {
  /// Array of markers arranged in ascending order
  MarkersArray {
    idx: usize,
  },
  /// The next stencil subid
  NextStencil,
  /// The next stencil sector mainid
  NextMainID,
  /// An immediate value
  Immediate {
    imm: u64,
  },
  /// An entry from the WS
  WorkingSetId {
    idx: usize,
  },
  ResolveLaterStencilID {
    marker: u64,
  },
  /// An absolutely located StencilID
  StencilId {
    mainid: usize,
    subid: usize,
  },
}

#[repr(C)]
pub struct DispatchStarter {
  pub hotness_or_resume: u64,
  pub ws: *mut c_void,
  pub pickle: *const PickleInstruction,
  pub taskstate: *mut VMTaskState,
  pub wsarr: extern "C" fn(*mut c_void, *const u8, usize),
}

pub extern "C" fn setws(ws: *mut c_void, data: *const u8, len: usize) {
  unsafe {
    (*(ws as *mut WorkingSet)).arr = slice::from_raw_parts(data, len);
  }
}

pub struct ACAoTCinder<T: BytecodeResolver + Send + Sync + 'static>(PhantomData<T>);

impl<T: BytecodeResolver + Send + Sync + 'static> ACAoTCinder<T> {
  pub const C_DISPATCH: &'static [CRTFn] = &c_pickle_generate_table::<T>();

  pub fn create<const SENDBACK: bool>() -> Box<dyn NativeCompiler<SENDBACK>> {
    Box::new(Self(PhantomData))
  }
}

pub const INST_RETURN_P_ID: Resolved = Resolved::StencilId {
  mainid: 0,
  subid: 2,
};

impl<const SBK: bool, T: BytecodeResolver + Send + Sync + 'static> NativeCompiler<SBK>
  for ACAoTCinder<T>
{
  fn compile<'a>(
    &'a mut self,
    pickle: &'a [super::pickle::def::PickleInstruction],
    jumps: crate::kvwrap::SaVMJumpWrapRef,
  ) -> crate::CacheData {
    let mut mapping: Vec<StencilVec<StencilMap>> = Vec::with_capacity(pickle.len() + 1);
    let mut entries: HashMap<u64, u64> = HashMap::with_capacity(jumps.0.len());
    let mut entrymap: IndexSet<Box<[u8]>> = IndexSet::with_capacity(pickle.len().div_ceil(4));

    // Append prelude and return
    {
      let (idx, _) = entrymap.insert_full(Box::new([]));
      assert!(idx == 0);

      let inst = &[
        StencilMap {
          stencil: &emit::inst_fireup,
          resolve: stencilify(&[("CALL", Resolved::NextStencil)]),
        },
        StencilMap {
          stencil: &emit::inst_prelude,
          resolve: stencilify(&[
            ("MARKER_FIRST", Resolved::MarkersArray { idx }),
            (
              "MARKERS_TOTAL",
              Resolved::Immediate {
                imm: jumps.0.len() as _,
              },
            ),
            ("NEXT", Resolved::NextMainID),
          ]),
        },
        StencilMap {
          stencil: &emit::inst_return_partial,
          resolve: stencilify(&[]),
        },
      ];
      mapping.push(stencilify(inst));
    }

    let mut meta = CompilerMeta {
      jumps,
      pickle,
      mapping: &mut mapping,
      crt: Self::C_DISPATCH,
      entrymap,
    };
    emitter::emit::<T>(&mut meta, &mut entries);

    if let Some(val) = meta.entrymap.get_index_mut2(0) {
      *val = meta
        .jumps
        .0
        .iter()
        .flat_map(|x| {
          let marker = x.marker;
          let internal = *entries.get(&marker).unwrap();

          pack_marker(marker, internal, 0)
        })
        .collect::<Box<_>>();
    }

    let entrymap = meta.entrymap.into_iter().collect::<_>();
    meta.mapping.push(stencilify(&[StencilMap {
      stencil: &emit::inst_return,
      resolve: stencilify(&[]),
    }]));

    for item in meta.mapping {
      item.iter_mut().for_each(|x| {
        for reloc in x.resolve.iter_mut() {
          if let Resolved::ResolveLaterStencilID { marker } = reloc.1 {
            reloc.1 = Resolved::StencilId {
              mainid: *entries.get(&marker).unwrap() as _,
              subid: 0,
            }
          }
        }
      });
    }

    CacheData::CinderTempCache {
      entrymap,
      binary: Arc::from(mapping),
    }
  }
}

pub const fn pack_marker(marker: u64, internal: u64, loc: usize) -> [u8; 24] {
  let mut buf = [0u8; 24];
  {
    let (a, b) = buf.split_at_mut(8);
    let (b, c) = b.split_at_mut(8);

    a.copy_from_slice(&marker.to_ne_bytes());
    b.copy_from_slice(&internal.to_ne_bytes());
    c.copy_from_slice(&(loc as u64).to_ne_bytes());
  }
  buf
}

pub const fn unpack_marker(data: [u8; 24]) -> (u64, u64, usize) {
  let [&a, &b, &c] = unsafe {
    let [a, b, c] = data.as_chunks_unchecked() else {
      unreachable!()
    };

    [a, b, c]
  };

  (
    u64::from_ne_bytes(a),
    u64::from_ne_bytes(b),
    u64::from_ne_bytes(c) as usize,
  )
}

#[derive(Clone, Copy)]
pub struct StencilVec<T>(pub(crate) [Option<T>; 6]);

impl<T> StencilVec<T> {
  pub fn iter(&self) -> impl Iterator<Item = &T> {
    self.0.iter().filter_map(|x| x.as_ref())
  }

  pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
    self.0.iter_mut().filter_map(|x| x.as_mut())
  }
}

impl<T: Debug> Debug for StencilVec<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_list()
      .entries(self.0.iter().filter_map(|x| x.as_ref()))
      .finish()
  }
}

pub struct CompilerMeta<'a> {
  pub pickle: &'a [PickleInstruction],
  pub jumps: SaVMJumpWrapRef<'a>,
  pub mapping: &'a mut Vec<StencilVec<StencilMap>>,

  pub crt: &'static [CRTFn],

  pub entrymap: IndexSet<Box<[u8]>>,
}

fn stencilify<T: Copy>(data: &[T]) -> StencilVec<T> {
  assert!(data.len() <= 6);
  let mut src = data
    .iter()
    .map(|&x| Some(x))
    .chain(iter::repeat_with(|| None));

  StencilVec([
    unsafe { src.next().unwrap_unchecked() },
    unsafe { src.next().unwrap_unchecked() },
    unsafe { src.next().unwrap_unchecked() },
    unsafe { src.next().unwrap_unchecked() },
    unsafe { src.next().unwrap_unchecked() },
    unsafe { src.next().unwrap_unchecked() },
  ])
}
