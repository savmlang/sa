use std::{
  ffi::c_void,
  iter,
  mem::{transmute, zeroed},
  ptr::{self, addr_of_mut, null_mut},
};

use sart::structures::ffi::{
  LFFITypeMap, VReg,
  libffi_sys::{
    FFI_TYPE_STRUCT, ffi_abi_FFI_DEFAULT_ABI, ffi_call, ffi_cif, ffi_prep_cif, ffi_type,
    ffi_type_uint8, ffi_type_uint16, ffi_type_uint32, ffi_type_uint64, ffi_type_void,
  },
};
use sart::{
  ctr::{CVMTaskState, VMTaskState},
  structures::ffi::{COut, CallSig},
};

use crate::{
  CODE_CACHE, FNCALL_DISPATCH, SymbolMapTable, SymbolMapTableInfo, ThreadSafe, VM,
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};

mod spawn;
pub use spawn::*;

struct NativeAsyncExecutor;

// impl NativeAsyncExecutor {
//   pub fn run(task: impl Future<Output = ()>) {}
// }

thread_local! {
  pub static EXEC: NativeAsyncExecutor = NativeAsyncExecutor;
}

pub extern "C" fn ffi_synccall_sectionid(taskstate: *mut VMTaskState, sectionid: u64) {
  unsafe {
    let vm = (*taskstate).engine_or_pt.pt as *const _ as *const VM;

    let [r7, r8] = (*vm).fncall(sectionid, taskstate);

    (*taskstate).r7 = r7;
    (*taskstate).r8 = r8;
  }
}

pub extern "C" fn ffi_libcall_sectionid(taskstate: *mut VMTaskState, sectionid: u64) {
  unsafe {
    let vm = (*taskstate).engine_or_pt.pt as *const _ as *const VM;

    let (v, cdecl) = FNCALL_DISPATCH.get().unwrap().get(&sectionid).unwrap();
    run_cdecl(v.0, cdecl, taskstate)
  }
}

pub fn call_synccall(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let sectionid = arrcastint!(ws, start = 0, stop = 8, u64);

  unsafe {
    let vm = taskstate.engine_or_pt.pt as *const _ as *const VM;

    let tskptr = taskstate as *mut _;

    let mut dispatch = || {
      let [r7, r8] = (*vm).fncall(sectionid, taskstate);

      (*taskstate).r7 = r7;
      (*taskstate).r8 = r8;
    };

    if CODE_CACHE.contains_key(&sectionid) {
      return dispatch();
    }

    return match (*vm).resolve.as_ref().learn_data(sectionid) {
      SymbolMapTableInfo::MixedSizedBytecode => dispatch(),
      SymbolMapTableInfo::NativePointer => {
        FNCALL_DISPATCH.get()
          .map_or_else(|| {
            match (*vm).resolve.as_ref().resolve_data(sectionid) {
              SymbolMapTable::NativePointer { fnptr, cdecl } => {
                run_cdecl(fnptr, &cdecl, tskptr)
              },
              _ => unreachable!()
            }
          }, |x| {
            let (fnptr, cdecl) = x.get(&sectionid).expect("SaVM Error - Uncached Library Call in cached area. This is a error with SaVM and no amount of bytecode patching can rectify it");

            run_cdecl(fnptr.0, cdecl, tskptr)
          });
      }
    };
  }
}

static mut BITS128_ELEMENTS: [*mut ffi_type; 3] = unsafe {
  [
    addr_of_mut!(ffi_type_uint64),
    addr_of_mut!(ffi_type_uint64),
    null_mut(),
  ]
};

static FFI_TYPE_BITS128: ThreadSafe<ffi_type> = ThreadSafe(ffi_type {
  size: 0,      // libffi fills this
  alignment: 0, // libffi fills this
  type_: FFI_TYPE_STRUCT as u16,
  elements: unsafe { &raw mut BITS128_ELEMENTS as *mut _ },
});

fn run_cdecl(fnptr: *const (), cdecl: &CallSig, taskstate: *mut VMTaskState) {
  match cdecl {
    CallSig::SaFFI(_) => unsafe {
      let fcall: extern "C" fn(*mut CVMTaskState) = transmute(fnptr);

      fcall(taskstate as _);
    },
    CallSig::CDef(cdef) => unsafe {
      let mut bits128 = FFI_TYPE_BITS128.0;

      let out_bytes = cdef.out.width();
      let mut output = match cdef.out {
        COut::Void => addr_of_mut!(ffi_type_void),
        COut::Bits8 => addr_of_mut!(ffi_type_uint8),
        COut::Bits16 => addr_of_mut!(ffi_type_uint16),
        COut::Bits32 => addr_of_mut!(ffi_type_uint32),
        COut::Bits64 => addr_of_mut!(ffi_type_uint64),
        COut::Bits128 => addr_of_mut!(bits128),
      };

      let mut lffis: [LFFITypeMap; 32] = zeroed();
      let mut types = [null_mut(); 33];

      cdef
        .inargs
        .iter()
        .zip(lffis.iter_mut())
        .for_each(|(x, slot)| unsafe { x.vtype.as_lffitype(slot) });

      lffis
        .iter_mut()
        .map(|x| &mut x.lffitype as *mut _)
        .chain(iter::once(null_mut()))
        .zip(types.iter_mut())
        .for_each(|(ffi, ty)| {
          *ty = ffi;
        });

      let mut cif = ffi_cif {
        ..Default::default()
      };

      ffi_prep_cif(
        &mut cif,
        ffi_abi_FFI_DEFAULT_ABI,
        cdef.inargs.len() as u32,
        output,
        types.as_mut_ptr(),
      );

      let mut stores = [0u64; 32];
      let mut inargs = [null_mut(); 32];

      cdef
        .inargs
        .iter()
        .map(|mval| {
          let outval = match mval.vreg {
            VReg::R1 => 0,
            VReg::R2 => 1,
            VReg::R3 => 2,
            VReg::R4 => 3,
            VReg::R5 => 4,
            VReg::R6 => 5,
            VReg::R7 => 6,
            VReg::R8 => 7,
            VReg::Scratchpad => 8,
            VReg::Largepad => 9,
            VReg::LoadFromPtrInR2 => 10,
          };

          let ts = &mut *taskstate;
          let locreslv = resolve_location_src!(ts => outval);

          ((*locreslv).u64, mval.regof, mval.vtype)
        })
        .zip(stores.iter_mut())
        .zip(inargs.iter_mut())
        .for_each(|(((storeval, regof, vtype), store), inarg)| {
          *store = storeval;

          let ptr = vtype.ptr(store as *mut _ as _, regof);

          *inarg = ptr;
        });

      let mut ret_fullsize = [0u64; 2];

      ffi_call(
        &mut cif,
        Some(transmute(fnptr)),
        ret_fullsize.as_mut_ptr() as _,
        inargs.as_mut_ptr(),
      );

      let r7 = &raw mut (*taskstate).r7;
      ptr::copy_nonoverlapping(ret_fullsize.as_ptr() as *const u8, r7 as *mut u8, out_bytes);
    },

    //
    CallSig::SaFFIAsyncO(_) | CallSig::SaFFIAsyncQ(_) => {
      unimplemented!("synccall was ran with ASYNC library")
    }
  }
}

pub fn call_asynccall(
  _pickle: &PickleInstruction,
  _ws: &mut WorkingSet,
  _taskstate: &mut VMTaskState,
) {
  unimplemented!("Synccall-asyncall will be implemented later!")
}

pub fn call_task(_pickle: &PickleInstruction, _ws: &mut WorkingSet, _taskstate: &mut VMTaskState) {
  unimplemented!("Task will be implemented later!")
}
