use std::sync::atomic::Ordering;

use llvm_sys::{
  LLVMAtomicOrdering, LLVMAtomicRMWBinOp,
  core::{
    LLVMBuildAtomicCmpXchg, LLVMBuildAtomicRMW, LLVMBuildExtractValue, LLVMBuildInsertElement,
    LLVMBuildSExt, LLVMConstInt, LLVMGetUndef, LLVMSetOrdering, LLVMSetWeak, LLVMTypeOf,
    LLVMVectorType,
  },
};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::{
      OffsetBytes, offsetload, offsetstore,
      reg::{LLVMTypeOrWidth, llvmresolve_location_src_load, llvmresolve_location_src_store},
    },
  },
  pickle::{
    def::PickleInstruction,
    reader::{ATOMIC, ATOMICRmwOp, parse_atomic},
  },
};

fn ordmap(ord: Ordering) -> LLVMAtomicOrdering {
  match ord {
    Ordering::Relaxed => LLVMAtomicOrdering::LLVMAtomicOrderingUnordered,
    Ordering::AcqRel => LLVMAtomicOrdering::LLVMAtomicOrderingAcquireRelease,
    Ordering::Acquire => LLVMAtomicOrdering::LLVMAtomicOrderingAcquire,
    Ordering::Release => LLVMAtomicOrdering::LLVMAtomicOrderingRelease,
    Ordering::SeqCst => LLVMAtomicOrdering::LLVMAtomicOrderingSequentiallyConsistent,
    _ => unimplemented!(),
  }
}

pub fn handle_atomic(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  match parse_atomic(pickle, &meta.ws) {
    ATOMIC::LOAD {
      typedata,
      ptr_loc,
      ptr_loc_of,
      load_loc,
      load_loc_of,
      ord,
    } => {
      let typ = LLVMTypeOrWidth::Type(typedata);

      let pointer = llvmresolve_location_src_load(meta, typ, ptr_loc, None, ptr_loc_of as _, 1);

      let loaded = offsetload(
        meta.builder,
        meta.llvmctx,
        typ.r#type().x1,
        pointer,
        OffsetBytes::U(0),
      );

      unsafe {
        LLVMSetOrdering(loaded, ordmap(ord));
      }

      let load = llvmresolve_location_src_store(meta, typ, load_loc, None, load_loc_of as _, 1);
      load.synchronize(meta, loaded);
    }
    ATOMIC::STORE {
      typedata,
      ptr_loc,
      ptr_loc_of,
      val_stored_loc,
      val_store_of,
      ord,
    } => {
      let typ = LLVMTypeOrWidth::Type(typedata);

      let pointer = llvmresolve_location_src_load(meta, typ, ptr_loc, None, ptr_loc_of as _, 1);

      let to_store =
        llvmresolve_location_src_load(meta, typ, val_stored_loc, None, val_store_of as _, 1);

      let store = offsetstore(
        meta.builder,
        meta.llvmctx,
        to_store,
        pointer,
        OffsetBytes::U(0),
      );

      unsafe {
        LLVMSetOrdering(store, ordmap(ord));
      }
    }
    ATOMIC::RMW {
      typedata,

      ptr_loc,
      ptr_loc_of,

      load_loc,
      load_loc_of,

      rhs_loc,
      rhs_loc_of,

      op,
      ord,
    } => {
      let typ = LLVMTypeOrWidth::Type(typedata);

      let signed = typ.r#type().signed;

      let pointer = llvmresolve_location_src_load(meta, typ, ptr_loc, None, ptr_loc_of as _, 1);
      let rhs_value = llvmresolve_location_src_load(meta, typ, rhs_loc, None, rhs_loc_of as _, 1);

      let load = llvmresolve_location_src_store(meta, typ, load_loc, None, load_loc_of as _, 1);

      let op = match op {
        ATOMICRmwOp::Add => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAdd,
        ATOMICRmwOp::Min => {
          if signed {
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMin
          } else {
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMin
          }
        }
        ATOMICRmwOp::Max => {
          if signed {
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpMax
          } else {
            LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpUMax
          }
        }
        ATOMICRmwOp::And => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpAnd,
        ATOMICRmwOp::Nand => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpNand,
        ATOMICRmwOp::Or => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpOr,
        ATOMICRmwOp::Sub => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpSub,
        ATOMICRmwOp::Xchg => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXchg,
        ATOMICRmwOp::Xor => LLVMAtomicRMWBinOp::LLVMAtomicRMWBinOpXor,
      };

      unsafe {
        let vect = LLVMBuildAtomicRMW(meta.builder, op, pointer, rhs_value, ordmap(ord), 0);

        load.synchronize(meta, vect);
      };
    }
    ATOMIC::CAS {
      typedata,

      ptr_loc,
      ptr_loc_of,

      val_stored_loc,
      val_store_of,

      expected_loc,
      expected_of,

      ret_loc,
      ret_of,

      ord_success,
      ord_failure,
    } => {
      let typ = LLVMTypeOrWidth::Type(typedata);

      let pointer = llvmresolve_location_src_load(meta, typ, ptr_loc, None, ptr_loc_of as _, 1);

      let val_stored =
        llvmresolve_location_src_load(meta, typ, val_stored_loc, None, val_store_of as _, 1);
      let expected =
        llvmresolve_location_src_load(meta, typ, expected_loc, None, expected_of as _, 1);

      let ret = llvmresolve_location_src_store(meta, typ, ret_loc, None, ret_of as _, 2);

      unsafe {
        let cas_inst = LLVMBuildAtomicCmpXchg(
          meta.builder,
          pointer,
          expected,
          val_stored,
          ordmap(ord_success),
          ordmap(ord_failure),
          0,
        );

        LLVMSetWeak(cas_inst, 1);

        // We need to convert to 2x type vector
        let old_val = LLVMBuildExtractValue(meta.builder, cas_inst, 0, LLVM_VAR_NAME.0);
        let success_i1 = LLVMBuildExtractValue(meta.builder, cas_inst, 1, LLVM_VAR_NAME.0);

        let success_as_t = LLVMBuildSExt(
          meta.builder,
          success_i1,
          LLVMTypeOf(old_val),
          LLVM_VAR_NAME.0,
        );

        let mut final_vec = LLVMGetUndef(LLVMVectorType(LLVMTypeOf(old_val), 2));
        let idx0 = LLVMConstInt(meta.i32, 0, 0);
        let idx1 = LLVMConstInt(meta.i32, 1, 0);

        final_vec = LLVMBuildInsertElement(meta.builder, final_vec, old_val, idx0, LLVM_VAR_NAME.0);
        final_vec =
          LLVMBuildInsertElement(meta.builder, final_vec, success_as_t, idx1, LLVM_VAR_NAME.0);

        ret.synchronize(meta, final_vec);
      }
    }
  }
}
