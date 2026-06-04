// Arithmatic Logic Memory Unit

use std::mem::offset_of;

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::{
      OffsetBytes, offsetload,
      reg::{
        LLVMTypeMapping, LLVMTypeOrWidth, llvmresolve_location_src_load,
        llvmresolve_location_src_store,
      },
    },
    ssaupdater::{REG_R1, REG_R5},
  },
  pickle::{
    def::PickleInstruction,
    reader::au::{ARITH, DIVLIKE, parse_arith, parse_divlike},
  },
};
use llvm_sys::{
  LLVMOpcode::LLVMInsertElement,
  core::{
    LLVMBuildAShr, LLVMBuildAdd, LLVMBuildBitCast, LLVMBuildCall2, LLVMBuildExtractValue,
    LLVMBuildInsertElement, LLVMBuildLShr, LLVMBuildMul, LLVMBuildSDiv, LLVMBuildSExt,
    LLVMBuildSRem, LLVMBuildSub, LLVMBuildTrunc, LLVMBuildUDiv, LLVMBuildURem, LLVMBuildZExt,
    LLVMBuildZExtOrBitCast, LLVMConstInt, LLVMConstVector, LLVMGetIntrinsicDeclaration,
    LLVMGetUndef, LLVMGlobalGetValueType, LLVMIntTypeInContext, LLVMLookupIntrinsicID,
    LLVMVectorType,
  },
  prelude::LLVMValueRef,
};
pub mod fp;
pub mod vfma;
pub use vfma::*;
pub mod vsh;
pub use fp::*;
use sart::ctr::VMTaskState;
pub use vsh::*;

#[macro_export]
macro_rules! llvmreadws {
  ($meta:expr, start = $start:expr, stop = $stop:expr, $t:ty) => {
    <$t>::from_ne_bytes($meta.ws[$start..$stop].try_into().unwrap())
  };
}

#[inline(always)]
pub fn handle_mov(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let source = pickle.u1;
  let target = pickle.u2;

  let meta_ptr = meta as *mut CompilerMeta;

  unsafe {
    if source == target && source > 7 {
      let val = match source {
        12 => offsetload(
          meta.builder,
          meta.llvmctx,
          meta.i64,
          meta.vmctx,
          OffsetBytes::U(offset_of!(VMTaskState, largepad) as _),
        ),
        _ => unreachable!(),
      };

      (*meta_ptr).regmnt.setreg(REG_R1, val, meta_ptr);
    } else {
      let src = (*meta_ptr).regmnt.usereg(source as _, meta_ptr);

      (*meta_ptr).regmnt.setreg(target as _, src, meta_ptr);
    }
  }
}

#[inline(always)]
fn divlikeop<F>(pickle: &PickleInstruction, meta: &mut CompilerMeta, call: F)
where
  F: FnOnce(&mut CompilerMeta, &LLVMTypeMapping, LLVMValueRef, LLVMValueRef) -> LLVMValueRef,
{
  let DIVLIKE {
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_divlike(pickle, meta.ws.as_ref());

  let datatype = LLVMTypeOrWidth::Type(datatype);
  let typdata = datatype.r#type();

  let src1 = llvmresolve_location_src_load(meta, datatype, src1, None, of_src1, 1);
  let src2 = llvmresolve_location_src_load(meta, datatype, src2, None, of_src2, 1);
  let target = llvmresolve_location_src_store(meta, datatype, tgt, None, of_tgt as _, 1);

  let output = call(meta, &typdata, src1, src2);

  target.synchronize(meta, output);
}

pub fn handle_div(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  divlikeop(pickle, meta, |meta, r#type, src1, src2| unsafe {
    if r#type.signed {
      LLVMBuildSDiv(meta.builder, src1, src2, LLVM_VAR_NAME.0)
    } else {
      LLVMBuildUDiv(meta.builder, src1, src2, LLVM_VAR_NAME.0)
    }
  });
}

pub fn handle_rem(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  divlikeop(pickle, meta, |meta, r#type, src1, src2| unsafe {
    if r#type.signed {
      LLVMBuildSRem(meta.builder, src1, src2, LLVM_VAR_NAME.0)
    } else {
      LLVMBuildURem(meta.builder, src1, src2, LLVM_VAR_NAME.0)
    }
  });
}

#[inline(always)]
fn arithlikeop<E, F>(pickle: &PickleInstruction, meta: &mut CompilerMeta, multipler: E, call: F)
where
  // Remains useful for things like, mul_wide
  E: FnOnce(u16) -> u32,
  F: FnOnce(
    &mut CompilerMeta,
    u16,
    &LLVMTypeMapping,
    u32,
    LLVMValueRef,
    LLVMValueRef,
  ) -> LLVMValueRef,
{
  let ARITH {
    datatype,
    count,
    instdefined,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_arith(meta.ws.as_ref());

  let datatype = LLVMTypeOrWidth::Type(datatype);
  let typdata = datatype.r#type();

  let src1 = llvmresolve_location_src_load(meta, datatype, src1, None, of_src1, count as _);
  let src2 = llvmresolve_location_src_load(meta, datatype, src2, None, of_src2, count as _);
  let target = llvmresolve_location_src_store(
    meta,
    datatype,
    tgt,
    None,
    of_tgt as _,
    (multipler(instdefined) * count) as _,
  );

  let output = call(meta, instdefined, &typdata, count, src1, src2);

  target.synchronize(meta, output);
}

pub fn handle_add(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  arithlikeop(
    pickle,
    meta,
    |_| 1,
    |meta, instdefined, r#type, count, src1, src2| unsafe {
      // [<Carry/Sigflow bit>] [<saturation bit>] [Padding (14bits)] (16b)
      let carry = (instdefined >> 15) == 1; // gets the last bit
      let saturate = (instdefined >> 14 & 0b01) == 1; // gets the saturation bit

      debug_assert!(!(carry && saturate));

      if carry {
        let overflow = if r#type.signed {
          "llvm.sadd.with.overflow"
        } else {
          "llvm.uadd.with.overflow"
        };

        let valvect = meta.call_intrinsic(overflow, &mut [r#type.x1], &mut [src1, src2]);

        let sum = LLVMBuildExtractValue(meta.builder, valvect, 0, LLVM_VAR_NAME.0);
        let oflow = {
          let v = LLVMBuildExtractValue(meta.builder, valvect, 1, LLVM_VAR_NAME.0);

          LLVMBuildZExtOrBitCast(meta.builder, v, meta.i64, LLVM_VAR_NAME.0)
        };
        let meta_ptr = meta as *mut CompilerMeta;
        (*meta_ptr).regmnt.setreg(REG_R5, oflow, meta_ptr);

        sum
      } else if saturate {
        let saturate = if r#type.signed {
          "llvm.sadd.sat"
        } else {
          "llvm.uadd.sat"
        };

        let mut typearg = if count == 1 {
          [r#type.x1]
        } else {
          [LLVMVectorType(r#type.x1, count)]
        };

        meta.call_intrinsic(saturate, &mut typearg, &mut [src1, src2])
      } else {
        LLVMBuildAdd(meta.builder, src1, src2, LLVM_VAR_NAME.0)
      }
    },
  );
}

pub fn handle_sub(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  arithlikeop(
    pickle,
    meta,
    |_| 1,
    |meta, instdefined, r#type, count, src1, src2| unsafe {
      // [<borrow bit>] [<saturation bit>] [Padding (14bits)] (16b)
      let borrow = (instdefined >> 15) == 1; // gets the last bit
      let saturate = (instdefined >> 14 & 0b01) == 1; // gets the saturation bit

      debug_assert!(!(borrow && saturate));
      debug_assert!(count != 0);
      debug_assert!(!(borrow && count != 1));

      if borrow {
        let saturate = if r#type.signed {
          "llvm.ssub.with.overflow"
        } else {
          "llvm.usub.with.overflow"
        };
        let mut typearg = if count == 1 {
          [r#type.x1]
        } else {
          [LLVMVectorType(r#type.x1, count)]
        };

        let valvect = meta.call_intrinsic(saturate, &mut typearg, &mut [src1, src2]);

        let sum = LLVMBuildExtractValue(meta.builder, valvect, 0, LLVM_VAR_NAME.0);
        let oflow = {
          let v = LLVMBuildExtractValue(meta.builder, valvect, 1, LLVM_VAR_NAME.0);

          LLVMBuildZExtOrBitCast(meta.builder, v, meta.i64, LLVM_VAR_NAME.0)
        };
        let meta_ptr = meta as *mut CompilerMeta;
        (*meta_ptr).regmnt.setreg(REG_R5, oflow, meta_ptr);

        sum
      } else if saturate {
        let saturate = if r#type.signed {
          "llvm.ssub.sat"
        } else {
          "llvm.usub.sat"
        };

        let mut typearg = if count == 1 {
          [r#type.x1]
        } else {
          [LLVMVectorType(r#type.x1, count)]
        };

        meta.call_intrinsic(saturate, &mut typearg, &mut [src1, src2])
      } else {
        LLVMBuildSub(meta.builder, src1, src2, LLVM_VAR_NAME.0)
      }
    },
  );
}

pub fn handle_mul(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  arithlikeop(
    pickle,
    meta,
    |instdefined| {
      let eflags = (instdefined >> 14) as u8;

      let wide = (eflags & 0x03) == 1;
      if wide { 2 } else { 1 }
    },
    |meta, instdefined, r#type, count, src1, src2| unsafe {
      let eflags = (instdefined >> 14) as u8;

      let wide = (eflags & 0x03) == 1;
      let lowbits = (eflags & 0x01) == 0;

      if wide || !lowbits {
        let bit_width = r#type.width * 8;
        let wide_elem_type = LLVMIntTypeInContext(meta.llvmctx, (bit_width * 2) as _);
        let wide_vector_type = if count == 1 {
          wide_elem_type
        } else {
          LLVMVectorType(wide_elem_type, count)
        };

        let (w_src1, w_src2) = if r#type.signed {
          (
            LLVMBuildSExt(meta.builder, src1, wide_vector_type, LLVM_VAR_NAME.0),
            LLVMBuildSExt(meta.builder, src2, wide_vector_type, LLVM_VAR_NAME.0),
          )
        } else {
          (
            LLVMBuildZExt(meta.builder, src1, wide_vector_type, LLVM_VAR_NAME.0),
            LLVMBuildZExt(meta.builder, src2, wide_vector_type, LLVM_VAR_NAME.0),
          )
        };

        let wide_mul = LLVMBuildMul(meta.builder, w_src1, w_src2, LLVM_VAR_NAME.0);

        if !lowbits {
          let shift_val = LLVMConstInt(wide_elem_type, bit_width as u64, 0);
          let shift_vector = if count == 1 {
            shift_val
          } else {
            LLVMConstVector(vec![shift_val; count as usize].as_mut_ptr(), count)
          };

          let shifted = if r#type.signed {
            LLVMBuildAShr(meta.builder, wide_mul, shift_vector, LLVM_VAR_NAME.0)
          } else {
            LLVMBuildLShr(meta.builder, wide_mul, shift_vector, LLVM_VAR_NAME.0)
          };

          let original_vector_type = if count == 1 {
            r#type.x1
          } else {
            LLVMVectorType(r#type.x1, count)
          };

          return LLVMBuildTrunc(meta.builder, shifted, original_vector_type, LLVM_VAR_NAME.0);
        } else {
          let target = if count == 1 {
            LLVMVectorType(r#type.x1, 2)
          } else {
            LLVMVectorType(r#type.x1, 2 * count)
          };

          return LLVMBuildBitCast(meta.builder, wide_mul, target, LLVM_VAR_NAME.0);
        }
      }

      // Low Bits, Wrapping
      LLVMBuildMul(meta.builder, src1, src2, LLVM_VAR_NAME.0)
    },
  );
}
