// Arithmatic Logic Memory Unit

use std::{ffi::CStr, mem::offset_of, ops::Deref};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    dispose::LLVMMsg,
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
    reader::{
      Immediate, REG,
      au::{ARITH, DIVLIKE, parse_arith, parse_divlike},
      parse_reg,
      vcmp::{CMPOp, FloatOP, IntOP, VCMP, parse_vcmp},
    },
  },
};
use llvm_sys::{
  LLVMIntPredicate,
  LLVMOpcode::LLVMInsertElement,
  LLVMRealPredicate,
  core::{
    LLVMBuildAShr, LLVMBuildAdd, LLVMBuildBitCast, LLVMBuildCall2, LLVMBuildExtractValue,
    LLVMBuildFCmp, LLVMBuildICmp, LLVMBuildInsertElement, LLVMBuildLShr, LLVMBuildMul,
    LLVMBuildSDiv, LLVMBuildSExt, LLVMBuildSRem, LLVMBuildSub, LLVMBuildTrunc, LLVMBuildUDiv,
    LLVMBuildURem, LLVMBuildZExt, LLVMBuildZExtOrBitCast, LLVMConstInt, LLVMConstVector,
    LLVMGetBasicBlockName, LLVMGetInsertBlock, LLVMGetIntrinsicDeclaration, LLVMGetUndef,
    LLVMGlobalGetValueType, LLVMInt8TypeInContext, LLVMInt16TypeInContext, LLVMIntTypeInContext,
    LLVMLookupIntrinsicID, LLVMTypeOf, LLVMVectorType,
  },
  prelude::LLVMValueRef,
};
use sart::ctr::VMTaskState;

pub mod atomic;
pub mod cast;
pub mod fp;
pub mod vbit;
pub mod vcnt;
pub mod vcopy;
pub mod vfma;
pub mod vfop;
pub mod vsh;

pub use atomic::*;
pub use cast::*;
pub use fp::*;
pub use vbit::*;
pub use vcnt::*;
pub use vcopy::*;
pub use vfma::*;
pub use vfop::*;
pub use vsh::*;

#[macro_export]
macro_rules! llvmreadws {
  ($meta:expr, start = $start:expr, stop = $stop:expr, $t:ty) => {
    <$t>::from_ne_bytes($meta.ws[$start..$stop].try_into().unwrap())
  };
}

pub fn handle_reg(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let REG {
    src,
    offset,
    width,
    immediate,
  } = parse_reg(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Width(width);
  let store = llvmresolve_location_src_store(meta, typ, src, None, offset as _, 1);

  let immediate = unsafe {
    match immediate {
      Immediate::U64(x) => LLVMConstInt(meta.i64, x, 0),
      Immediate::U32(x) => LLVMConstInt(meta.i32, x as _, 0),
      Immediate::U16(x) => LLVMConstInt(LLVMInt16TypeInContext(meta.llvmctx), x as _, 0),
      Immediate::U8(x) => LLVMConstInt(LLVMInt8TypeInContext(meta.llvmctx), x as _, 0),
    }
  };

  store.synchronize(meta, immediate);
}
pub fn handle_vcmp(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VCMP {
    datawdt,
    cmpop,
    count,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_vcmp(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Width(datawdt);
  let r#type = typ.r#type();

  let src1 = llvmresolve_location_src_load(meta, typ, src1, None, of_src1, count);
  let src2 = llvmresolve_location_src_load(meta, typ, src2, None, of_src2, count);
  let tgt = llvmresolve_location_src_store(meta, typ, tgt, None, of_tgt, count);

  let value = match cmpop {
    CMPOp::IntOp(i) => unsafe {
      let op = match i {
        IntOP::Equal => LLVMIntPredicate::LLVMIntEQ,
        IntOP::NotEqual => LLVMIntPredicate::LLVMIntNE,
        IntOP::SignedGreaterThan => LLVMIntPredicate::LLVMIntSGT,
        IntOP::SignedGreaterThanOrEqual => LLVMIntPredicate::LLVMIntSGE,
        IntOP::SignedLessThan => LLVMIntPredicate::LLVMIntSLT,
        IntOP::SignedLessThanOrEqual => LLVMIntPredicate::LLVMIntSLE,
        IntOP::UnsignedGreaterThan => LLVMIntPredicate::LLVMIntUGT,
        IntOP::UnsignedGreaterThanOrEqual => LLVMIntPredicate::LLVMIntUGE,
        IntOP::UnsignedLessThan => LLVMIntPredicate::LLVMIntULT,
        IntOP::UnsignedLessThanOrEqual => LLVMIntPredicate::LLVMIntULE,
      };
      LLVMBuildICmp(meta.builder, op, src1, src2, LLVM_VAR_NAME.0)
    },
    CMPOp::FloatOp(f) => unsafe {
      let op = match f {
        FloatOP::Equal => LLVMRealPredicate::LLVMRealOEQ,
        FloatOP::GreaterThan => LLVMRealPredicate::LLVMRealOGT,
        FloatOP::GreaterThanOrEqual => LLVMRealPredicate::LLVMRealOGE,
        FloatOP::LessThan => LLVMRealPredicate::LLVMRealOLT,
        FloatOP::LessThanOrEqual => LLVMRealPredicate::LLVMRealOLE,
        FloatOP::NotEqual => LLVMRealPredicate::LLVMRealONE,
        FloatOP::Ordered => LLVMRealPredicate::LLVMRealORD,
        FloatOP::OrderedNotEqual => LLVMRealPredicate::LLVMRealONE,
        FloatOP::Unordered => LLVMRealPredicate::LLVMRealUNO,
        FloatOP::UnorderedOrEqual => LLVMRealPredicate::LLVMRealUEQ,
        FloatOP::UnorderedOrGreaterThan => LLVMRealPredicate::LLVMRealUGT,
        FloatOP::UnorderedOrGreaterThanOrEqual => LLVMRealPredicate::LLVMRealUGE,
        FloatOP::UnorderedOrLessThan => LLVMRealPredicate::LLVMRealULT,
        FloatOP::UnorderedOrLessThanOrEqual => LLVMRealPredicate::LLVMRealULE,
      };

      LLVMBuildFCmp(meta.builder, op, src1, src2, LLVM_VAR_NAME.0)
    },
  };

  let val = unsafe {
    if count == 1 {
      LLVMBuildZExt(meta.builder, value, LLVMTypeOf(src1), LLVM_VAR_NAME.0)
    } else {
      LLVMBuildSExt(meta.builder, value, LLVMTypeOf(src1), LLVM_VAR_NAME.0)
    }
  };

  tgt.synchronize(meta, val);
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

      (*meta_ptr).regmnt.setreg(REG_R1, val);
    } else {
      let src = (*meta_ptr).regmnt.usereg(source as _);

      (*meta_ptr).regmnt.setreg(target as _, src);
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
        (*meta_ptr).regmnt.setreg(REG_R5, oflow);

        sum
      } else if saturate {
        let saturate = if r#type.signed {
          "llvm.sadd.sat"
        } else {
          "llvm.uadd.sat"
        };

        let mut typearg = [LLVMTypeOf(src1)];

        meta.call_intrinsic(saturate, &mut typearg, &mut [src1, src2])
      } else {
        let blk = LLVMGetInsertBlock(meta.builder);
        let name = CStr::from_ptr(LLVMGetBasicBlockName(blk));

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
        let mut typearg = [LLVMTypeOf(src1)];

        let valvect = meta.call_intrinsic(saturate, &mut typearg, &mut [src1, src2]);

        let sum = LLVMBuildExtractValue(meta.builder, valvect, 0, LLVM_VAR_NAME.0);
        let oflow = {
          let v = LLVMBuildExtractValue(meta.builder, valvect, 1, LLVM_VAR_NAME.0);

          LLVMBuildZExtOrBitCast(meta.builder, v, meta.i64, LLVM_VAR_NAME.0)
        };
        let meta_ptr = meta as *mut CompilerMeta;
        (*meta_ptr).regmnt.setreg(REG_R5, oflow);

        sum
      } else if saturate {
        let saturate = if r#type.signed {
          "llvm.ssub.sat"
        } else {
          "llvm.usub.sat"
        };

        let mut typearg = [LLVMTypeOf(src1)];

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
        let wide_vector_type = LLVMTypeOf(src1);

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

          let original_vector_type = LLVMTypeOf(src1);

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
