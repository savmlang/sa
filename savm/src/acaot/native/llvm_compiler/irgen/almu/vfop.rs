use llvm_sys::{
  core::{
    LLVMBuildAShr, LLVMBuildCall2, LLVMBuildLShr, LLVMBuildShl, LLVMBuildSub, LLVMConstInt,
    LLVMConstNull, LLVMFunctionType, LLVMGetCalledFunctionType, LLVMGetIntrinsicDeclaration,
    LLVMGlobalGetValueType, LLVMLookupIntrinsicID, LLVMTypeOf, LLVMVectorType,
  },
  prelude::LLVMValueRef,
};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::reg::{LLVMTypeOrWidth, llvmresolve_location_src_load, llvmresolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::vfop::{VDATAOP, parse_vdataop},
  },
};

pub fn handle_vdataop<F>(pickle: &PickleInstruction, meta: &mut CompilerMeta, process: F)
where
  F: FnOnce(LLVMTypeOrWidth, u32, &mut CompilerMeta, LLVMValueRef) -> LLVMValueRef,
{
  let VDATAOP {
    datatype,
    count,
    src1,
    of_src1,
    tgt,
    of_tgt,
  } = parse_vdataop(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Type(datatype);

  let src1 = { llvmresolve_location_src_load(meta, typ, src1 as u8, None, of_src1, count) };
  let target = { llvmresolve_location_src_store(meta, typ, tgt as u8, None, of_tgt, count) };

  let output = process(typ, count, meta, src1);

  target.synchronize(meta, src1);
}

pub fn handle_vneg(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  handle_vdataop(pickle, meta, |ty, count, meta, src1| {
    let extllvm = ty.r#type();
    let basety = if count == 1 {
      extllvm.x1
    } else {
      unsafe { LLVMVectorType(extllvm.x1, count) }
    };

    if extllvm.float {
      meta.call_intrinsic("llvm.fneg", &mut [basety], &mut [src1])
    } else {
      unsafe {
        let zero = LLVMConstNull(basety);

        LLVMBuildSub(meta.builder, zero, src1, LLVM_VAR_NAME.0)
      }
    }
  });
}

pub fn handle_vabs(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  handle_vdataop(pickle, meta, |ty, count, meta, src1| {
    let extllvm = ty.r#type();
    let basety = if count == 1 {
      extllvm.x1
    } else {
      unsafe { LLVMVectorType(extllvm.x1, count) }
    };

    meta.call_intrinsic(
      if extllvm.float {
        "llvm.fabs"
      } else {
        "llvm.abs"
      },
      &mut [basety],
      &mut [src1],
    )
  });
}
