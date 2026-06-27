use llvm_sys::{
  core::{LLVMBuildFAdd, LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFSub},
  prelude::LLVMValueRef,
};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::reg::{
      LLVMTypeMapping, LLVMTypeOrWidth, llvmresolve_location_src_load,
      llvmresolve_location_src_store,
    },
  },
  pickle::{
    def::PickleInstruction,
    reader::fp::{VFP, parse_vfp},
  },
};

#[inline(always)]
fn fpop<F>(pickle: &PickleInstruction, meta: &mut CompilerMeta, call: F)
where
  // Remains useful for things like, mul_wide
  F: FnOnce(&mut CompilerMeta, &LLVMTypeMapping, u32, LLVMValueRef, LLVMValueRef) -> LLVMValueRef,
{
  let VFP {
    count,
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
    ..
  } = parse_vfp(pickle, meta.ws.as_ref());

  let datatype = LLVMTypeOrWidth::Type(datatype);
  let typdata = datatype.r#type();

  let src1 = llvmresolve_location_src_load(meta, datatype, src1, None, of_src1, count as _);
  let src2 = llvmresolve_location_src_load(meta, datatype, src2, None, of_src2, count as _);
  let target = llvmresolve_location_src_store(meta, datatype, tgt, None, of_tgt as _, count as _);

  let output = call(meta, &typdata, count, src1, src2);

  target.synchronize(meta, output);
}

pub fn handle_vfadd(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  fpop(pickle, meta, |meta, _datatype, _, src1, src2| unsafe {
    LLVMBuildFAdd(meta.builder, src1, src2, LLVM_VAR_NAME.0)
  });
}

pub fn handle_vfsub(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  fpop(pickle, meta, |meta, _datatype, _, src1, src2| unsafe {
    LLVMBuildFSub(meta.builder, src1, src2, LLVM_VAR_NAME.0)
  });
}

pub fn handle_vfmul(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  fpop(pickle, meta, |meta, _datatype, _, src1, src2| unsafe {
    LLVMBuildFMul(meta.builder, src1, src2, LLVM_VAR_NAME.0)
  });
}

pub fn handle_vfdiv(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  fpop(pickle, meta, |meta, _datatype, _, src1, src2| unsafe {
    LLVMBuildFDiv(meta.builder, src1, src2, LLVM_VAR_NAME.0)
  });
}
