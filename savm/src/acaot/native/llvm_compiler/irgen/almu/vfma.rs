use llvm_sys::core::{
  LLVMBuildAShr, LLVMBuildCall2, LLVMBuildLShr, LLVMBuildShl, LLVMFunctionType,
  LLVMGetCalledFunctionType, LLVMGetIntrinsicDeclaration, LLVMGlobalGetValueType,
  LLVMLookupIntrinsicID, LLVMTypeOf, LLVMVectorType,
};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::reg::{LLVMTypeOrWidth, llvmresolve_location_src_load, llvmresolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::{
      fp::{VFMA, parse_vfma},
      vsh::{VSH, parse_vsh},
    },
  },
};

pub fn handle_vfma(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VFMA {
    datatype,
    count,
    src1,
    of_src1,
    src2,
    of_src2,
    src3,
    of_src3,
    tgt,
    of_tgt,
  } = parse_vfma(pickle, meta.ws.as_ref());

  let datatype = LLVMTypeOrWidth::Type(datatype);
  let typdata = datatype.r#type();

  let src1 = llvmresolve_location_src_load(meta, datatype, src1, None, of_src1 as _, count);
  let src2 = llvmresolve_location_src_load(meta, datatype, src2, None, of_src2 as _, count);
  let src3 = llvmresolve_location_src_load(meta, datatype, src3, None, of_src3 as _, count);
  let target = llvmresolve_location_src_store(meta, datatype, tgt, None, of_tgt as _, count);

  let val = unsafe {
    let mut params = [LLVMTypeOf(src1)];

    meta.call_intrinsic("llvm.fma", &mut params, &mut [src1, src2, src3])
  };
  target.synchronize(meta, val);
}
