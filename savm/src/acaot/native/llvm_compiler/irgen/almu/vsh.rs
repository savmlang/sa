use llvm_sys::core::{LLVMBuildAShr, LLVMBuildLShr, LLVMBuildShl};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::reg::{LLVMTypeOrWidth, llvmresolve_location_src_load, llvmresolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::vsh::{VSH, parse_vsh},
  },
};

pub fn handle_vsh(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VSH {
    op,
    flags_src1: src1,
    flags_src2: src2,
    flags_target: target,
    count,
    of_src1,
    of_src2,
    of_target,
    typ,
  } = parse_vsh(&pickle, &meta.ws);

  let datatype = LLVMTypeOrWidth::Type(typ);
  let typdata = datatype.r#type();

  let src1 = llvmresolve_location_src_load(meta, datatype, src1, None, of_src1 as _, count);
  let src2 = llvmresolve_location_src_load(meta, datatype, src2, None, of_src2 as _, count);
  let target = llvmresolve_location_src_store(meta, datatype, target, None, of_target as _, count);

  let val = unsafe {
    match op {
      // SHL
      0 => LLVMBuildShl(meta.builder, src1, src2, LLVM_VAR_NAME.0),
      1 => {
        if typdata.signed {
          LLVMBuildAShr(meta.builder, src1, src2, LLVM_VAR_NAME.0)
        } else {
          LLVMBuildLShr(meta.builder, src1, src2, LLVM_VAR_NAME.0)
        }
      }
      _ => unreachable!(),
    }
  };
  target.synchronize(meta, val);
}
