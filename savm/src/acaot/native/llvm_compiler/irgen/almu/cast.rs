use llvm_sys::core::{
  LLVMBuildFPExt, LLVMBuildFPToSI, LLVMBuildFPToUI, LLVMBuildFPTrunc, LLVMBuildSExtOrBitCast,
  LLVMBuildSIToFP, LLVMBuildTrunc, LLVMBuildUIToFP, LLVMBuildZExtOrBitCast,
};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::reg::{LLVMTypeOrWidth, llvmresolve_location_src_load, llvmresolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::cast::{CAST, VFCAST, parse_cast, parse_vfcast},
  },
};

pub fn handle_cast(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let CAST {
    offset_src,
    offset_target,
    src,
    target,
    type_initial,
    type_final,
  } = parse_cast(pickle, &meta.ws);

  let typ_src = LLVMTypeOrWidth::Type(type_initial);
  let typ_tgt = LLVMTypeOrWidth::Type(type_final);

  let typ_src_llvm = typ_src.r#type();
  let typ_tgt_llvm = typ_tgt.r#type();

  let src = llvmresolve_location_src_load(meta, typ_src, src, None, offset_src, 1);
  let target = llvmresolve_location_src_store(meta, typ_tgt, target, None, offset_target, 1);

  let arg1 = meta.builder;
  #[allow(non_snake_case)]
  let Name = LLVM_VAR_NAME.0;
  #[allow(non_snake_case)]
  let DestTy = typ_tgt_llvm.x1;
  #[allow(non_snake_case)]
  let Val = src;

  let val = unsafe {
    // Identity Cast
    if typ_src_llvm.x1 == typ_tgt_llvm.x1 {
      src
    } else
    // float->float
    if typ_src_llvm.float && typ_tgt_llvm.float {
      // demote
      if typ_src_llvm.width > typ_tgt_llvm.width {
        LLVMBuildFPTrunc(arg1, Val, DestTy, Name)
      }
      // Promote
      else {
        LLVMBuildFPExt(arg1, Val, DestTy, Name)
      }
    }
    // i -> float
    else if !typ_src_llvm.float && typ_tgt_llvm.float {
      if typ_src_llvm.signed {
        LLVMBuildSIToFP(arg1, Val, DestTy, Name)
      } else {
        LLVMBuildUIToFP(arg1, Val, DestTy, Name)
      }
    }
    // float -> i
    else if typ_src_llvm.float && !typ_tgt_llvm.float {
      if typ_tgt_llvm.signed {
        LLVMBuildFPToSI(arg1, Val, DestTy, Name)
      } else {
        LLVMBuildFPToUI(arg1, Val, DestTy, Name)
      }
    }
    // u* -> i or u (ZExt)
    else if !typ_src_llvm.signed {
      // Trunc
      if typ_src_llvm.width > typ_tgt_llvm.width {
        LLVMBuildTrunc(arg1, Val, DestTy, Name)
      }
      // ZExt (and bitcast if same width)
      else {
        LLVMBuildZExtOrBitCast(arg1, Val, DestTy, Name)
      }
    }
    // i -> u or i (SExt)
    else {
      // Trunc
      if typ_src_llvm.width > typ_tgt_llvm.width {
        LLVMBuildTrunc(arg1, Val, DestTy, Name)
      }
      // SExt (and bitcast if same width)
      else {
        LLVMBuildSExtOrBitCast(arg1, Val, DestTy, Name)
      }
    }
  };

  target.synchronize(meta, val);
}

pub fn handle_vfcast(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VFCAST {
    offset_src,
    offset_target,
    count,
    src,
    target,
    type_initial,
    type_final,
  } = parse_vfcast(pickle, &meta.ws);

  let typ_src = LLVMTypeOrWidth::Type(type_initial);
  let typ_tgt = LLVMTypeOrWidth::Type(type_final);

  let typ_src_llvm = typ_src.r#type();
  let typ_tgt_llvm = typ_tgt.r#type();

  let src = llvmresolve_location_src_load(meta, typ_src, src, None, offset_src, count);
  let target = llvmresolve_location_src_store(meta, typ_tgt, target, None, offset_target, count);

  let arg1 = meta.builder;
  #[allow(non_snake_case)]
  let Name = LLVM_VAR_NAME.0;
  #[allow(non_snake_case)]
  let DestTy = typ_tgt.vect(count);
  #[allow(non_snake_case)]
  let Val = src;

  let value = unsafe {
    // int->float
    if !typ_src_llvm.float {
      if typ_src_llvm.signed {
        LLVMBuildSIToFP(arg1, Val, DestTy, Name)
      } else {
        LLVMBuildUIToFP(arg1, Val, DestTy, Name)
      }
    } else {
      if typ_tgt_llvm.signed {
        LLVMBuildFPToSI(arg1, Val, DestTy, Name)
      } else {
        LLVMBuildFPToUI(arg1, Val, DestTy, Name)
      }
    }
  };

  target.synchronize(meta, value);
}
