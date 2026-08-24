use llvm_sys::{
  core::{
    LLVMBuildFNeg, LLVMBuildSub, LLVMConstInt, LLVMConstNull, LLVMInt1TypeInContext, LLVMTypeOf,
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
    reader::vfop::{
      FOP_CEIL, FOP_FLOOR, FOP_ROUND, FOP_SQRT, FOP_TRUNC, VDATAOP, VFOP, parse_vdataop, parse_vfop,
    },
  },
};

pub fn handle_vfop(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VFOP {
    src,
    target,
    subop,
    offset_src,
    offset_target,
    count,
    typetag,
  } = parse_vfop(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Type(typetag);

  let src = llvmresolve_location_src_load(meta, typ, src, None, offset_src, count);
  let target = llvmresolve_location_src_store(meta, typ, target, None, offset_target, count);

  unsafe {
    let mut params = [LLVMTypeOf(src)];
    let name = match subop {
      FOP_CEIL => "llvm.ceil",
      FOP_FLOOR => "llvm.floor",
      FOP_ROUND => "llvm.round",
      FOP_SQRT => "llvm.sqrt",
      FOP_TRUNC => "llvm.trunc",
      _ => unreachable!(),
    };

    let vect = meta.call_intrinsic(name, &mut params, &mut [src]);
    target.synchronize(meta, vect);
  }
}

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

  target.synchronize(meta, output);
}

pub fn handle_vneg(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  handle_vdataop(pickle, meta, |ty, _count, meta, src1| {
    let extllvm = ty.r#type();
    let basety = unsafe { LLVMTypeOf(src1) };

    if extllvm.float {
      unsafe { LLVMBuildFNeg(meta.builder, src1, LLVM_VAR_NAME.0) }
    } else {
      unsafe {
        let zero = LLVMConstNull(basety);

        LLVMBuildSub(meta.builder, zero, src1, LLVM_VAR_NAME.0)
      }
    }
  });
}

pub fn handle_vabs(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  handle_vdataop(pickle, meta, |ty, _count, meta, src1| {
    let extllvm = ty.r#type();
    let basety = unsafe { LLVMTypeOf(src1) };

    if extllvm.float {
      meta.call_intrinsic("llvm.fabs", &mut [basety], &mut [src1])
    } else {
      let poison = unsafe { LLVMConstInt(LLVMInt1TypeInContext(meta.llvmctx), 0, 0) };
      meta.call_intrinsic("llvm.abs", &mut [basety], &mut [src1, poison])
    }
  });
}
