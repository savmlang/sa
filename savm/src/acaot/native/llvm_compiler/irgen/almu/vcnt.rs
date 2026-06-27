use llvm_sys::core::LLVMTypeOf;

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta,
    irgen::reg::{LLVMTypeOrWidth, llvmresolve_location_src_load, llvmresolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::vminimax::{VCNT, VMINIMAX, parse_vcnt, parse_vminimax},
  },
};

pub fn handle_vcnt(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VCNT {
    op,
    flags_src,
    flags_target,
    count,
    of_src,
    of_target,
    typ,
    alignment_src,
    alignment_target,
  } = parse_vcnt(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Width(typ);

  let src = llvmresolve_location_src_load(meta, typ, flags_src, alignment_src, of_src as _, count);
  let target = llvmresolve_location_src_store(
    meta,
    typ,
    flags_target,
    alignment_target,
    of_target as _,
    count,
  );

  unsafe {
    let intrinsic = match op {
      0 => "llvm.ctpop",
      1 => "llvm.ctlz",
      2 => "llvm.clrsb",
      3 => "llvm.ctz",
      _ => unreachable!(),
    };

    let val = meta.call_intrinsic(intrinsic, &mut [LLVMTypeOf(src)], &mut [src]);

    target.synchronize(meta, val);
  }
}

pub fn handle_vminimax(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VMINIMAX {
    op,
    flags_src1,
    flags_src2,
    flags_target,
    count,
    of_src1,
    of_src2,
    of_target,
    typ,
    alignment_src1,
    alignment_src2,
    alignment_target,
  } = parse_vminimax(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Type(typ);

  let src1 =
    llvmresolve_location_src_load(meta, typ, flags_src1, alignment_src1, of_src1 as _, count);
  let src2 =
    llvmresolve_location_src_load(meta, typ, flags_src2, alignment_src2, of_src2 as _, count);

  let target = llvmresolve_location_src_store(
    meta,
    typ,
    flags_target,
    alignment_target,
    of_target as _,
    count,
  );

  let is_min = op == 0;

  let mapping = typ.r#type();
  let name = if mapping.float {
    if is_min { "llvm.minnum" } else { "llvm.maxnum" }
  } else if mapping.signed {
    if is_min { "llvm.smin" } else { "llvm.smax" }
  } else {
    if is_min { "llvm.umin" } else { "llvm.umax" }
  };

  let out = unsafe { meta.call_intrinsic(name, &mut [LLVMTypeOf(src1)], &mut [src1, src2]) };

  target.synchronize(meta, out);
}
