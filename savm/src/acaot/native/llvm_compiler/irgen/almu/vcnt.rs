use llvm_sys::core::{
  LLVMBuildAShr, LLVMBuildOr, LLVMBuildShl, LLVMBuildXor, LLVMConstInt, LLVMConstVector,
  LLVMGetIntTypeWidth, LLVMInt1TypeInContext, LLVMTypeOf,
};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
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
    let ctx = meta.llvmctx;
    let builder = meta.builder;

    let val = match op {
      0 => meta.call_intrinsic("llvm.ctpop", &mut [LLVMTypeOf(src)], &mut [src]),
      1 => {
        let zero_poison = LLVMConstInt(LLVMInt1TypeInContext(ctx), 0, 0);
        meta.call_intrinsic("llvm.ctlz", &mut [LLVMTypeOf(src)], &mut [src, zero_poison])
      }
      2 => {
        let x1 = typ.r#type().x1;
        let bits = LLVMGetIntTypeWidth(x1);

        let rhs = {
          let scalar_rhs = LLVMConstInt(x1, (bits - 1) as u64, 0);
          let mut mask = vec![scalar_rhs; count as usize];

          if count == 1 { scalar_rhs } else { LLVMConstVector(mask.as_mut_ptr(), count as _) }
        };

        let ashr = LLVMBuildAShr(builder, src, rhs, LLVM_VAR_NAME.0);

        let xor = LLVMBuildXor(builder, src, ashr, LLVM_VAR_NAME.0);

        // (x ^ sign) << 1
        let one = LLVMConstInt(x1, 1, 0);
        let mut ones = vec![one; count as usize];

        let one_vec = if count == 1 { one } else { LLVMConstVector(ones.as_mut_ptr(), count as _) };

        let shl = LLVMBuildShl(builder, xor, one_vec, LLVM_VAR_NAME.0);

        // ... | 1
        let add_one = LLVMBuildOr(builder, shl, one_vec, LLVM_VAR_NAME.0);

        let zero_poison = LLVMConstInt(LLVMInt1TypeInContext(ctx), 0, 0);

        meta.call_intrinsic(
          "llvm.ctlz",
          &mut [LLVMTypeOf(src)],
          &mut [add_one, zero_poison],
        )
      }
      3 => {
        let zero_poison = LLVMConstInt(LLVMInt1TypeInContext(ctx), 0, 0);
        meta.call_intrinsic("llvm.cttz", &mut [LLVMTypeOf(src)], &mut [src, zero_poison])
      }
      _ => unreachable!(),
    };

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
