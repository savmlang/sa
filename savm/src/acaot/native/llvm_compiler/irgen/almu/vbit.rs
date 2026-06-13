use llvm_sys::core::{LLVMBuildAnd, LLVMBuildNot, LLVMBuildOr, LLVMBuildXor, LLVMTypeOf};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::reg::{LLVMTypeOrWidth, llvmresolve_location_src_load, llvmresolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::vbit::{
      VBIT, VBIT_BAND, VBIT_BAND_NOT, VBIT_BITREV, VBIT_BITSWAP, VBIT_BOR, VBIT_BOR_NOT, VBIT_BXOR,
      VBIT_BXOR_NOT, VBIT_NOT, VROT, VROT_L, VROT_R, parse_vbit, parse_vrot,
    },
  },
};

pub fn handle_vbit(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VBIT {
    count,
    op,
    width,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_vbit(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Width(width);

  let src1 = llvmresolve_location_src_load(meta, typ, src1, None, of_src1, count);
  let src2 = llvmresolve_location_src_load(meta, typ, src2, None, of_src2, count);
  let target = llvmresolve_location_src_store(meta, typ, tgt, None, of_tgt, count);

  #[allow(non_snake_case)]
  unsafe {
    let arg1 = meta.builder;
    let LHS = src1;
    let RHS = src2;
    let Name = LLVM_VAR_NAME.0;

    let out = match op {
      VBIT_BAND => LLVMBuildAnd(arg1, LHS, RHS, Name),
      VBIT_BOR => LLVMBuildOr(arg1, LHS, RHS, Name),
      VBIT_NOT => LLVMBuildNot(arg1, LHS, Name),
      VBIT_BXOR => LLVMBuildXor(arg1, LHS, RHS, Name),
      VBIT_BITSWAP => meta.call_intrinsic("llvm.bswap", &mut [LLVMTypeOf(src1)], &mut [src1]),
      VBIT_BITREV => meta.call_intrinsic("llvm.bitreverse", &mut [LLVMTypeOf(src1)], &mut [src1]),
      VBIT_BAND_NOT => {
        let src2 = LLVMBuildNot(arg1, src2, Name);

        LLVMBuildAnd(arg1, LHS, src2, Name)
      }
      VBIT_BOR_NOT => {
        let src2 = LLVMBuildNot(arg1, src2, Name);

        LLVMBuildOr(arg1, LHS, src2, Name)
      }
      VBIT_BXOR_NOT => {
        let src2 = LLVMBuildNot(arg1, src2, Name);

        LLVMBuildXor(arg1, LHS, src2, Name)
      }
      _ => unreachable!(),
    };
    target.synchronize(meta, out);
  }
}

pub fn handle_vrot(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VROT {
    count,
    op,
    typetag,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_vrot(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Width(typetag);

  let src1 = llvmresolve_location_src_load(meta, typ, src1, None, of_src1, count);
  let src2 = llvmresolve_location_src_load(meta, typ, src2, None, of_src2, count);
  let target = llvmresolve_location_src_store(meta, typ, tgt, None, of_tgt, count);

  let name = match op {
    VROT_L => "llvm.fshl",
    VROT_R => "llvm.fshr",
    _ => unreachable!(),
  };

  unsafe {
    let vect = meta.call_intrinsic(name, &mut [LLVMTypeOf(src1)], &mut [src1, src2]);
    target.synchronize(meta, vect);
  }
}
