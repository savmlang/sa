use crate::acaot::native::llvm_compiler::{LLVM_VAR_NAME, irgen::reg::LLVMTypeMapping};
use llvm_sys::{
  core::{
    LLVMBuildBitCast, LLVMBuildExtractElement, LLVMBuildInsertElement, LLVMConstInt, LLVMGetUndef,
    LLVMInt32TypeInContext, LLVMVectorType,
  },
  prelude::{LLVMBuilderRef, LLVMContextRef, LLVMValueRef},
};
use sart::structures::QuadPackedData;
use std::ops::Range;

pub fn regmapper(reg0: u8, offset_bytes: i32, typedata: LLVMTypeMapping, count: u32) -> RegMapOut {
  debug_assert!(
    offset_bytes >= 0,
    "OffsetBytes cannot be negative for registers"
  );
  let width = typedata.width as u32;

  let counts_in_1_reg = size_of::<QuadPackedData>() as u8 / typedata.width;
  let offset_knot_in_count = offset_bytes as u32 / width;

  debug_assert!(
    offset_knot_in_count < counts_in_1_reg as u32,
    "OffsetBytes cannot be greater than count in 1 register"
  );

  let regs = {
    let bytes_touched = width * count + offset_bytes as u32;
    let totalregstouched = bytes_touched.div_ceil(8) as u8;

    reg0..(reg0 + totalregstouched)
  };

  let lane0_reg0 = offset_knot_in_count % (counts_in_1_reg as u32);
  let lanek_regk = (offset_knot_in_count + count) % (counts_in_1_reg as u32);

  let total_regs = regs.len();
  let masks = regs
    .clone()
    .enumerate()
    .flat_map(|(idx, reg_id)| {
      let is_first = idx == 0;
      let is_last = idx == total_regs - 1;

      let start_lane = if is_first { lane0_reg0 as u8 } else { 0 };
      let end_lane = if is_last {
        if lanek_regk == 0 && count > 0 {
          counts_in_1_reg
        } else {
          lanek_regk as u8
        }
      } else {
        counts_in_1_reg
      };

      (start_lane..end_lane).map(move |lid| RegMask {
        laneid: lid,
        reg_fromroot: reg_id - reg0,
      })
    })
    .collect::<Vec<RegMask>>();

  RegMapOut {
    regstouched: regs,
    vectmask: masks,
  }
}

pub fn load_all_vectored(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  ty: LLVMTypeMapping,
  regs: &[RegMask],
  regv: &[LLVMValueRef],
) -> LLVMValueRef {
  unsafe {
    // DirectPath
    if ty.width == 8 {
      let base = ty.x1;
      let vect = if regv.len() == 1 {
        base
      } else {
        LLVMVectorType(base, regv.len() as _)
      };
      let mut vectbase = LLVMGetUndef(vect);

      for (id, &v) in regv.iter().enumerate() {
        let mut v = v;

        if ty.float {
          v = LLVMBuildBitCast(builder, v, ty.xreg, LLVM_VAR_NAME.0);
        }

        let idx = LLVMConstInt(LLVMInt32TypeInContext(ctx), id as u64, 0);

        if regv.len() == 1 {
          vectbase = v;
        } else {
          vectbase = LLVMBuildInsertElement(builder, vectbase, v, idx, LLVM_VAR_NAME.0);
        }
      }

      return vectbase;
    }

    let i32_ty = LLVMInt32TypeInContext(ctx);

    let prepared_vectors: Vec<LLVMValueRef> = regv
      .iter()
      .map(|&x| LLVMBuildBitCast(builder, x, ty.xreg, LLVM_VAR_NAME.0))
      .collect();

    let dest_vector_type = LLVMVectorType(ty.x1, regs.len() as _);
    let mut final_vector = LLVMGetUndef(dest_vector_type);

    for (dest_lane, mask) in regs.iter().enumerate() {
      let src_reg_idx = mask.reg_fromroot as usize;
      let src_lane_idx = mask.laneid as u64;

      let src_vector = prepared_vectors[src_reg_idx];

      let src_lane_val = LLVMConstInt(i32_ty, src_lane_idx, 0);
      let extracted_scalar =
        LLVMBuildExtractElement(builder, src_vector, src_lane_val, LLVM_VAR_NAME.0);

      let dest_lane_val = LLVMConstInt(i32_ty, dest_lane as u64, 0);
      final_vector = LLVMBuildInsertElement(
        builder,
        final_vector,
        extracted_scalar,
        dest_lane_val,
        LLVM_VAR_NAME.0,
      );
    }

    final_vector
  }
}

#[derive(Debug, Clone)]
pub struct RegMapOut {
  pub regstouched: Range<u8>,
  pub vectmask: Vec<RegMask>,
}

#[derive(Debug, Clone)]
pub struct RegMask {
  pub reg_fromroot: u8,
  pub laneid: u8,
}
