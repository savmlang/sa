use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta, LLVM_VAR_NAME,
    irgen::{
      almu::{
        handle_add, handle_atomic, handle_cast, handle_div, handle_mov, handle_mul, handle_reg,
        handle_rem, handle_sub, handle_vabs, handle_vbit, handle_vcmp, handle_vcnt, handle_vcopy,
        handle_vfadd, handle_vfcast, handle_vfdiv, handle_vfma, handle_vfmul, handle_vfop,
        handle_vfsub, handle_vminimax, handle_vneg, handle_vrot, handle_vsh,
      },
      reg::{LLVMTypeOrWidth, llvmresolve_location_src_load},
    },
  },
  pickle::def::{
    PICKLE_OPCODE_ATOMIC, PICKLE_OPCODE_CAST, PICKLE_OPCODE_DIV, PICKLE_OPCODE_HINT,
    PICKLE_OPCODE_JIF, PICKLE_OPCODE_JMP, PICKLE_OPCODE_MARK, PICKLE_OPCODE_MOV, PICKLE_OPCODE_REG,
    PICKLE_OPCODE_REM, PICKLE_OPCODE_VABS, PICKLE_OPCODE_VADD, PICKLE_OPCODE_VADDF,
    PICKLE_OPCODE_VBIT, PICKLE_OPCODE_VCMP, PICKLE_OPCODE_VCNT, PICKLE_OPCODE_VCOPY,
    PICKLE_OPCODE_VDIVF, PICKLE_OPCODE_VFCAST, PICKLE_OPCODE_VFMA, PICKLE_OPCODE_VFOP,
    PICKLE_OPCODE_VMINIMAX, PICKLE_OPCODE_VMUL, PICKLE_OPCODE_VMULF, PICKLE_OPCODE_VNEG,
    PICKLE_OPCODE_VROT, PICKLE_OPCODE_VSH, PICKLE_OPCODE_VSUB, PICKLE_OPCODE_VSUBF,
    PICKLE_OPCODE_WS_PUT,
  },
};
use llvm_sys::{
  LLVMIntPredicate,
  core::{
    LLVMAppendBasicBlockInContext, LLVMBuildBr, LLVMBuildCondBr, LLVMBuildICmp,
    LLVMClearInsertionPosition, LLVMConstNull, LLVMGetInsertBlock, LLVMPositionBuilderAtEnd,
  },
};
use std::ptr::copy_nonoverlapping;

pub unsafe fn compile_pickle(meta: &mut CompilerMeta) {
  unsafe {
    let builder = meta.builder;

    // Position at block V0
    LLVMPositionBuilderAtEnd(builder, meta.blockv0);

    let pickles = meta.pickle;
    let mut idx = 0usize;
    loop {
      if idx == pickles.len() {
        break;
      }

      let pickle = &pickles[idx];
      let current_block = LLVMGetInsertBlock(builder);

      match pickle.opcode {
        // A NO-OP
        PICKLE_OPCODE_WS_PUT => {}

        // Soak up all in ws
        PICKLE_OPCODE_HINT => {
          let bytes = pickle.u3 as usize;

          {
            copy_nonoverlapping(
              // the next instruction after the OPCODE_HINT is a bytestream
              pickles.as_ptr().add(idx + 1) as *const u8,
              meta.ws.as_mut_ptr(),
              bytes,
            );
          };

          let total_inst = pickle.u2 as usize;
          idx += total_inst;
        }
        PICKLE_OPCODE_MARK => {
          let marker = u64::from_ne_bytes(meta.ws[0..8].try_into().unwrap());

          let newblock = meta.blockmap.get(&marker).unwrap().current;

          if !current_block.is_null() {
            LLVMBuildBr(builder, newblock);
          }

          LLVMPositionBuilderAtEnd(builder, newblock);
        }

        op if !current_block.is_null() => match op {
          // AU
          PICKLE_OPCODE_VADD => handle_add(pickle, meta),
          PICKLE_OPCODE_VSUB => handle_sub(pickle, meta),
          PICKLE_OPCODE_VMUL => handle_mul(pickle, meta),
          PICKLE_OPCODE_DIV => handle_div(pickle, meta),
          PICKLE_OPCODE_REM => handle_rem(pickle, meta),

          // FAU
          PICKLE_OPCODE_VADDF => handle_vfadd(pickle, meta),
          PICKLE_OPCODE_VSUBF => handle_vfsub(pickle, meta),
          PICKLE_OPCODE_VMULF => handle_vfmul(pickle, meta),
          PICKLE_OPCODE_VDIVF => handle_vfdiv(pickle, meta),

          // CU, LU
          PICKLE_OPCODE_MOV => handle_mov(pickle, meta),
          PICKLE_OPCODE_VCMP => handle_vcmp(pickle, meta),
          PICKLE_OPCODE_JIF => {
            let intent = pickle.u1;
            let relocation_src = pickle.u2;
            let width = pickle.u3;

            let offset = i32::from_ne_bytes(meta.ws[0..4].try_into().unwrap_unchecked());
            let marker = u64::from_ne_bytes(meta.ws[4..12].try_into().unwrap_unchecked());

            let typ = LLVMTypeOrWidth::Width(width);
            let r#type = typ.r#type();

            let src = llvmresolve_location_src_load(meta, typ, relocation_src, None, offset, 1);

            let ifzero = LLVMBuildICmp(
              meta.builder,
              LLVMIntPredicate::LLVMIntNE,
              src,
              LLVMConstNull(r#type.x1),
              LLVM_VAR_NAME.0,
            );

            let contd =
              LLVMAppendBasicBlockInContext(meta.llvmctx, meta.llvmfn, c"jmp_target".as_ptr());
            let jmpblock = meta.blockmap.get(&marker).unwrap().current;

            let (then, other) = if intent == 0 {
              // Jump If Zero
              (contd, jmpblock)
            } else {
              (jmpblock, contd)
            };

            LLVMBuildCondBr(meta.builder, ifzero, then, other);

            LLVMPositionBuilderAtEnd(meta.builder, contd);
          }
          PICKLE_OPCODE_REG => handle_reg(pickle, meta),
          PICKLE_OPCODE_JMP => {
            let marker = u64::from_ne_bytes(meta.ws[0..8].try_into().unwrap());

            let jmpaddr = meta.blockmap.get(&marker).unwrap().current;
            LLVMBuildBr(builder, jmpaddr);

            LLVMClearInsertionPosition(builder);
          }

          // AU-Pt-II
          PICKLE_OPCODE_VSH => handle_vsh(pickle, meta),
          PICKLE_OPCODE_VFMA => handle_vfma(pickle, meta),
          PICKLE_OPCODE_VABS => handle_vabs(pickle, meta),
          PICKLE_OPCODE_VNEG => handle_vneg(pickle, meta),

          PICKLE_OPCODE_VFOP => handle_vfop(pickle, meta),
          PICKLE_OPCODE_VBIT => handle_vbit(pickle, meta),
          PICKLE_OPCODE_VROT => handle_vrot(pickle, meta),

          PICKLE_OPCODE_VCNT => handle_vcnt(pickle, meta),
          PICKLE_OPCODE_VMINIMAX => handle_vminimax(pickle, meta),

          PICKLE_OPCODE_ATOMIC => handle_atomic(pickle, meta),
          PICKLE_OPCODE_VCOPY => handle_vcopy(pickle, meta),

          PICKLE_OPCODE_CAST => handle_cast(pickle, meta),
          PICKLE_OPCODE_VFCAST => handle_vfcast(pickle, meta),
          _ => {}
        },

        _ => {}
      }

      idx += 1;
    }

    // Jump to epilogue (normal)
    // Unless its a spin loop (though why would one write one??)
    let final_block = LLVMGetInsertBlock(builder);
    if !final_block.is_null() {
      LLVMBuildBr(builder, meta.epilogue);
    }
  }
}
