use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta,
    irgen::almu::{
      handle_add, handle_div, handle_mov, handle_mul, handle_rem, handle_sub, handle_vabs,
      handle_vfadd, handle_vfdiv, handle_vfma, handle_vfmul, handle_vfsub, handle_vneg, handle_vsh,
    },
  },
  pickle::def::{
    PICKLE_OPCODE_DIV, PICKLE_OPCODE_HINT, PICKLE_OPCODE_MARK, PICKLE_OPCODE_MOV,
    PICKLE_OPCODE_REG, PICKLE_OPCODE_REM, PICKLE_OPCODE_VABS, PICKLE_OPCODE_VADD,
    PICKLE_OPCODE_VADDF, PICKLE_OPCODE_VDIVF, PICKLE_OPCODE_VFMA, PICKLE_OPCODE_VMUL,
    PICKLE_OPCODE_VMULF, PICKLE_OPCODE_VNEG, PICKLE_OPCODE_VSH, PICKLE_OPCODE_VSUB,
    PICKLE_OPCODE_VSUBF, PICKLE_OPCODE_WS_PUT,
  },
};
use llvm_sys::core::{LLVMBuildBr, LLVMConstInt, LLVMPositionBuilderAtEnd};
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

      match pickle.opcode {
        // A NO-OP
        PICKLE_OPCODE_WS_PUT => {}

        // Soak up all in ws
        PICKLE_OPCODE_HINT => {
          let bytes = pickle.u3 as usize;

          unsafe {
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
          LLVMBuildBr(builder, newblock);

          LLVMPositionBuilderAtEnd(builder, newblock);
          meta.regmnt.newblock(newblock);
        }

        // AU
        PICKLE_OPCODE_VADD => handle_add(pickle, meta),
        PICKLE_OPCODE_VSUB => handle_sub(pickle, meta),
        PICKLE_OPCODE_VMUL => handle_mul(pickle, meta),
        PICKLE_OPCODE_DIV => handle_div(pickle, meta),
        PICKLE_OPCODE_REM => handle_rem(pickle, meta),

        PICKLE_OPCODE_VADDF => handle_vfadd(pickle, meta),
        PICKLE_OPCODE_VSUBF => handle_vfsub(pickle, meta),
        PICKLE_OPCODE_VMULF => handle_vfmul(pickle, meta),
        PICKLE_OPCODE_VDIVF => handle_vfdiv(pickle, meta),

        PICKLE_OPCODE_MOV => handle_mov(pickle, meta),
        PICKLE_OPCODE_REG => {
          let reg = pickle.u1;
          let marker = u64::from_ne_bytes(meta.ws[0..8].try_into().unwrap());

          let cint = LLVMConstInt(meta.i64, marker, 0);
          let meta_ptr = meta as *mut CompilerMeta;

          (*meta_ptr).regmnt.setreg(reg as _, cint, meta_ptr);
        }

        PICKLE_OPCODE_VSH => handle_vsh(pickle, meta),

        PICKLE_OPCODE_VFMA => handle_vfma(pickle, meta),
        PICKLE_OPCODE_VABS => handle_vabs(pickle, meta),
        PICKLE_OPCODE_VNEG => handle_vneg(pickle, meta),

        _ => {}
      }

      idx += 1;
    }

    // Jump to epilogue (normal)
    LLVMBuildBr(builder, meta.epilogue);
  }
}
