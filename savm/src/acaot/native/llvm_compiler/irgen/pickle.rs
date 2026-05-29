use crate::acaot::{
  native::llvm_compiler::CompilerMeta,
  pickle::def::{PICKLE_OPCODE_HINT, PICKLE_OPCODE_MARK},
};
use llvm_sys::core::{LLVMBuildBr, LLVMPositionBuilderAtEnd};
use std::ptr::copy_nonoverlapping;

pub unsafe fn compile_pickle(meta: &mut CompilerMeta) {
  unsafe {
    let builder = meta.builder;

    // Position at block V0
    LLVMPositionBuilderAtEnd(builder, meta.blockv0);

    let pickle = meta.pickle;
    let mut idx = 0usize;
    loop {
      if idx == pickle.len() {
        break;
      }

      let op = pickle[idx];

      match op.opcode {
        // Soak up all in ws
        PICKLE_OPCODE_HINT => {
          let bytes = op.u3 as usize;

          unsafe {
            copy_nonoverlapping(
              // the next instruction after the OPCODE_HINT is a bytestream
              pickle.as_ptr().add(idx + 1) as *const u8,
              meta.ws.as_mut_ptr(),
              bytes,
            );
          };

          let total_inst = op.u2 as usize;
          idx += total_inst;
        }
        PICKLE_OPCODE_MARK => {
          let marker = u64::from_ne_bytes(meta.ws[0..8].try_into().unwrap());

          let newblock = meta.blockmap.get(&marker).unwrap().current;
          LLVMBuildBr(builder, newblock);

          LLVMPositionBuilderAtEnd(builder, newblock);
          meta.regmnt.newblock(newblock);
        }

        _ => {}
      }

      idx += 1;
    }

    // Jump to epilogue (normal)
    LLVMBuildBr(builder, meta.epilogue);
  }
}
