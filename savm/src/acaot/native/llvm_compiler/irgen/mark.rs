use llvm_sys::{
  LLVMIntPredicate,
  core::{
    LLVMAppendBasicBlockInContext, LLVMBuildAdd, LLVMBuildCondBr, LLVMBuildICmp, LLVMConstInt,
    LLVMInt16TypeInContext, LLVMPositionBuilderAtEnd,
  },
};

use crate::acaot::native::llvm_compiler::{CompilerMeta, LLVM_VAR_NAME};

pub fn mark_advanced(meta: &mut CompilerMeta) {
  unsafe {
    let builder = meta.builder;
    let ctx = meta.llvmctx;
    let f = meta.llvmfn;

    let counter = meta.regmnt.counter();
    let curr_counter = counter.read_variable(builder);

    let continue_block = LLVMAppendBasicBlockInContext(ctx, f, LLVM_VAR_NAME.0);

    // Add 1 to the counter
    let counter_below_256 = {
      let i16 = LLVMInt16TypeInContext(ctx);
      let counter_new = LLVMBuildAdd(
        builder,
        curr_counter,
        LLVMConstInt(i16, 1, 0),
        LLVM_VAR_NAME.0,
      );
      counter.write_variable(builder, counter_new);

      LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntULT,
        counter_new,
        LLVMConstInt(i16, 256, 0),
        LLVM_VAR_NAME.0,
      )
    };

    meta.call_intrinsic("llvm.assume", &mut [], &mut [counter_below_256]);
    LLVMBuildCondBr(
      builder,
      counter_below_256,
      continue_block,
      meta.sync_epilogue,
    );

    LLVMPositionBuilderAtEnd(builder, continue_block);
  }
}
