use llvm_sys::{
  LLVMIntPredicate,
  core::{
    LLVMAppendBasicBlockInContext, LLVMBuildAdd, LLVMBuildBr, LLVMBuildCondBr, LLVMBuildICmp,
    LLVMConstInt, LLVMPositionBuilderAtEnd,
  },
};

use crate::{
  CacheLevel,
  acaot::native::llvm_compiler::{CompilerMeta, LLVM_VAR_NAME},
};

pub fn mark_advanced(meta: &mut CompilerMeta, marker: u64) {
  unsafe {
    let builder = meta.builder;
    let ctx = meta.llvmctx;
    let f = meta.llvmfn;

    let counter = meta.regmnt.counter();
    let curr_counter = counter.read_variable(builder);

    let continue_block = LLVMAppendBasicBlockInContext(ctx, f, LLVM_VAR_NAME.0);
    let else_block = LLVMAppendBasicBlockInContext(ctx, f, LLVM_VAR_NAME.0);

    // Add 1 to the counter
    let counter_below_256 = {
      let counter_new = LLVMBuildAdd(
        builder,
        curr_counter,
        LLVMConstInt(meta.i64, 1, 0),
        LLVM_VAR_NAME.0,
      );
      counter.write_variable(builder, counter_new);

      LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntULT,
        counter_new,
        LLVMConstInt(meta.i64, 256, 0),
        LLVM_VAR_NAME.0,
      )
    };

    // Only add the HINT for Crater
    // This is to reduce CINDER comptimes
    if matches!(meta.cache_level, CacheLevel::LLVMCrater) {
      meta.call_intrinsic("llvm.assume", &mut [], &mut [counter_below_256]);
    }
    LLVMBuildCondBr(builder, counter_below_256, continue_block, else_block);

    // else_block
    {
      LLVMPositionBuilderAtEnd(builder, else_block);

      meta
        .regmnt
        .counter()
        .write_variable(builder, LLVMConstInt(meta.i64, marker, 0));

      LLVMBuildBr(builder, meta.sync_epilogue);
    }

    LLVMPositionBuilderAtEnd(builder, continue_block);
  }
}
