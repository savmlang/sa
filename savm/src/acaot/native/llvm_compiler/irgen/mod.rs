use crate::acaot::native::llvm_compiler::{CompilerMeta, LLVM_VAR_NAME};
use llvm_sys::{
  LLVMIntPredicate,
  core::{
    LLVMAddCase, LLVMBuildAnd, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildGEP2, LLVMBuildICmp,
    LLVMBuildLoad2, LLVMBuildRetVoid, LLVMBuildStore, LLVMBuildSwitch, LLVMBuildUnreachable,
    LLVMConstInt, LLVMFunctionType, LLVMGetIntrinsicDeclaration, LLVMInt8TypeInContext,
    LLVMInt64TypeInContext, LLVMLookupIntrinsicID, LLVMPointerTypeInContext,
    LLVMPositionBuilderAtEnd, LLVMVoidTypeInContext,
  },
  prelude::{LLVMBuilderRef, LLVMContextRef, LLVMTypeRef, LLVMValueRef},
};
use sart::{
  ctr::{FLAGS::FLAG_JUMP_TO_RESUME, VMTaskState},
  structures::QuadPackedData,
};
use std::{mem::offset_of, ptr::null_mut};

mod pickle;

pub fn compile(meta: &mut CompilerMeta) {
  unsafe {
    let builder = meta.builder;
    let ctx = meta.llvmctx;
    let module = meta.llvmmodule;
    let vmctx = meta.vmctx;
    let meta_ptr = meta as *mut _;

    // Prologue
    {
      meta.regmnt.init_largepad(meta_ptr);

      let scratchpad_ptr = offsetload(
        builder,
        ctx,
        meta.ptr,
        meta.vmctx,
        offset_of!(VMTaskState, scratchpad),
      );
      meta.scratchpad = scratchpad_ptr;

      let resume_flags = offsetload(
        builder,
        ctx,
        meta.i32,
        meta.vmctx,
        offset_of!(VMTaskState, flags),
      );

      let rhs = LLVMConstInt(meta.i32, FLAG_JUMP_TO_RESUME as _, 0);
      let opcode = LLVMBuildAnd(builder, resume_flags, rhs, LLVM_VAR_NAME.0);

      let zero = LLVMConstInt(meta.i32, 0, 0);

      let opcode_bool = LLVMBuildICmp(
        builder,
        LLVMIntPredicate::LLVMIntNE,
        opcode,
        zero,
        LLVM_VAR_NAME.0,
      );

      LLVMBuildCondBr(builder, opcode_bool, meta.jumpresolver, meta.blockv0);
    }

    // JumpResolver
    {
      LLVMPositionBuilderAtEnd(builder, meta.jumpresolver);

      let val = offsetload(
        builder,
        ctx,
        meta.i64,
        meta.vmctx,
        offset_of!(VMTaskState, curline_or_resume),
      );
      let switch = LLVMBuildSwitch(builder, val, meta.trap, meta.blockmap.len() as _);

      for (k, block) in &meta.blockmap {
        let val = LLVMConstInt(meta.i64, *k, 0);
        LLVMAddCase(switch, val, block.current);
      }
    }

    // Trap
    {
      LLVMPositionBuilderAtEnd(builder, meta.trap);

      let trap = LLVMLookupIntrinsicID(c"llvm.trap".as_ptr(), 9);
      let trap_func = LLVMGetIntrinsicDeclaration(module, trap, null_mut(), 0);
      let trap_type = LLVMFunctionType(LLVMVoidTypeInContext(ctx), null_mut(), 0, 0);

      LLVMBuildCall2(
        builder,
        trap_type,
        trap_func,
        null_mut(),
        0,
        LLVM_VAR_NAME.0,
      );
      LLVMBuildUnreachable(builder);
    }

    // Now we must compile pickles
    pickle::compile_pickle(meta);

    // Epilogue
    {
      LLVMPositionBuilderAtEnd(builder, meta.epilogue);

      // Sendback
      #[cfg(feature = "sendback")]
      let regs = [0, 1, 2, 3, 4, 5, 6, 7];

      #[cfg(not(feature = "sendback"))]
      let regs = [6, 7];

      meta.regmnt.newblock(meta.epilogue);

      regs.into_iter().for_each(|regid| {
        let regval = meta.regmnt.usereg(regid, meta_ptr);

        offsetstore(
          builder,
          ctx,
          regval,
          vmctx,
          size_of::<QuadPackedData>() * regid,
        );
      });

      LLVMBuildRetVoid(builder);
    }

    // Async Epilogue
    {
      LLVMPositionBuilderAtEnd(builder, meta.async_epilogue);

      // Sendback all regs in async
      let regs = [0, 1, 2, 3, 4, 5, 6, 7];

      meta.regmnt.newblock(meta.async_epilogue);

      regs.into_iter().for_each(|regid| {
        let regval = meta.regmnt.usereg(regid, meta_ptr);

        offsetstore(
          builder,
          ctx,
          regval,
          vmctx,
          size_of::<QuadPackedData>() * regid,
        );
      });

      LLVMBuildRetVoid(builder);
    }
  }
}

pub fn offsetptr(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  pointerval: LLVMValueRef,
  offset_bytes: usize,
) -> LLVMValueRef {
  unsafe {
    let index_ty = LLVMInt64TypeInContext(ctx);
    let i8_ty = LLVMInt8TypeInContext(ctx);

    let mut indices = [LLVMConstInt(index_ty, offset_bytes as _, 0)];
    LLVMBuildGEP2(
      builder,
      i8_ty,
      pointerval,
      indices.as_mut_ptr(),
      1,
      LLVM_VAR_NAME.0,
    )
  }
}

pub fn offsetload(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  ty: LLVMTypeRef,
  pointerval: LLVMValueRef,
  offset_bytes: usize,
) -> LLVMValueRef {
  unsafe {
    let offset_ptr = offsetptr(builder, ctx, pointerval, offset_bytes);

    LLVMBuildLoad2(builder, ty, offset_ptr, LLVM_VAR_NAME.0)
  }
}

pub fn offsetstore(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  val: LLVMValueRef,
  pointerval: LLVMValueRef,
  offset_bytes: usize,
) {
  unsafe {
    let offset_ptr = offsetptr(builder, ctx, pointerval, offset_bytes);

    LLVMBuildStore(builder, val, offset_ptr);
  }
}
