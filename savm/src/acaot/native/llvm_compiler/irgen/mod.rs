use crate::acaot::native::llvm_compiler::{CompilerMeta, LLVM_VAR_NAME};
use llvm_sys::{
  LLVMIntPredicate,
  core::{
    LLVMAddCase, LLVMBuildAnd, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildGEP2, LLVMBuildICmp,
    LLVMBuildLoad2, LLVMBuildRetVoid, LLVMBuildStore, LLVMBuildSwitch, LLVMBuildUnreachable,
    LLVMConstInt, LLVMFunctionType, LLVMGetCalledFunctionType, LLVMGetIntrinsicDeclaration,
    LLVMGlobalGetValueType, LLVMInt8TypeInContext, LLVMInt64TypeInContext, LLVMLookupIntrinsicID,
    LLVMPointerTypeInContext, LLVMPositionBuilderAtEnd, LLVMSetAlignment, LLVMVoidTypeInContext,
  },
  prelude::{LLVMBuilderRef, LLVMContextRef, LLVMTypeRef, LLVMValueRef},
};
use sart::{
  ctr::{FLAGS::FLAG_JUMP_TO_RESUME, VMTaskState},
  structures::QuadPackedData,
};
use std::{mem::offset_of, ptr::null_mut};

pub(crate) mod almu;
mod pickle;
pub(crate) mod reg;

pub fn compile(meta: &mut CompilerMeta) {
  unsafe {
    let builder = meta.builder;
    let ctx = meta.llvmctx;
    let module = meta.llvmmodule;
    let vmctx = meta.vmctx;
    let meta_ptr = meta as *mut CompilerMeta;

    // Prologue
    {
      (*meta_ptr).regmnt.init_largepad(meta_ptr);

      let scratchpad_ptr = offsetload(
        builder,
        ctx,
        meta.ptr,
        meta.vmctx,
        OffsetBytes::U(offset_of!(VMTaskState, scratchpad) as _),
      );
      meta.scratchpad = scratchpad_ptr;

      let resume_flags = offsetload(
        builder,
        ctx,
        meta.i32,
        meta.vmctx,
        OffsetBytes::U(offset_of!(VMTaskState, flags) as _),
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
        OffsetBytes::I(offset_of!(VMTaskState, curline_or_resume) as _),
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

      meta.call_intrinsic("llvm.trap", &mut [], &mut []);
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
        if let Some(regval) = (*meta_ptr).regmnt.try_usereg(regid, meta_ptr) {
          offsetstore(
            builder,
            ctx,
            regval,
            vmctx,
            OffsetBytes::U(size_of::<QuadPackedData>() as u64 * regid as u64),
          );
        }
      });

      LLVMBuildRetVoid(builder);
    }

    // Async Epilogue
    {
      LLVMPositionBuilderAtEnd(builder, meta.async_epilogue);

      // Sendback all regs in async
      let regs = [0, 1, 2, 3, 4, 5, 6, 7];

      (*meta_ptr).regmnt.newblock(meta.async_epilogue);

      regs.into_iter().for_each(|regid| {
        if let Some(regval) = (*meta_ptr).regmnt.try_usereg(regid, meta_ptr) {
          offsetstore(
            builder,
            ctx,
            regval,
            vmctx,
            OffsetBytes::U(size_of::<QuadPackedData>() as u64 * regid as u64),
          );
        }
      });

      LLVMBuildRetVoid(builder);
    }
  }
}

impl<'a> CompilerMeta<'a> {
  pub fn call_intrinsic(
    &mut self,
    name: &str,
    params: &mut [LLVMTypeRef],
    args: &mut [LLVMValueRef],
  ) -> LLVMValueRef {
    unsafe {
      let namebytes = name.as_bytes();
      let iid = LLVMLookupIntrinsicID(namebytes.as_ptr() as _, namebytes.len());

      let func =
        LLVMGetIntrinsicDeclaration(self.llvmmodule, iid, params.as_mut_ptr(), params.len());
      let func_type = LLVMGlobalGetValueType(func);

      LLVMBuildCall2(
        self.builder,
        func_type,
        func,
        args.as_mut_ptr(),
        args.len() as _,
        LLVM_VAR_NAME.0,
      )
    }
  }
}

pub fn offsetptr(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  pointerval: LLVMValueRef,
  offset_bytes: u64,
  signed: bool,
) -> LLVMValueRef {
  unsafe {
    let index_ty = LLVMInt64TypeInContext(ctx);
    let i8_ty = LLVMInt8TypeInContext(ctx);

    let mut indices = [LLVMConstInt(
      index_ty,
      offset_bytes as _,
      if signed { 1 } else { 0 },
    )];
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

pub fn offsetload_aligned(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  ty: LLVMTypeRef,
  pointerval: LLVMValueRef,
  offset_bytes: OffsetBytes,
  align: Option<u32>,
) -> LLVMValueRef {
  unsafe {
    let (of, signed) = offset_bytes.into();
    let offset_ptr = offsetptr(builder, ctx, pointerval, of, signed);

    let load = LLVMBuildLoad2(builder, ty, offset_ptr, LLVM_VAR_NAME.0);

    if let Some(align) = align {
      LLVMSetAlignment(load, align);
    }

    load
  }
}

pub fn offsetload(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  ty: LLVMTypeRef,
  pointerval: LLVMValueRef,
  offset_bytes: OffsetBytes,
) -> LLVMValueRef {
  offsetload_aligned(builder, ctx, ty, pointerval, offset_bytes, None)
}

pub fn offsetstore_aligned(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  val: LLVMValueRef,
  pointerval: LLVMValueRef,
  offset_bytes: OffsetBytes,
  align: Option<u32>,
) {
  unsafe {
    let (of, signed) = offset_bytes.into();
    let offset_ptr = offsetptr(builder, ctx, pointerval, of, signed);

    let store = LLVMBuildStore(builder, val, offset_ptr);

    if let Some(align) = align {
      LLVMSetAlignment(store, align);
    }
  }
}

pub fn offsetstore(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  val: LLVMValueRef,
  pointerval: LLVMValueRef,
  offset_bytes: OffsetBytes,
) {
  offsetstore_aligned(builder, ctx, val, pointerval, offset_bytes, None);
}

pub enum OffsetBytes {
  U(u64),
  I(i64),
}

impl OffsetBytes {
  pub fn into(self) -> (u64, bool) {
    match self {
      OffsetBytes::U(x) => (x, false),
      OffsetBytes::I(x) => (x.cast_unsigned(), true),
    }
  }
}
