use crate::acaot::native::llvm_compiler::{CompilerMeta, LLVM_VAR_NAME};
use llvm_sys::{
  LLVMIntPredicate,
  core::{
    LLVMAddCase, LLVMBuildAnd, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildGEP2, LLVMBuildICmp,
    LLVMBuildLoad2, LLVMBuildMemCpy, LLVMBuildOr, LLVMBuildRetVoid, LLVMBuildStore,
    LLVMBuildSwitch, LLVMBuildUnreachable, LLVMConstInt, LLVMGetIntrinsicDeclaration,
    LLVMGlobalGetValueType, LLVMInt8TypeInContext, LLVMInt64TypeInContext, LLVMLookupIntrinsicID,
    LLVMPositionBuilderAtEnd, LLVMSetAlignment,
  },
  prelude::{LLVMBuilderRef, LLVMContextRef, LLVMTypeRef, LLVMValueRef},
};
use sart::{
  ctr::{
    FLAGS::FLAG_JUMP_TO_RESUME,
    OPCODES::{OPCODE_JIT_CHECK, OPCODE_OK},
    VMTaskState,
  },
  structures::QuadPackedData,
};
use std::mem::offset_of;

pub(crate) mod almu;
pub(super) mod mark;
mod pickle;
pub(crate) mod reg;

pub fn compile<const SENDBACK: bool>(meta: &mut CompilerMeta) {
  unsafe {
    let builder = meta.builder;
    let ctx = meta.llvmctx;
    let vmctx = meta.vmctx;
    let meta_ptr = meta as *mut CompilerMeta;

    // Prologue
    {
      let scratchpad_ptr = offsetload(
        builder,
        ctx,
        meta.ptr,
        meta.vmctx,
        OffsetBytes::U(offset_of!(VMTaskState, scratchpad) as _),
      );
      meta.scratchpad_ptr = scratchpad_ptr;

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

      // Scratchpad Copy
      LLVMBuildMemCpy(
        builder,
        meta.scratchpad,
        64,
        meta.scratchpad_ptr,
        64,
        LLVMConstInt(meta.i64, 192, 0),
      );

      // Load(Hydrate) all regs
      backload(meta_ptr, builder, ctx, vmctx);

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

    // Epilogue (SYNC)
    {
      LLVMPositionBuilderAtEnd(builder, meta.sync_epilogue);
      // Scratchpad Copy

      LLVMBuildMemCpy(
        builder,
        meta.scratchpad_ptr,
        64,
        meta.scratchpad,
        64,
        LLVMConstInt(meta.i64, 192, 0),
      );

      // Add reentracy flag (FLAG_JMP)
      {
        let flags = offsetload(
          builder,
          ctx,
          meta.i32,
          vmctx,
          OffsetBytes::U(offset_of!(VMTaskState, flags) as _),
        );

        let finalized = LLVMBuildOr(
          builder,
          flags,
          LLVMConstInt(meta.i32, FLAG_JUMP_TO_RESUME as _, 0),
          c"new_flags".as_ptr(),
        );

        offsetstore(
          builder,
          ctx,
          finalized,
          vmctx,
          OffsetBytes::U(offset_of!(VMTaskState, flags) as _),
        );
      }

      let marker = meta.regmnt.counter().read_variable(builder);

      // marker = what we get
      offsetstore(
        builder,
        ctx,
        marker,
        vmctx,
        OffsetBytes::U(offset_of!(VMTaskState, curline_or_resume) as _),
      );

      // offset = JIT_CHECK
      offsetstore(
        builder,
        ctx,
        LLVMConstInt(meta.i32, OPCODE_JIT_CHECK as _, 0),
        vmctx,
        OffsetBytes::U(offset_of!(VMTaskState, opcode) as _),
      );

      // Sendback
      sendback::<true>(meta_ptr, builder, ctx, vmctx);

      LLVMBuildRetVoid(builder);
    }

    // Epilogue
    {
      LLVMPositionBuilderAtEnd(builder, meta.epilogue);

      offsetstore(
        builder,
        ctx,
        LLVMConstInt(meta.i32, OPCODE_OK as _, 0),
        vmctx,
        OffsetBytes::U(offset_of!(VMTaskState, opcode) as _),
      );

      // Sendback
      sendback::<SENDBACK>(meta_ptr, builder, ctx, vmctx);

      LLVMBuildRetVoid(builder);
    }

    meta.regmnt.finalize();
  }
}

fn backload(
  meta_ptr: *mut CompilerMeta,
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  vmctx: LLVMValueRef,
) {
  let regs = [0, 1, 2, 3, 4, 5, 6, 7usize];

  regs.into_iter().for_each(|regid| unsafe {
    let value = offsetload(
      builder,
      ctx,
      (*meta_ptr).i64,
      vmctx,
      OffsetBytes::U(size_of::<QuadPackedData>() as u64 * regid as u64),
    );

    (*meta_ptr).regmnt.setreg(regid, value);
  });
}

fn sendback<const SENDBACK: bool>(
  meta_ptr: *mut CompilerMeta,
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  vmctx: LLVMValueRef,
) {
  let regs = if SENDBACK {
    &[0, 1, 2, 3, 4, 5, 6, 7usize] as &[usize]
  } else {
    &[6, 7usize] as &[usize]
  };

  regs.into_iter().for_each(|&regid| {
    if let Some(regval) = unsafe { (*meta_ptr).regmnt.try_usereg(regid) } {
      offsetstore(
        builder,
        ctx,
        regval,
        vmctx,
        OffsetBytes::U(size_of::<QuadPackedData>() as u64 * regid as u64),
      );
    }
  });
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
) -> LLVMValueRef {
  unsafe {
    let (of, signed) = offset_bytes.into();
    let offset_ptr = offsetptr(builder, ctx, pointerval, of, signed);

    let store = LLVMBuildStore(builder, val, offset_ptr);

    if let Some(align) = align {
      LLVMSetAlignment(store, align);
    }

    store
  }
}

pub fn offsetstore(
  builder: LLVMBuilderRef,
  ctx: LLVMContextRef,
  val: LLVMValueRef,
  pointerval: LLVMValueRef,
  offset_bytes: OffsetBytes,
) -> LLVMValueRef {
  offsetstore_aligned(builder, ctx, val, pointerval, offset_bytes, None)
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
