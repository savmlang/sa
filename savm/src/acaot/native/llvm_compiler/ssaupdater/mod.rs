use crate::acaot::native::llvm_compiler::{
  CompilerMeta, LLVM_VAR_NAME,
  irgen::{OffsetBytes, offsetload},
  ssaupdater::ssavar::SsaResolver,
};
use llvm_sys::{
  LLVMValue,
  core::{
    LLVMBuildGEP2, LLVMBuildLoad2, LLVMConstInt, LLVMGetInsertBlock, LLVMGetLastInstruction,
    LLVMInt64TypeInContext, LLVMPointerTypeInContext, LLVMPositionBuilder,
    LLVMPositionBuilderAtEnd, LLVMPositionBuilderBefore,
  },
  prelude::{LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMTypeRef, LLVMValueRef},
};
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::{
  ffi::{c_char, c_void},
  hint::cold_path,
  mem::offset_of,
  ptr::NonNull,
};
pub mod ssavar;

pub type ValueManaged = SsaResolver;

pub struct VMRegManager {
  registers: [ValueManaged; 9],
  rcm: ReducedCompilerMeta,
}

macro_rules! regid {
  (
    $(
      $name:ident => $val:expr
    ),*
  ) => {
    $(
      pub static $name: usize = $val;
    )*
  };
}
regid! {
  REG_R1 => 0,
  REG_R2 => 1,
  REG_R3 => 2,
  REG_R4 => 3,
  REG_R5 => 4,
  REG_R6 => 5,
  REG_R7 => 6,
  REG_R8 => 7,
  LARGEPAD => 8
}

#[derive(Clone, Copy)]
pub struct ReducedCompilerMeta {
  pub prologue: LLVMBasicBlockRef,
  pub builder: LLVMBuilderRef,
  pub vmctx: LLVMValueRef,
  pub ctx: LLVMContextRef,
  pub i64: LLVMTypeRef,
  pub ptr: LLVMTypeRef,
  pub fnval: LLVMValueRef,
}

impl VMRegManager {
  pub fn new(compiler: ReducedCompilerMeta) -> Self {
    let initreg = |regid: usize| unsafe {
      let ReducedCompilerMeta {
        prologue,
        builder,
        vmctx,
        ctx,
        i64,
        ptr,
        ..
      } = compiler;

      Self::initreg(regid as _, vmctx, builder, ctx, prologue, i64, ptr)
    };

    Self {
      rcm: compiler,
      registers: [
        initreg(0),
        initreg(1),
        initreg(2),
        initreg(3),
        initreg(4),
        initreg(5),
        initreg(6),
        initreg(7),
        initreg(8),
      ],
    }
  }

  fn initreg(
    regof: usize,
    vmctx: LLVMValueRef,
    builder: LLVMBuilderRef,
    ctx: LLVMContextRef,
    prologue: LLVMBasicBlockRef,

    i64: LLVMTypeRef,
    ptr: LLVMTypeRef,
  ) -> SsaResolver {
    unsafe {
      let block = LLVMGetInsertBlock(builder);

      let ssa = {
        let prologue_last = LLVMGetLastInstruction(prologue);
        LLVMPositionBuilderBefore(builder, prologue_last);

        let (ty, offset_bytes) = match regof {
          0..8 => (
            LLVMInt64TypeInContext(ctx),
            regof * size_of::<QuadPackedData>(),
          ),
          8 => (
            LLVMPointerTypeInContext(ctx, 0),
            offset_of!(VMTaskState, largepad),
          ),
          _ => unreachable!("Unknown Values"),
        };
        let value0 = offsetload(builder, ctx, ty, vmctx, OffsetBytes::U(offset_bytes as _));

        SsaResolver::new(
          value0,
          match regof {
            0..8 => i64,
            8 => ptr,
            _ => unreachable!("Unable to get SSAValue"),
          },
        )
      };

      // Restore
      LLVMPositionBuilderAtEnd(builder, block);

      ssa
    }
  }

  pub fn setreg(&mut self, regid: usize, value: LLVMValueRef) {
    unsafe {
      let resolver = self
        .registers
        .get_mut(regid)
        .expect("Could not expect that rN error");

      resolver.write_variable(self.rcm.builder, value);
    }
  }

  pub fn try_usereg(&mut self, regid: usize) -> Option<LLVMValueRef> {
    let ssa = self
      .registers
      .get_mut(regid)
      .expect("Could not expect that rN error");

    unsafe { Some(ssa.read_variable(self.rcm.builder)) }
  }

  pub fn usereg(&mut self, regid: usize) -> LLVMValueRef {
    unsafe {
      let Some(out) = self.try_usereg(regid) else {
        unreachable!();
      };

      out
    }
  }

  pub fn finalize(&mut self) {
    self.registers.iter_mut().for_each(|x| {
      unsafe { x.fillphis(self.rcm.ctx, self.rcm.fnval) };
    });

    self.registers.iter_mut().for_each(|x| {
      unsafe { x.finalize(self.rcm.ctx, self.rcm.fnval) };
    });
  }
}
