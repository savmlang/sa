use crate::acaot::native::llvm_compiler::{
  CompilerMeta, LLVM_VAR_NAME,
  irgen::{OffsetBytes, offsetload},
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

pub type SSAUpdaterC = *mut c_void;

unsafe extern "C" {
  unsafe fn create_ssaupdater() -> SSAUpdaterC;
  unsafe fn ssaupdater_init(updater: SSAUpdaterC, typedata: LLVMTypeRef, name: *const c_char);
  unsafe fn ssaupdater_def(
    updater: SSAUpdaterC,
    basicblock: LLVMBasicBlockRef,
    value: LLVMValueRef,
  );
  unsafe fn ssaupdater_get(updater: SSAUpdaterC, basicblock: LLVMBasicBlockRef) -> LLVMValueRef;
  unsafe fn ssaupdater_free(updater: SSAUpdaterC);
}

pub struct SSAUpdaterGuard(NonNull<c_void>);

impl SSAUpdaterGuard {
  fn getref(&self) -> SSAUpdaterC {
    self.0.as_ptr()
  }

  pub fn create(typedata: LLVMTypeRef, name: *const c_char) -> Self {
    unsafe {
      let updater = create_ssaupdater();
      ssaupdater_init(updater, typedata, name);

      Self(NonNull::new_unchecked(updater))
    }
  }

  pub fn def(&mut self, basicblock: LLVMBasicBlockRef, value: LLVMValueRef) {
    unsafe {
      let updater = self.getref();
      ssaupdater_def(updater, basicblock, value);
    }
  }

  pub fn get(&mut self, basicblock: LLVMBasicBlockRef) -> NonNull<LLVMValue> {
    unsafe {
      let updater = self.getref();
      NonNull::new_unchecked(ssaupdater_get(updater, basicblock))
    }
  }
}

pub type StoredRegValue = Option<NonNull<LLVMValue>>;
pub type ValueManaged = Option<(SSAUpdaterGuard, StoredRegValue)>;

pub struct VMRegManager {
  block: LLVMBasicBlockRef,
  registers: [ValueManaged; 9],
  reg_desync: u16,
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

impl VMRegManager {
  pub fn new(block: LLVMBasicBlockRef) -> Self {
    Self {
      block,
      registers: [const { None }; 9],
      reg_desync: 0,
    }
  }

  pub fn init_largepad(&mut self, compiler: *mut CompilerMeta) {
    unsafe {
      if let Some(_) = self.registers[8].as_mut() {
        unreachable!();
      }

      let prologue = (*compiler).prologue;
      let builder = (*compiler).builder;
      let vmctx = (*compiler).vmctx;
      let ctx = (*compiler).llvmctx;
      let ptr = (*compiler).ptr;

      let mut ssa = SSAUpdaterGuard::create(ptr, LLVM_VAR_NAME.0);

      Self::initreg(8, &mut ssa, vmctx, builder, ctx, prologue);
    }
  }

  fn initreg(
    regof: usize,
    ssa: &mut SSAUpdaterGuard,
    vmctx: LLVMValueRef,
    builder: LLVMBuilderRef,
    ctx: LLVMContextRef,
    prologue: LLVMBasicBlockRef,
  ) {
    unsafe {
      let block = LLVMGetInsertBlock(builder);

      {
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

        ssa.def(prologue, value0);
      }

      // Restore
      LLVMPositionBuilderAtEnd(builder, block);
    }
  }

  pub fn setreg(&mut self, regid: usize, value: LLVMValueRef, compiler: *mut CompilerMeta) {
    unsafe {
      let valueptr = Some(NonNull::new_unchecked(value));

      let Some((ssaup, regval)) = self
        .registers
        .get_mut(regid)
        .expect("Could not expect that rN error")
      // Register uninit
      else {
        let prologue = (*compiler).prologue;
        let builder = (*compiler).builder;
        let vmctx = (*compiler).vmctx;
        let ctx = (*compiler).llvmctx;
        let i64 = (*compiler).i64;
        let ptr = (*compiler).ptr;

        let mut ssa = SSAUpdaterGuard::create(
          match regid {
            0..8 => i64,
            8 => ptr,
            _ => unreachable!("Unable to get SSAValue"),
          },
          LLVM_VAR_NAME.0,
        );

        Self::initreg(regid as _, &mut ssa, vmctx, builder, ctx, prologue);

        // We do NOT sync the values here
        *self
          .registers
          .get_mut(regid)
          .expect("Could not expect that rN error") = Some((ssa, valueptr));
        self.reg_desync |= 1 << regid;

        return;
      };

      *regval = valueptr;

      self.reg_desync |= 1 << regid;
    }
  }

  pub fn try_usereg(&mut self, regid: usize, compiler: *mut CompilerMeta) -> Option<LLVMValueRef> {
    let (ssaup, regval) = self
      .registers
      .get_mut(regid)
      .expect("Could not expect that rN error")
      .as_mut()?;

    if let Some(r) = regval {
      let regval: LLVMValueRef = r.as_ptr();
      return Some(regval);
    }

    Some(ssaup.get(self.block).as_ptr())
  }

  pub fn usereg(&mut self, regid: usize, compiler: *mut CompilerMeta) -> LLVMValueRef {
    unsafe {
      let Some(out) = self.try_usereg(regid, compiler) else {
        let prologue = (*compiler).prologue;
        let builder = (*compiler).builder;
        let vmctx = (*compiler).vmctx;
        let ctx = (*compiler).llvmctx;
        let i64 = (*compiler).i64;
        let ptr = (*compiler).ptr;

        let mut ssa = SSAUpdaterGuard::create(
          match regid {
            0..8 => i64,
            8 => ptr,
            _ => unreachable!("Unable to get SSAValue"),
          },
          LLVM_VAR_NAME.0,
        );

        Self::initreg(regid as _, &mut ssa, vmctx, builder, ctx, prologue);

        let val = ssa.get(self.block);
        *self
          .registers
          .get_mut(regid)
          .expect("Could not expect that rN error") = Some((ssa, Some(val)));

        return val.as_ptr();
      };

      out
    }
  }

  pub fn newblock(&mut self, newblock: LLVMBasicBlockRef) {
    self.sync_internal::<true>();
    self.block = newblock;
  }

  pub fn sync(&mut self) {
    self.sync_internal::<false>();
  }

  fn sync_internal<const CLEAR: bool>(&mut self) {
    if self.reg_desync == 0 && !CLEAR {
      cold_path();
      return;
    }

    self.registers.iter_mut().enumerate().for_each(|(id, x)| {
      // Clear the local values of all regs used
      if let Some((ssa, regvalue)) = x.as_mut() {
        if let Some(regvalue) = regvalue.as_ref() {
          if (self.reg_desync & (1 << id) != 0) {
            ssa.def(self.block, regvalue.as_ptr());
          }
        }

        if CLEAR {
          _ = regvalue.take();
        }
      }
    });
    self.reg_desync = 0;
  }
}

impl Drop for VMRegManager {
  fn drop(&mut self) {
    if self.reg_desync != 0 {
      cold_path();
      self.sync_internal::<true>();
    }
  }
}

impl Drop for SSAUpdaterGuard {
  fn drop(&mut self) {
    unsafe {
      ssaupdater_free(self.getref());
    }
  }
}
