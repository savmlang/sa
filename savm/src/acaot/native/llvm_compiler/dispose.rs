use core::slice;
use llvm_sys::{
  LLVMContext, LLVMModule,
  core::{
    LLVMContextDispose, LLVMDisposeBuilder, LLVMDisposeMemoryBuffer, LLVMDisposeMessage,
    LLVMDisposeModule, LLVMGetBufferSize, LLVMGetBufferStart,
  },
  prelude::{LLVMBuilderRef, LLVMMemoryBufferRef},
  target::{LLVMDisposeTargetData, LLVMTargetDataRef},
  target_machine::{LLVMDisposeTargetMachine, LLVMOpaqueTargetMachine},
  transforms::pass_builder::{LLVMDisposePassBuilderOptions, LLVMPassBuilderOptionsRef},
};
use std::{
  ffi::{CStr, c_char},
  ops::Deref,
};

macro_rules! llvmdispose {
  (
    $(
      $name:ident($t0:ty, $free:ident)
    ),*
  ) => {
    $(
      pub struct $name(pub $t0);

      impl $name {
        pub fn as_ref(&self) -> $t0 {
          self.0
        }

        pub fn as_mut_ref(&mut self) -> *mut $t0 {
          &mut self.0
        }
      }

      impl Drop for $name {
        fn drop(&mut self) {
          if !self.0.is_null() {
            unsafe {
              $free(self.0)
            };
          }
        }
      }
    )*
  };
}

llvmdispose! {
  LLVMCtx(*mut LLVMContext, LLVMContextDispose),
  Module(*mut LLVMModule, LLVMDisposeModule),
  OpaqueMachine(*mut LLVMOpaqueTargetMachine, LLVMDisposeTargetMachine),
  OpaqueTargetData(LLVMTargetDataRef, LLVMDisposeTargetData),
  LLVMMsg(*mut c_char, LLVMDisposeMessage),
  LLVMBuffer(LLVMMemoryBufferRef, LLVMDisposeMemoryBuffer),
  IRBuilder(LLVMBuilderRef, LLVMDisposeBuilder),
  PassBuilderOptions(LLVMPassBuilderOptionsRef, LLVMDisposePassBuilderOptions)
}

impl Deref for LLVMMsg {
  type Target = CStr;

  fn deref(&self) -> &Self::Target {
    unsafe { CStr::from_ptr(self.0) }
  }
}

impl Deref for LLVMBuffer {
  type Target = [u8];

  fn deref(&self) -> &Self::Target {
    unsafe {
      let begin = LLVMGetBufferStart(self.0) as *const u8;
      let len = LLVMGetBufferSize(self.0);

      slice::from_raw_parts(begin, len)
    }
  }
}
