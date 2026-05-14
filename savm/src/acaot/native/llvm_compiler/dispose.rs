use llvm_sys::{
  LLVMContext, LLVMModule,
  core::{LLVMContextDispose, LLVMDisposeMessage, LLVMDisposeModule},
  target::{LLVMDisposeTargetData, LLVMTargetDataRef},
  target_machine::{LLVMDisposeTargetMachine, LLVMOpaqueTargetMachine},
};
use std::ffi::c_char;

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
  LLVMMsg(*mut c_char, LLVMDisposeMessage)
}
