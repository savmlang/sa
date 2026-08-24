use llvm_sys::{core::LLVMConstVector, prelude::LLVMValueRef};

pub fn vectorize(val: LLVMValueRef, count: u32) -> LLVMValueRef {
  if count == 1 {
    val
  } else {
    unsafe { LLVMConstVector(vec![val; count as usize].as_mut_ptr(), count) }
  }
}
