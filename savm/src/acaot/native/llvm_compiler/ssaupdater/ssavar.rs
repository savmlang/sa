#![allow(unsafe_op_in_unsafe_fn)]

use std::ffi::CStr;
use std::ptr::NonNull;

use crate::acaot::native::llvm_compiler::LLVM_CTX;
use crate::acaot::native::llvm_compiler::dispose::IRBuilder;
use ahash::HashMap;
use ahash::HashMapExt;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

unsafe extern "C" {
  unsafe fn llvm_get_num_preds(bb: LLVMBasicBlockRef) -> u64;
  unsafe fn llvm_get_preds(bb: LLVMBasicBlockRef, arr: *mut LLVMBasicBlockRef);
}

pub struct SsaResolver {
  ty: LLVMTypeRef,

  initval: LLVMValueRef,

  block_defs: HashMap<LLVMBasicBlockRef, LLVMValueRef>,
  phis: HashMap<LLVMBasicBlockRef, LLVMValueRef>,
}

impl SsaResolver {
  pub fn new(initval: LLVMValueRef, ty: LLVMTypeRef) -> Self {
    Self {
      ty,
      initval,
      block_defs: HashMap::new(),
      phis: HashMap::new(),
    }
  }

  pub unsafe fn write_variable(&mut self, builder: LLVMBuilderRef, value: LLVMValueRef) {
    let block = LLVMGetInsertBlock(builder);

    _ = self.block_defs.insert(block, value);
  }

  pub unsafe fn read_variable(&mut self, builder: LLVMBuilderRef) -> LLVMValueRef {
    let block = LLVMGetInsertBlock(builder);
    let phis = &mut self.phis;

    *self.block_defs.entry(block).or_insert_with(|| {
      let poisoned = LLVMBuildFreeze(builder, LLVMGetPoison(self.ty), c"ssatemp".as_ptr());

      _ = phis.insert(block, poisoned);

      poisoned
    })
  }

  pub unsafe fn fillphis(&mut self, ctx: LLVMContextRef, fnval: LLVMValueRef) {
    let builder_raii = IRBuilder(LLVMCreateBuilderInContext(ctx));
    let builder = builder_raii.0;

    let prologue = LLVMGetEntryBasicBlock(fnval);

    // Push the prologue value
    _ = self.block_defs.insert(prologue, self.initval);

    // Populate a POISON PHI in all the blocks (except prologue)
    {
      let mut knot = prologue;

      let defs = &mut self.block_defs;
      while let Some(pt) = NonNull::new(LLVMGetNextBasicBlock(knot)) {
        let blk = pt.as_ptr();

        let first_instr = LLVMGetFirstInstruction(blk);
        if first_instr.is_null() {
          LLVMPositionBuilderAtEnd(builder, blk);
        } else {
          LLVMPositionBuilderBefore(builder, first_instr);
        }

        _ = self.phis.entry(blk).or_insert_with(|| {
          let poisoned = LLVMBuildFreeze(builder, LLVMGetPoison(self.ty), c"ssafill".as_ptr());

          defs.entry(blk).or_insert(poisoned);

          poisoned
        });

        knot = blk;
      }
    }

    drop(builder_raii);
  }

  pub unsafe fn finalize(&mut self, ctx: LLVMContextRef, fnval: LLVMValueRef) {
    let builder_raii = IRBuilder(LLVMCreateBuilderInContext(ctx));
    let builder = builder_raii.0;

    let prologue = LLVMGetEntryBasicBlock(fnval);

    // Generate PHI Blocks for required ones
    {
      let mut preds = vec![];

      let mut values = vec![];
      let mut blocks = vec![];

      // Collect Predecessors & Process
      let mut knot = prologue;
      while let Some(pt) = NonNull::new(LLVMGetNextBasicBlock(knot)) {
        let blk = pt.as_ptr();

        // Reserve required space + Write predeccesors
        {
          let num = llvm_get_num_preds(blk);
          preds.reserve(num as _);
          values.reserve(num as _);
          blocks.reserve(num as _);

          llvm_get_preds(blk, preds.as_mut_ptr());

          preds.set_len(num as _);
        }

        // Correct the PHI Nodes now...
        if !preds.is_empty() {
          let ourphi = *self.phis.get(&blk).unwrap();

          let first_instr = LLVMGetFirstInstruction(blk);
          if first_instr.is_null() {
            LLVMPositionBuilderAtEnd(builder, blk);
          } else {
            LLVMPositionBuilderBefore(builder, first_instr);
          }

          let correctphi = LLVMBuildPhi(builder, self.ty, c"ssavar.resolve".as_ptr());

          // PHI up the pred's values
          for &pred in &preds {
            let &predval = self.block_defs.get(&pred).unwrap();
            values.push(predval);
            blocks.push(pred);
          }

          LLVMAddIncoming(
            correctphi,
            values.as_mut_ptr(),
            blocks.as_mut_ptr(),
            values.len() as _,
          );

          values.clear();
          blocks.clear();

          LLVMReplaceAllUsesWith(ourphi, correctphi);
          LLVMInstructionEraseFromParent(ourphi);

          if let Some(x) = self.block_defs.get_mut(&blk) {
            if *x == ourphi {
              *x = correctphi;
            }
          }
        }

        knot = blk;
      }
    }

    drop(builder_raii);
  }
}
