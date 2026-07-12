use crate::{
  StringStore,
  mir::{
    block::{BlockId, instr::HLInstruction},
    function::{
      builder::{FunctionBuilder, InstId},
      ssa::ValueId,
    },
    value::{ValueType, ValueTypeRef},
  },
};

#[macro_use]
mod macrodata;

impl<'a, 'b, T: StringStore> FunctionBuilder<'a, 'b, T> {
  fn jmpverify(&mut self, block: BlockId, args: &[ValueId]) -> Result<(), CommonError> {
    let block = unsafe { self.parent.blocks.get_unchecked(block.0) };

    if block.params.len() != args.len() {
      return Err(CommonError::InvalidBlockArgs);
    }

    let succ = block
      .params
      .iter()
      .zip(args.iter())
      .map(|(&a, &b)| {
        (
          self.parent.get_ssa(a).unwrap(),
          self.parent.get_ssa(b).unwrap(),
        )
      })
      .all(|(a, b)| a.typetag == b.typetag);

    if !succ {
      return Err(CommonError::InvalidBlockArgs);
    }

    Ok(())
  }

  implement! {
    // Arithmatic
    fn vadd(ctx, a, b) -> ValueId {
      verify: {
        typecheck!(ctx, a, b { is_num });
      },
      process: {
        let (vtype, _) = ctx.type_of(a);
        let out = ctx.define_ssa(vtype);
        let (_, t) = ctx.type_of(a);
        let id = if t.is_float() {
          ctx.inst_process(HLInstruction::VAddf { a, b, out })
        } else {
          ctx.inst_process(HLInstruction::Vadd { src1: a, src2: b, out })
        };


        Instruction { id, out }
      }
    }
    fn vsub(ctx, a, b) -> ValueId {
      verify: {
        typecheck!(ctx, a, b { is_num });
      },
      process: {
        let (vtype, _) = ctx.type_of(a);
        let out = ctx.define_ssa(vtype);
        let (_, t) = ctx.type_of(a);
        let id = if t.is_float() {
          ctx.inst_process(HLInstruction::VSubf { a, b, out })
        } else {
          ctx.inst_process(HLInstruction::VSub { src1: a, src2: b, out })
        };


        Instruction { id, out }
      }
    }
    fn vmul(ctx, a, b) -> ValueId {
      verify: {
        typecheck!(ctx, a, b { is_num });
      },
      process: {
        let (vtype, _) = ctx.type_of(a);
        let out = ctx.define_ssa(vtype);
        let (_, t) = ctx.type_of(a);
        let id = if !t.is_float() {
          ctx.inst_process(HLInstruction::VMulLo { src1: a, src2: b, out })
        } else {
          ctx.inst_process(HLInstruction::VMulf { a, b, out })
        };


        Instruction { id, out }
      }
    }
    fn div(ctx, a, b) -> ValueId {
      verify: {
        typecheck!(ctx, a, b { is_int });
        typecheck!(ctx, a, b { is_scalar });
      },
      process: {
        let (vtype, _) = ctx.type_of(a);
        let out = ctx.define_ssa(vtype);
        let id = ctx.inst_process(HLInstruction::Div { src: a, divisor: b, out });


        Instruction { id, out }
      }
    }
    fn vdivf(ctx, a, b) -> ValueId {
      verify: {
        typecheck!(ctx, a, b { is_float });
      },
      process: {
        let (vtype, _) = ctx.type_of(a);
        let out = ctx.define_ssa(vtype);
        let id = ctx.inst_process(HLInstruction::VDivf { a, b, out });


        Instruction { id, out }
      }
    }

    // Control Flow
    fn jump(ctx) {
      immediates {
        block: BlockId,
        args: &[ValueId]
      }
      verify: {
        Self::jmpverify(ctx, block, args)?;
      },
      process: {
        let id = ctx.inst_process(HLInstruction::Jump { block, args: Box::from(args) });
        // Add to the index (to help regalloc reserve stuff)
        unsafe {
          _ = ctx.parent.blocks.get_unchecked_mut(ctx.currblock.0).succ.insert(block);
          _ = ctx.parent.blocks.get_unchecked_mut(block.0).preds.insert(ctx.currblock);
        }

        Instruction { id, out: () }
      }
    }

    /// Initialize a constant integer
    fn iconst(ctx) -> ValueId {
      immediates {
        intty: ValueTypeRef,
        value: u64
      }
      verify: {
        let tt = ctx.module.type_data(intty).unwrap();
        if !(tt.is_int() && tt.is_scalar()) {
          return Err(CommonError::TypeVerificationFailure);
        }
      },
      process: {
        let tt = ctx.module.type_data(intty).unwrap();

        let typedata = match tt {
          &ValueType::Base { base, .. } => base,
          _ => unreachable!()
        };

        let out = ctx.define_ssa(intty);
        let id = ctx.inst_process(HLInstruction::Set { out, typedata, value });

        Instruction { id, out }
      }
    }
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum CommonError {
  TypeVerificationFailure,
  InvalidBlockArgs,
}

pub struct Instruction<T> {
  pub id: InstId,
  pub out: T,
}

pub type InstructionId = InstId;
