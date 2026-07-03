use crate::{
  StringStore,
  mir::{block::instr::HLInstruction, function::ssa::ValueId},
};

pub mod instr;

pub struct Block<'a, T: StringStore> {
  pub store: &'a T,

  pub(crate) v0: bool,

  pub(crate) instr: Vec<HLInstruction<ValueId>>,
  pub(crate) preds: Vec<BlockId>,
  pub(crate) succ: Vec<BlockId>,

  pub(crate) params: Vec<ValueId>,
  pub(crate) man_params: usize,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);
