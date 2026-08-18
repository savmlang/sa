use std::{collections::HashSet, fmt::Formatter};

use crate::{
  StringStore,
  mir::{block::instr::HLInstruction, function::ssa::ValueId},
};
use rapidhash::fast::RandomState;

pub mod instr;

pub struct Block<'a, T: StringStore> {
  pub store: &'a T,

  pub(crate) v0: bool,

  pub(crate) instr: Vec<HLInstruction<ValueId>>,
  pub(crate) preds: HashSet<BlockId, RandomState>,
  pub(crate) succ: HashSet<BlockId, RandomState>,

  pub(crate) params: Vec<ValueId>,
}

impl<'a, T: StringStore> Block<'a, T> {
  pub fn params(&self) -> &[ValueId] {
    &self.params
  }

  pub fn instructions(&self) -> &[HLInstruction<ValueId>] {
    &self.instr
  }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub(crate) usize);

pub const BLOCK_0: BlockId = BlockId(0);

impl BlockId {
  pub(crate) fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}
