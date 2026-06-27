use std::collections::HashMap;

use rapidhash::{HashMapExt, RapidHashMap};

use crate::{
  StringStore,
  mir::{
    Module,
    block::{Block, instr::HLInstruction},
    function::{
      Function,
      ssa::{SSA, ValueId},
    },
    ssa::SSAResolver,
    value::ValueTypeRef,
  },
};

pub struct FunctionBuilder<'a, 'b, T: StringStore> {
  pub(crate) parent: &'b mut Function<'a, T>,

  pub(crate) module: &'b Module<'a, T>,

  currblock: BlockId,
  instr: InstId,

  pub hmap: Vec<SSAResolver>,
}

impl<'a, 'b, T: StringStore> FunctionBuilder<'a, 'b, T> {
  pub(crate) fn new(parent: &'b mut Function<'a, T>, module: &'b Module<'a, T>) -> Self {
    let sig = module.sigs.get(parent.sig.0).unwrap();

    let empty_blocks = parent.blocks.is_empty();

    let mut out = Self {
      parent,
      module,

      currblock: BlockId(0),
      instr: InstId(0),

      hmap: Vec::with_capacity(12),
    };

    if empty_blocks {
      if let Some(arg) = sig.args {
        out.block(&[arg]);
      } else {
        out.block(&[]);
      }
    }

    out
  }

  /// Creates a new Block with the given DEFAULT params
  pub fn block(&mut self, params: &[ValueTypeRef]) -> BlockId {
    let params = params
      .iter()
      .map(|&typetag| self.define_ssa(typetag))
      .collect::<Vec<_>>();

    let newid = self.parent.blocks.len();
    self.parent.blocks.push(Block {
      store: self.parent.store,
      instr: Vec::with_capacity(16),

      preds: vec![],
      succ: vec![],

      man_params: params.len(),
      params,
    });

    BlockId(newid)
  }

  /// Get the total [HLInstruction] at the current moment for the supplied block id
  pub fn block_inst(
    &mut self,
    block: BlockId,
  ) -> Option<impl Iterator<Item = (InstId, &HLInstruction<ValueId>)>> {
    self.parent.blocks.get(block.0).map(|x| {
      x.instr
        .as_slice()
        .iter()
        .enumerate()
        .map(|(idx, i)| (InstId(idx), i))
    })
  }

  /// Get the total [HLInstruction]s at the current moment for the supplied block id
  pub fn block_total_inst(&mut self, block: BlockId) -> Option<usize> {
    self.parent.blocks.get(block.0).map(|x| x.instr.len())
  }

  /// Position builder at the end of the block.
  pub fn position_end(&mut self, block: BlockId) -> Option<()> {
    self.currblock = block;
    self.instr = InstId({ self.parent.blocks.get(block.0)? }.instr.len());

    Some(())
  }

  /// Position builder at the Instruction specified by its ID
  pub fn position_at(&mut self, block: BlockId, inst: InstId) -> Option<()> {
    self.currblock = block;

    let max = { self.parent.blocks.get(block.0)? }.instr.len();

    if inst.0 > max {
      return None;
    }

    self.instr = inst;
    Some(())
  }

  pub(crate) fn define_ssa(&mut self, typetag: ValueTypeRef) -> ValueId {
    let idx = self.parent.ssa.len();
    self.parent.ssa.push(SSA {
      _parent: self.parent.store,
      typetag,
    });

    ValueId(idx)
  }

  // TODO: Will define purposes later!
  pub(crate) fn inst_process(&mut self, inst: HLInstruction<ValueId>) {
    self.instr.0 += 1;

    unsafe { self.parent.blocks.get_unchecked_mut(self.currblock.0) }
      .instr
      .push(inst);
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(pub(crate) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstId(pub(crate) usize);
