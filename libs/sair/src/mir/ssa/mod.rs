use std::collections::HashMap;

use crate::mir::{block::BlockId, function::ssa::ValueId, value::ValueTypeRef};

pub struct SSAResolver {
  pub typetag: ValueTypeRef,

  initval: ValueId,

  block_defs: HashMap<BlockId, ValueId, rapidhash::fast::RandomState>,
  phis: HashMap<BlockId, ValueId, rapidhash::fast::RandomState>,
}
