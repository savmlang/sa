#![allow(dead_code, unused)]

use crate::mir::function::ssa::ValueId;

#[derive(Debug, Default)]
pub struct BlockAlloc {
  r1: Reg,
  r2: Reg,
  r3: Reg,
  r4: Reg,

  r5: Reg,
  r6: Reg,
  r7: Reg,
  r8: Reg,
}

impl BlockAlloc {
  pub fn clear(&mut self) {
    for n in [
      &mut self.r1,
      &mut self.r2,
      &mut self.r3,
      &mut self.r4,
      &mut self.r5,
      &mut self.r6,
      &mut self.r7,
      &mut self.r8,
    ] {
      n.clear();
    }
  }
}

#[derive(Debug, Default)]
pub struct Reg {
  values: Vec<ValueId>,
}

impl Reg {
  pub fn storable() {}

  pub fn clear(&mut self) {
    self.values.clear();
  }
}
