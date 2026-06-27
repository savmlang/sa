use std::fmt::Debug;

use crate::{StringStore, mir::value::ValueTypeRef};

pub struct SSA<'a, T: StringStore> {
  pub(crate) _parent: &'a T,

  pub typetag: ValueTypeRef,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(pub(crate) usize);

impl Debug for ValueId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "#{}", self.0)
  }
}
