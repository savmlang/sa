use crate::{
  StringStore,
  mir::{
    Module,
    block::instr::loc::{LocSrc, VMRegister},
    function::{Function, ssa::ValueId},
    value::{Alignment, ValueType, ValueTypeArray, ValueTypeRef},
  },
};
use std::{fmt::Debug, rc::Rc};

pub mod v0;

pub trait TargetVM: Debug {
  type T: StringStore;

  fn regalloc(&self, func: &Function<'_, Self::T>, module: &Module<'_, Self::T>);
}

pub(crate) fn sabi_map<T: StringStore>(
  args: &[ValueId],
  func: &Function<'_, T>,
  module: &Module<'_, T>,
) -> Option<LocSrc> {
  let mut output = None;

  let composition: Rc<[ValueTypeRef]> = Rc::from(
    args
      .iter()
      .map(|x| unsafe { func.ssa.get_unchecked(x.0) }.typetag)
      .collect::<Box<[_]>>(),
  );

  // We create a full struct based on the data actually
  let argstruct = ValueType::Composite {
    align: composition
      .as_ref()
      .iter()
      .filter_map(|&x| module.type_data(x).map(|x| x.align(module)))
      .max()
      .map(Alignment::parse),
    composition: ValueTypeArray::Rc(composition),
  };

  let modsize = argstruct.size(module);
  match modsize {
    0 => {}
    1..=8 => {
      output = Some(LocSrc {
        offset: 0,
        reg: VMRegister::R7,

        count: 1,
        width: 8,
      });
    }
    9..=16 => {
      output = Some(LocSrc {
        offset: 0,
        reg: VMRegister::R7,

        count: 2,
        width: 8,
      });
    }
    _ => {
      output = Some(LocSrc {
        offset: 0,
        reg: VMRegister::Scratchpad,

        count: 2,
        width: argstruct.size(module),
      });
    }
  }

  output
}
