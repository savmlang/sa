use crate::{
  StringStore,
  llir::instr::loc::VMLoc,
  mir::{
    Module,
    block::instr::loc::LocSrc,
    function::{Function, ssa::ValueId},
    value::{Alignment, ValueType, ValueTypeArray, ValueTypeRef},
  },
};
use std::{fmt::Debug, rc::Rc, marker::PhantomData};

pub mod v0;

pub struct DummyTGTVM<T: StringStore>(pub PhantomData<T>);

impl<T: StringStore> Debug for DummyTGTVM<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "dummy-dummy-dummy")
  }
}

impl<T: StringStore> TargetVM for DummyTGTVM<T> {
  type T = T;

  fn regalloc(&self, func: &Function<'_, Self::T>, module: &Module<'_, Self::T>) -> crate::mir::regalloc::RegAllocResult {
    crate::mir::regalloc::allocate(func, module)
  }
}

pub trait TargetVM: Debug {
  type T: StringStore;

  fn regalloc(&self, func: &Function<'_, Self::T>, module: &Module<'_, Self::T>) -> crate::mir::regalloc::RegAllocResult;
}

#[allow(dead_code)]
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
        reg: VMLoc::R7,

        count: 1,
        width: 8,
      });
    }
    9..=16 => {
      output = Some(LocSrc {
        offset: 0,
        reg: VMLoc::R7,

        count: 2,
        width: 8,
      });
    }
    _ => {
      let size = argstruct.size(module);

      // Scratchpad has a max size of 192 bytes; overflow uses Largepad
      if size > 192 {
        output = Some(LocSrc {
          offset: 0,
          reg: VMLoc::Largepad,

          count: 1,
          width: size,
        });
      } else {
        output = Some(LocSrc {
          offset: 0,
          reg: VMLoc::Scratchpad,

          count: 1,
          width: size,
        });
      }
    }
  }

  output
}
