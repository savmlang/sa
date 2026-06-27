use crate::{PickleJumpData, SaVMJumps};

pub struct SaVMJumpWrap(pub SaVMJumps);
pub struct SaVMJumpWrapRef<'a>(pub &'a SaVMJumps);

impl SaVMJumpWrapImpl for SaVMJumpWrap {
  fn arr_ref(&self) -> &[PickleJumpData] {
    self.0.as_ref()
  }
}

impl<'a> SaVMJumpWrapImpl for SaVMJumpWrapRef<'a> {
  fn arr_ref(&self) -> &[PickleJumpData] {
    self.0.as_ref()
  }
}

pub trait SaVMJumpWrapImpl {
  fn arr_ref(&self) -> &[PickleJumpData];

  fn get(&self, id: &u64) -> Option<usize> {
    let arr = self.arr_ref();
    let v = arr
      .binary_search_by_key(id, |&PickleJumpData { marker, .. }| marker)
      .ok()?;

    // SAFETY:
    // The binary search has already established
    // the validity of the index.
    Some(unsafe { arr.get_unchecked(v).loc })
  }
}
