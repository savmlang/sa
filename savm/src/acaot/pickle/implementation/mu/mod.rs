//! Memory Unit

use std::ptr::{null_mut, replace};

use sart::ctr::VMTaskState;

use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  resolve,
};

mod vcopy;
pub use vcopy::*;

mod cast;
pub use cast::*;

mod vops;
pub use vops::*;

mod vbit;
pub use vbit::*;

pub fn call_scratch(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let op_class = pickle.u1;

  let payload = u16::from_ne_bytes([pickle.u2, pickle.u3]);

  match op_class {
    // Allocate
    // `[padding (6-bits)] size_reg[4-bits] align_reg[4-bits]`
    0b00 => {
      let size_reg = (payload as u8 >> 4);
      let align_reg = (payload as u8 & 0x0F);

      unsafe {
        let size = resolve!(taskstate => size_reg).u64;
        let align = resolve!(taskstate => align_reg).u64;

        debug_assert!(taskstate.largepad.is_null());
        debug_assert!(align == 0 || align.is_power_of_two());

        taskstate.largepad = ws.allocate(size, align);
      }
    }
    // Drop classic
    0b01 => unsafe {
      let pt = taskstate.largepad;
      taskstate.largepad = null_mut();

      ws.free(pt);
    },
    // Drop (alignment was given at alloc)
    0b10 => unsafe {
      let pt = taskstate.largepad;
      taskstate.largepad = null_mut();

      ws.salloc_free(pt);
    },
    _ => unreachable!(),
  }
}
