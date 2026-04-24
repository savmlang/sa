//! Arithmatic, Logic, Memory Unit

use std::ptr::null_mut;

use sart::ctr::VMTaskState;

use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::corevm::{SCRATCH, parse_scratch},
  },
  resolve,
};

macro_rules! import {
  (
    $($name:ident),*
  ) => {
    $(
      mod $name;
      pub use $name::*;
    )*
  };
}

import! {
  atomic,
  cast,
  fp,
  vbit,
  vcnt,
  vcopy,
  vfma,
  vfop,
  vops,
  vrot,
  vminimax,
  vsh,
  vau
}

pub fn call_scratch(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let scratch = parse_scratch(pickle, ws.arr.as_ref());

  match scratch {
    SCRATCH::Allocate {
      size_reg,
      align_reg,
    } => unsafe {
      let size = resolve!(taskstate => size_reg).u64 as usize;
      let align = resolve!(taskstate => align_reg).u64 as usize;

      debug_assert!(taskstate.largepad.is_null());
      debug_assert!(align == 0 || align.is_power_of_two());

      taskstate.largepad = ws.allocate(size, align);
    },
    // Drop classic
    SCRATCH::DropClassic => {
      let pt = taskstate.largepad;
      taskstate.largepad = null_mut();

      ws.free(pt);
    }
    // Drop (alignment was given at alloc)
    SCRATCH::DropAligned => {
      let pt = taskstate.largepad;
      taskstate.largepad = null_mut();

      ws.salloc_free(pt);
    }
    _ => unreachable!(),
  }
}
