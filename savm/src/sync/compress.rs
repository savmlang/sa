#![allow(dead_code)]

use crate::sync::VMState;
use sart::{ctr::VMTaskState, structures::QuadPackedData};
use std::{ptr, slice};

/// This is currently a non exhaustive way of
/// storing a VMState (a 6528B struct) in compressed
/// way
///
/// This only stores frames upto the current index
/// discarding redundant or old ones
pub(crate) struct CompressedTaskState {
  taskstate: Vec<VMTaskState>,
  scratchpad: Vec<QuadPackedData>,
}

impl CompressedTaskState {
  pub fn new() -> Self {
    Self {
      // Make an allocation for at least 1 VMTaskState
      taskstate: Vec::with_capacity(1),
      scratchpad: Vec::with_capacity(24),
    }
  }
}

pub(crate) fn compresss(vmstate: *mut VMState, compress: &mut CompressedTaskState) {
  unsafe {
    debug_assert!(
      (*vmstate).cindex < 50,
      "cindex exceeds max VM stack limit of 50!"
    );
    compress.taskstate.clear();
    let len = (*vmstate).cindex + 1;

    if len != 0 {
      // Copy Scratchpad
      // Scratchpad is a large continuous region
      let scratchpad = {
        compress.scratchpad.clear();
        compress.scratchpad.extend_from_slice(slice::from_raw_parts(
          (*vmstate).ts[0].scratchpad as *const _,
          len * 24,
        ));

        compress.scratchpad.as_mut_ptr()
      };

      // Copy VMTaskState
      compress.taskstate.extend(
        slice::from_raw_parts((*vmstate).ts.as_ptr(), len)
          .iter()
          .enumerate()
          .map(|(i, x)| {
            let mut v = *x;

            v.scratchpad = scratchpad.add(i * 24);

            v
          }),
      );
    }
  }
}

pub(crate) fn hydrate(compress: &CompressedTaskState, vmstate: *mut VMState) {
  unsafe {
    let len = compress.taskstate.len();

    debug_assert!(
      len <= 50,
      "Attempted to hydrate a token containing more than 50 frames!"
    );
    (*vmstate).cindex = len.saturating_sub(1);

    // Copy Scratchpad
    // It is guaranteed that scratchpad is a large continuous region
    {
      ptr::copy_nonoverlapping(
        compress.scratchpad.as_ptr() as _,
        (*vmstate).ts[0].scratchpad,
        len * 24,
      );
    }

    let scratchpad_base = (*vmstate).ts[0].scratchpad;
    // Copy VMTaskState
    for (id, compressed, new) in compress
      .taskstate
      .iter()
      .zip(slice::from_raw_parts_mut((*vmstate).ts.as_mut_ptr(), len))
      .enumerate()
      .map(|(a, (b, c))| (a, b, c))
    {
      *new = *compressed;
      new.scratchpad = scratchpad_base.add(id * 24);
    }
  }
}
