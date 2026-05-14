use sart::ctr::VMTaskState;
use std::ptr;

use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::corevm::{VCOPY, parse_vcopy},
  },
  resolve_location_src,
};

pub fn call_vcopy(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let VCOPY {
    src,
    target,
    count,
    src_offset,
    target_offset,
    ..
  } = parse_vcopy(pickle, unsafe { (*ws).arr }.as_ref());

  // vcopy is defined using COUNT
  let src1 =
    unsafe { (resolve_location_src!(taskstate => src) as *mut u8).offset(src_offset as _) };
  let target =
    unsafe { (resolve_location_src!(taskstate => target) as *mut u8).offset(target_offset as _) };

  unsafe { ptr::copy(src1, target, count.get(taskstate) as _) };
}
