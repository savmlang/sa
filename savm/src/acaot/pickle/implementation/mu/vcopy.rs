use sart::ctr::VMTaskState;
use std::ptr;

use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};

pub fn call_vcopy(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let count_bit = pickle.u1;
  // memflags have no meaning here
  let _ = pickle.u2;

  let srcflags = pickle.u3;

  let src1flags = srcflags >> 4;
  let src2flags = srcflags & 0x0F;

  let count_data = arrcastint!(ws, start = 0, stop = 4, u32);

  let count = if count_bit == 0 {
    count_data
  } else {
    unsafe { taskstate.r1.u32 }
  };

  let baseoffset = arrcastint!(ws, start = 4, stop = 8, i32);
  let targetoffset = arrcastint!(ws, start = 8, stop = 12, i32);

  // Fetch Decode
  // vcopy is defined using COUNT
  let src1 =
    unsafe { (resolve_location_src!(taskstate => src1flags) as *mut u8).offset(baseoffset as _) };
  let target =
    unsafe { (resolve_location_src!(taskstate => src2flags) as *mut u8).offset(targetoffset as _) };

  unsafe { ptr::copy(src1, target, count as _) };
}
