use std::{
  ffi::c_void,
  ptr::{self, null_mut},
  thread::spawn,
};

use sart::ctr::VMTaskState;

use crate::{
  BytecodeResolver, ThreadSafe, VM,
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::spawn::{SPAWN, parse_spawn},
  },
  resolve_location_src,
};

pub extern "C" fn savm_spawn<T: BytecodeResolver + Send + Sync + 'static>(
  taskstate: *mut VMTaskState,
  section: u64,
  return_hwnd: bool,
) -> *mut c_void {
  unsafe {
    let safe_taskstate = ThreadSafe(taskstate);
    let vm = ThreadSafe((*taskstate).engine.pt as *mut VM<T>);

    let stdrt = spawn(move || {
      let vm = vm;
      let taskstate = safe_taskstate;

      let [r7, r8] = (*vm.0).fncall(section, taskstate.0);
      (r7.u64, r8.u64)
    });

    // Return HWND
    if return_hwnd {
      let rtptr = Box::into_raw(Box::new(stdrt));

      return rtptr as _;
    }
  };

  null_mut()
}

pub fn call_spawn<T: BytecodeResolver + Send + Sync + 'static>(
  pickle: &PickleInstruction,
  ws: *mut WorkingSet,
  taskstate: *mut VMTaskState,
) {
  unsafe {
    let SPAWN {
      out_loc,
      return_hwnd,
      section,
    } = parse_spawn(pickle, (*ws).arr.as_ref());

    let hwnd = resolve_location_src!(taskstate => out_loc);

    let newhwnd = savm_spawn::<T>(taskstate, section, return_hwnd);

    if !newhwnd.is_null() {
      ptr::write(hwnd as *mut *mut c_void, newhwnd);
    }
  }
}
