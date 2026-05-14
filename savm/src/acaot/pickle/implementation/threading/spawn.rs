use std::{
  ffi::c_void,
  ptr::{self, null_mut},
  thread::spawn,
};

use sart::ctr::VMTaskState;

use crate::{
  GLOBAL_RUNTIME, ThreadSafe, VM,
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::spawn::{SPAWN, parse_spawn},
  },
  resolve_location_src,
};

pub extern "C" fn savm_spawn(
  taskstate: *mut VMTaskState,
  section: u64,
  launch_async: bool,
  return_hwnd: bool,
) -> *mut c_void {
  unsafe {
    let safe_taskstate = ThreadSafe(taskstate);
    let vm = ThreadSafe((*taskstate).engine_or_pt.pt as *mut VM);

    if launch_async {
      let tokiort = GLOBAL_RUNTIME.spawn(async move {
        let _vm = vm;
        let _taskstate = safe_taskstate;

        // let [r7, r8] = (*vm.0).async_fncall(section, taskstate.0).await;
        // (r7, r8)
        todo!("Add Async fncall");
      });

      // Return HWND
      if return_hwnd {
        let rtptr = Box::into_raw(Box::new(tokiort));

        return rtptr as _;
      }
    }
    // Launch SYNC
    else {
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
  };

  null_mut()
}

pub fn call_spawn(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let SPAWN {
    launch_as_async,
    out_loc,
    return_hwnd,
    section,
  } = parse_spawn(pickle, ws.arr.as_ref());

  unsafe {
    let hwnd = resolve_location_src!(taskstate => out_loc);

    let newhwnd = savm_spawn(taskstate, section, launch_as_async, return_hwnd);

    if !newhwnd.is_null() {
      ptr::write(hwnd as *mut *mut c_void, newhwnd);
    }
  }
}
