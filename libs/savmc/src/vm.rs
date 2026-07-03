use crate::{SAVMC_IBytecodeResolver, SAVMC_IBytecodeResolver as IBytecodeResolver};
use savm::VM;
use std::os::raw::c_void;

pub type SAVM = c_void;

#[no_mangle]
pub extern "C" fn savm_toolkit_savm_create(resolver: SAVMC_IBytecodeResolver) -> *mut SAVM {
  Box::into_raw(Box::new(VM::new(resolver))) as _
}

#[no_mangle]
pub extern "C" fn savm_toolkit_savm_call_section(vm: *mut SAVM, sectionid: u64) {
  let vm = vm as *mut VM<IBytecodeResolver>;

  unsafe { (*vm).call_section(sectionid) };
}

#[no_mangle]
pub extern "C" fn savm_toolkit_savm_dispatch_chocolate(
  vm: *mut SAVM,
  sectionid: u64,
  enable_jit_jump: bool,
) {
  let vm = vm as *mut VM<IBytecodeResolver>;

  unsafe {
    if enable_jit_jump {
      (*vm).dispatch_chocolate::<true>(sectionid)
    } else {
      (*vm).dispatch_chocolate::<false>(sectionid)
    }
  };
}
