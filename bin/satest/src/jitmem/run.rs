use std::mem::zeroed;

use savm::{
  BytecodeResolver, Executable, VM,
  sart::ctr::{FLAGS::FLAG_FIRST, OPCODES::OPCODE_JIT_CHECK},
  sync::VMSTAT,
};

pub fn run_jit<T: BytecodeResolver + Send + Sync + 'static>(vm: &VM<T>, exec: *const Executable) {
  loop {
    let opcode = vm.exec_jit(exec);

    if opcode == OPCODE_JIT_CHECK {
      continue;
    }

    break;
  }
}

pub fn clean() {
  VMSTAT.with(|x| {
    let mt = unsafe { &mut *x.get() };

    for (idx, ts) in mt.ts.iter_mut().enumerate() {
      // Only preserve scratchpad
      let scratchpad = ts.scratchpad;

      *ts = unsafe { zeroed() };
      if idx == 0 {
        ts.flags |= FLAG_FIRST;
      }

      ts.scratchpad = scratchpad;
    }
  });
}
