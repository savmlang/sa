use savm::{BytecodeResolver, Executable, VM, sart::ctr::OPCODES::OPCODE_JIT_CHECK};

pub fn run_jit<T: BytecodeResolver + Send + Sync + 'static>(vm: &VM<T>, exec: *const Executable) {
  loop {
    let opcode = vm.exec_jit(exec);

    if opcode == OPCODE_JIT_CHECK {
      continue;
    }

    break;
  }
}
