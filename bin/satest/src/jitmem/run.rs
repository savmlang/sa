use savm::{
  acaot::pickle::def::PickleInstruction, sart::ctr::OPCODES::OPCODE_JIT_CHECK, BytecodeResolver,
  Executable, VM,
};

pub fn run_jit<T: BytecodeResolver + Send + Sync + 'static>(
  vm: &VM<T>,
  pickle: &[PickleInstruction],
  exec: *const Executable,
  name: &str,
) {
  loop {
    #[allow(unused_assignments)]
    let mut opcode = 0;
    if name == "Cinder - ACAoT JIT" {
      #[cfg(all(
        feature = "native",
        any(target_arch = "x86_64"),
        any(target_os = "windows", target_os = "linux")
      ))]
      {
        opcode = vm.exec_jit_cinder(pickle, exec);
      }
    } else {
      opcode = vm.exec_jit(exec);
    }

    if opcode == OPCODE_JIT_CHECK {
      continue;
    }

    break;
  }
}
