use std::fs::File;

use savm::{BytecodeResolver, VM, sart::ctr::DispatchFn};

struct Resolver;

impl BytecodeResolver for Resolver {
  type Output = File;

  fn get_regions(&self, _module: u32) -> Option<&[u32]> {
    Some(&[0])
  }

  fn get_native_regions(&self, _module: u32) -> &[u32] {
    &[]
  }
  fn modules(&self) -> &[u32] {
    &[0]
  }
  fn resolve_bytecode_exact(&self, module: u32, region: u32) -> Option<Self::Output> {
    Some(File::open(format!("./out/{module}/{region}")).expect("Unknown err"))
  }

  fn resolve_native(&self, _module: u32, _func: u32) -> DispatchFn {
    todo!()
  }
}

fn main() {
  let mut vm = VM::new(Resolver);

  unsafe { vm.run() };
}
