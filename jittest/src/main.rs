use std::{mem::transmute, os::raw::c_uint, time::Instant};

use cranelift::{
  jit::{JITBuilder, JITModule},
  module::{self, Module},
  native,
  prelude::*,
};

use crate::jit::JIT;

mod jit;

fn main() {
  let mut flag_builder = settings::builder();
  flag_builder.set("use_colocated_libcalls", "false").unwrap();
  flag_builder.set("is_pic", "false").unwrap();
  let isa_builder = native::builder().unwrap_or_else(|msg| {
    panic!("host machine is not supported: {}", msg);
  });
  let isa = isa_builder
    .finish(settings::Flags::new(flag_builder))
    .unwrap();
  let builder = JITBuilder::with_isa(isa, module::default_libcall_names());

  let module = JITModule::new(builder);

  let mut jit = JIT {
    ctx: module.make_context(),
    fnctx: FunctionBuilderContext::new(),
    module,
  };

  println!("Starting compilating...");
  let t0 = Instant::now();
  let f: extern "C" fn(*const u64) -> usize = unsafe { transmute(jit.new_dummy().unwrap()) };
  let tf = Instant::now().duration_since(t0);

  println!("Compiled in {:?}", tf);

  let data = Box::new(84512131u64);

  println!("{} {}", f(data.as_ref() as *const _) as usize, 32);
}
