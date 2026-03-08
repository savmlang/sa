use std::mem::transmute;

use inkwell::{
  OptimizationLevel,
  context::Context,
  targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
};
use object::{File, Object, ObjectSection, ObjectSymbol, RelocationTarget};
use sajit::{
  MemoryExecutable,
  relocations::{RelocKind, Relocation},
};

fn main() {
  Target::initialize_native(&InitializationConfig::default())
    .expect("Failed to find native target");

  let triple = TargetMachine::get_default_triple();
  let target = Target::from_triple(&triple).expect("Cannot build Target");

  let machine = {
    let cpu = TargetMachine::get_host_cpu_name();
    let cpu = cpu.to_str().expect("IMPOSSIBLE");
    let features = TargetMachine::get_host_cpu_features();
    let features = features.to_str().expect("IMPOSSIBLE");

    target
      .create_target_machine(
        &triple,
        cpu,
        features,
        OptimizationLevel::Aggressive,
        RelocMode::PIC,
        CodeModel::Large,
      )
      .expect("Cannot create machine")
  };

  let context = Context::create();
  let module = context.create_module("my_simd_fn");
  let builder = context.create_builder();

  let function = {
    let arg0 = context.i64_type();

    context.i64_type().fn_type(&[arg0.into()], false)
  };

  let function = module.add_function("identity", function, None);

  // Build fn
  {
    let block = context.append_basic_block(function, "idompotency");

    builder.position_at_end(block);

    let externfn = {
      let ctx64 = context.i64_type();

      context.void_type().fn_type(&[ctx64.into()], false)
    };

    let externfn = module.add_function("call_ffi", externfn, None);
    externfn.set_call_conventions(0);

    let arg0 = function.get_nth_param(0).unwrap();

    builder.build_direct_call(externfn, &[arg0.into()], "call_ffi");

    builder
      .build_return(Some(&arg0))
      .expect("Unable to build return");
  }

  module.print_to_stderr();

  let object = machine
    .write_to_memory_buffer(&module, FileType::Object)
    .expect("Get Memory Buffer");

  let (machinecode, relocs) = {
    let slice = object.as_slice();

    let file = File::parse(slice).expect("Parsed");

    let section = file.section_by_name(".text").unwrap();

    let output = section.data().unwrap();

    let mut relocs = vec![];

    for (offset, reloc) in section.relocations() {
      println!("OFFSET : {offset}");

      relocs.push(Relocation {
        addend: 0,
        symbol_addr: identity as *const () as _,
        offset: offset as _,
        kind: RelocKind::Abs8,
      });
    }

    println!("{output:?}");

    (output, relocs)
  };

  unsafe {
    let jit = MemoryExecutable::new_anon(machinecode, &relocs).expect("Our JIT Function");

    let function: extern "C" fn(data: u64) -> u64 = transmute(jit.entry_ptr());

    println!("{}", function(80));
  }
}

extern "C" fn identity(data: i64) {
  println!("C Call {}", data);
}
