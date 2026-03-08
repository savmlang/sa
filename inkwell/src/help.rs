use std::mem::transmute;

use inkwell::{
  OptimizationLevel,
  context::Context,
  targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
};
use object::{Object, ObjectSection, ObjectSymbol, RelocationTarget};
use sajit::relocations::{RelocKind, Relocation};

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
        RelocMode::Static,
        CodeModel::Large,
      )
      .expect("Cannot create machine")
  };

  let context = Context::create();
  let module = context.create_module("my_simd_fn");
  let builder = context.create_builder();

  // Use a large vector to force the highest SIMD level detected
  // 512-bit vector = 16 x i32
  let i32_type = context.i32_type();
  let vec_type = i32_type.vec_type(16);

  // Create the "Hole" (external call)
  let ext_fn_type = context.void_type().fn_type(&[vec_type.into()], false);
  let ext_fn = module.add_function("simd_add", ext_fn_type, None);
  ext_fn.set_call_conventions(0);

  // Function Data
  let fn_type = vec_type.fn_type(&[vec_type.into()], false);
  let function = module.add_function("kernel", fn_type, None);
  let block = context.append_basic_block(function, "entry");
  builder.position_at_end(block);

  let arg0 = function.get_nth_param(0).unwrap().into_vector_value();
  let sum = builder.build_int_add(arg0, arg0, "sum").unwrap();

  // Call external - this is our relocation "hole"
  builder
    .build_direct_call(ext_fn, &[sum.into()], "call_ext")
    .unwrap();
  builder.build_return(Some(&sum)).unwrap();

  module.print_to_stderr();

  // Get object file
  let buffer = machine
    .write_to_memory_buffer(&module, FileType::Object)
    .unwrap();

  let raw_bytes = buffer.as_slice();

  // 5. Extract "Holes" via Object Crate
  let obj = object::File::parse(raw_bytes).unwrap();
  let text = obj.section_by_name(".text").expect("No .text");

  println!("\n[Machine Code]");
  println!("{:02x?}", text.data().unwrap());

  println!("\n[Relocation Holes]");

  let mut relocs = vec![];

  for (offset, reloc) in text.relocations() {
    // Get the name of the 'hole' target
    let symbol_name = match reloc.target() {
      RelocationTarget::Symbol(index) => {
        let symbol = obj.symbol_by_index(index).expect("Symbol not found");
        symbol.name().unwrap_or("unknown_symbol")
      }
      _ => "absolute_offset",
    };

    relocs.push(Relocation {
      addend: 0,
      symbol_addr: simd_add as *const () as _,
      kind: RelocKind::Abs8,
      offset: offset as _,
    });

    println!(
      "Offset: 0x{:02x}, Type: {:?}, Target: {:?}, Symbol: {symbol_name}",
      offset,
      reloc.kind(),
      reloc.target()
    );
  }

  let jit = unsafe { sajit::MemoryExecutable::new_anon(text.data().unwrap(), &relocs) }
    .expect("Unknown error");

  let fnc: extern "C" fn(myarr: [i32; 16]) = unsafe { transmute(jit.entry_ptr()) };

  fnc([80; 16]);
}

extern "C" fn simd_add(myarr: [i32; 16]) {
  println!("{myarr:?}");
}
