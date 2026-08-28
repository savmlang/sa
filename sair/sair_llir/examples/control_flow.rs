use sair_llir::prelude::*;

fn main() {
  println!("=== SaVM LLIR Example: Control Flow, Loops & Branching ===");

  let mut builder = LLBuilder::new_function(0x3000, "countdown_loop");
  let r1 = LocSrc::r1(); // counter
  let r2 = LocSrc::r2(); // decrement step
  let r3 = LocSrc::r3(); // accumulator
  let r4 = LocSrc::r4(); // cmp result

  // Block indices for structured branching:
  // - Block 0 is the entry block (created automatically, no marker emitted for id == 0).
  // - Block 1 is a hot loop header (with JITUp bit set: marker as i64 < 0).
  // - Block 2 is the loop body.
  // - Block 3 is the exit block.
  let loop_header = 1;
  let loop_body = 2;
  let loop_exit = 3;

  // Block 0: Entry
  builder
    .iconst64(10, r1)  // counter = 10
    .iconst64(1, r2)   // step = 1
    .iconst64(0, r3)   // acc = 0
    .jmp_to(loop_header, true); // Jump to hot loop header

  // Block 1: Loop Header (hot / JIT check enabled)
  // `hot_block` sets the 63rd bit (sign bit < 0) for JIT and OSR runtime checks!
  builder.hot_block("loop_header");
  builder
    .jz_to(Width::W64, loop_exit, false, r1) // Jump to exit if counter == 0
    .jmp_to(loop_body, false);

  // Block 2: Loop Body
  builder.block("loop_body");
  builder
    // acc = acc + counter
    .vadd_std(IntTy::U64, 1, r3, r1, r3)
    // counter = counter - 1
    .vsub_std(IntTy::U64, 1, r1, r2, r1)
    // Compare counter > 0
    .vcmp(Width::W64, CmpOp::UGt, 1, r1, r2, r4)
    // JNZ back to hot loop header
    .jnz_to(Width::W64, loop_header, true, r4);

  // Block 3: Loop Exit
  builder.block("loop_exit");
  builder.mov(r3, r1); // Return accumulator in r1

  let func = builder.finish();
  let mut module = LLModule::new("control_flow_demo");
  module.add_function(func);

  println!("--- Generated LLIR Representation ---");
  println!("{}", module);

  let bytecode = module.to_bytes();
  println!("--- Bytecode Emission ---");
  println!("Total bytecode bytes emitted: {}", bytecode.len());
}
