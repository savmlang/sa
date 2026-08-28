use sair_llir::prelude::*;

fn main() {
  println!("=== SaVM LLIR Example: Memory Ops, Scratchpad & Vector Copies ===");

  let mut builder = LLBuilder::new_function(0x4000, "memory_pipeline");
  let r1 = LocSrc::r1();
  let r2 = LocSrc::r2();
  let r3 = LocSrc::r3();
  let r4 = LocSrc::r4();
  let scratch = LocSrc::scratchpad();
  let largepad = LocSrc::largepad();
  let ptr_r2 = LocSrc::ptr_r2();

  // 1. Scratchpad dynamic allocation (alloc 1024 bytes aligned to 64 bytes)
  builder
    .iconst64(1024, r1) // size reg = r1
    .iconst64(64, r2)   // align reg = r2
    .scratch(ScratchClass::Alloc, 0, 1);

  // 2. Fetch largepad pointer and global RW pointer into r1
  builder
    .largepad_ptr()
    .mov(r1, r3) // Save largepad pointer into r3
    .global_rw_ptr()
    .mov(r1, r4); // Save global RW pointer into r4

  // 3. Vectored copy with custom memory flags
  let memflags = VCopyMemFlags::new(
    true, // volatile
    true, // non-overlapping
    AlignData::B64,
    AlignData::B64,
  );

  // Copy 128 bytes from scratchpad to largepad
  builder.vcopy(Count::abs(128), memflags, scratch, largepad);

  // Copy with offset
  builder.vcopy(
    Count::abs(64),
    VCopyMemFlags::default(),
    scratch.with_offset(32),
    largepad.with_offset(64),
  );

  // Indirect pointer copy through r2
  builder
    .iconst64(0x00FF_0000, r2)
    .vcopy_abs(32, ptr_r2, scratch);

  // 4. Scratchpad deallocation
  builder.scratch(ScratchClass::Dealloc, 0, 0);

  let func = builder.finish();
  let mut module = LLModule::new("memory_demo");
  module.add_function(func);

  println!("--- Generated LLIR Representation ---");
  println!("{}", module);

  let bytecode = module.to_bytes();
  println!("--- Bytecode Emission ---");
  println!("Total bytecode bytes emitted: {}", bytecode.len());
}
