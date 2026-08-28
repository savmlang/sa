use sair_llir::prelude::*;

fn main() {
  println!("=== SaVM LLIR Example: Concurrency, Tasks & Atomic Memory Operations ===");

  let mut builder = LLBuilder::new_function(0x5000, "concurrent_sync_pipeline");
  let r1 = LocSrc::r1();
  let r2 = LocSrc::r2();
  let r3 = LocSrc::r3();
  let r4 = LocSrc::r4();
  let r5 = LocSrc::r5();

  // 1. Spawning a new worker task/thread into section 0x6000 with HWND handle stored in r1
  builder.spawn(0x6000, SpawnFlags::new(true, Some(r1)));

  // 2. Synchronous module section call (ignoring registers r1 and r2 via typed RegBitmask)
  let ignore_mask = RegBitmask::empty().with(VMLoc::R1).with(VMLoc::R2);
  builder.synccall(ignore_mask, 0x7000);

  // 3. Task management operations (join, yield, park/unpark, sleep)
  builder
    .task(TaskSubOp::SyncYield, 0, 0x100)
    .task(TaskSubOp::SyncThreadUnpark, 1, 0x101)
    .task(TaskSubOp::WaitMs, 2, 0x102) // Def points to sleep duration
    .task(TaskSubOp::SyncJoin, 1, 0x103);

  // 4. Atomic Operations:
  // (a) Atomic Compare-And-Swap (CAS)
  builder.atomic(
    AtomicOp::Cas,
    IntTy::U64,
    AtomicOrdering::SeqCst,
    AtomicOrdering::SeqCst,
    AtomicRmwOp::Add,
    r1, // pointer location
    r2, // desired value to store
    r3, // expected value
    r4, // return value / success bool
  );

  // (b) Atomic Fetch-And-Add (RMW)
  builder.atomic(
    AtomicOp::Rmw,
    IntTy::U64,
    AtomicOrdering::AcqRel,
    AtomicOrdering::Acquire,
    AtomicRmwOp::Add,
    r1, // pointer
    r2, // operand
    r3, // ignored
    r4, // old value returned
  );

  // (c) Atomic Load
  builder.atomic(
    AtomicOp::Load,
    IntTy::U64,
    AtomicOrdering::Acquire,
    AtomicOrdering::Relaxed,
    AtomicRmwOp::Add,
    r1, // pointer
    r2,
    r3,
    r5, // loaded value into r5
  );

  // (d) Atomic Store
  builder.atomic(
    AtomicOp::Store,
    IntTy::U64,
    AtomicOrdering::Release,
    AtomicOrdering::Relaxed,
    AtomicRmwOp::Add,
    r1, // pointer
    r5, // value to store
    r3,
    r4,
  );

  let func = builder.finish();
  let mut module = LLModule::new("concurrency_demo");
  module.add_function(func);

  println!("--- Generated LLIR Representation ---");
  println!("{}", module);

  let bytecode = module.to_bytes();
  println!("--- Bytecode Emission ---");
  println!("Total bytecode bytes emitted: {}", bytecode.len());
}
