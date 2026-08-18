use sair::{
  SingleThreadedStringStore,
  llir::instr::loc::VMLoc,
  mir::{
    Module,
    block::BLOCK_0,
    regalloc::SCRATCHPAD_MAX_BYTES,
    value::{
      consts::{I8, I16, I32, I64},
      sig::Signature,
    },
  },
  saemit::machine::v0::V0VM,
};

fn main() {
  println!("================================================================================");
  println!("  RUNNING FUNCTION-WIDE BACKTRACKING REGISTER ALLOCATOR VERIFICATION SUITE");
  println!("================================================================================\n");

  test_01_abi_small_input_passing();
  test_02_abi_split_input_passing();
  test_03_abi_large_input_scratchpad();
  test_04_intra_register_subword_packing_spec_example();
  test_05_intra_register_packing_four_i16();
  test_06_intra_register_packing_eight_i8();
  test_07_intra_register_packing_two_i32();
  test_08_liveness_dead_value_elimination_intra_block();
  test_09_liveness_register_reuse_across_blocks();
  test_10_backtracking_under_register_contention();
  test_11_partial_spilling_to_scratchpad();
  test_12_tiered_memory_fallback_scratchpad_to_largepad();

  println!("================================================================================");
  println!("  ALL 12 REGISTER ALLOCATOR SPECIFICATION TESTS PASSED SUCCESSFULLY! (12/12)");
  println!("================================================================================\n");
}

/// Test 1: Basic ABI parameter passing with input <= 8B (Passed in R7)
fn test_01_abi_small_input_passing() {
  println!("[Test 1/12] Verifying ABI small input passing (<= 8B in R7)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "abi_small", &v0);

  let sig = Signature::new(&mut module, &[I64], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_abi_small", sig_ref);
  let param;
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    param = builder.entry_params()[0];
    let c1 = builder.iconst(I64, 42).unwrap().out;
    let sum = builder.vadd(param, c1).unwrap().out;
    builder.r#return(sum).unwrap();
  }

  let result = func.regalloc(&module);
  let param_loc = result.get(param).expect("param allocated");

  assert_eq!(param_loc.reg, VMLoc::R7, "Input <= 8B must be in R7");
  assert_eq!(param_loc.offset, 0, "Input in R7 offset must be 0");
  assert_eq!(param_loc.width, 8, "I64 width is 8B");
  println!("  -> Passed: Param v0 allocated in R7 (offset: 0, width: 8B)\n");
}

/// Test 2: Basic ABI parameter passing with 8B < size <= 16B (Passed in R7 and R8)
fn test_02_abi_split_input_passing() {
  println!("[Test 2/12] Verifying ABI split input passing (8B < size <= 16B in R7 and R8)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "abi_split", &v0);

  let sig = Signature::new(&mut module, &[I64, I64], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_abi_split", sig_ref);
  let (p0, p1);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    let params = builder.entry_params();
    p0 = params[0];
    p1 = params[1];
    let sum = builder.vadd(p0, p1).unwrap().out;
    builder.r#return(sum).unwrap();
  }

  let result = func.regalloc(&module);
  let p0_loc = result.get(p0).expect("p0 allocated");
  let p1_loc = result.get(p1).expect("p1 allocated");

  assert_eq!(p0_loc.reg, VMLoc::R7, "First 8B input must be in R7");
  assert_eq!(p1_loc.reg, VMLoc::R8, "Second 8B input must be in R8");
  println!("  -> Passed: Param v0 in R7, Param v1 in R8\n");
}

/// Test 3: Basic ABI parameter passing with input > 16B (Placed in Scratchpad)
fn test_03_abi_large_input_scratchpad() {
  println!("[Test 3/12] Verifying ABI large input passing (> 16B in Scratchpad)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "abi_large", &v0);

  // 3 * 8B = 24B > 16B
  let sig = Signature::new(&mut module, &[I64, I64, I64], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_abi_large", sig_ref);
  let (p0, p1, p2);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    let params = builder.entry_params();
    p0 = params[0];
    p1 = params[1];
    p2 = params[2];

    let t1 = builder.vadd(p0, p1).unwrap().out;
    let sum = builder.vadd(t1, p2).unwrap().out;
    builder.r#return(sum).unwrap();
  }

  let result = func.regalloc(&module);
  let p0_loc = result.get(p0).expect("p0 allocated");
  let p1_loc = result.get(p1).expect("p1 allocated");
  let p2_loc = result.get(p2).expect("p2 allocated");

  assert_eq!(p0_loc.reg, VMLoc::Scratchpad, "Inputs > 16B placed in Scratchpad");
  assert_eq!(p1_loc.reg, VMLoc::Scratchpad, "Inputs > 16B placed in Scratchpad");
  assert_eq!(p2_loc.reg, VMLoc::Scratchpad, "Inputs > 16B placed in Scratchpad");
  assert_eq!(p0_loc.offset, 0, "p0 offset 0 in Scratchpad");
  assert_eq!(p1_loc.offset, 1, "p1 offset 1 (8B) in Scratchpad");
  assert_eq!(p2_loc.offset, 2, "p2 offset 2 (16B) in Scratchpad");
  println!("  -> Passed: Large arguments placed in Scratchpad at offsets 0, 1, 2\n");
}

/// Test 4: Intra-Register Sub-Word Packing - Exact Specification Example: [I32][I16][I8][I8] in R1
fn test_04_intra_register_subword_packing_spec_example() {
  println!("[Test 4/12] Verifying Intra-Register Fitting exact spec example: [I32][I16][I8][I8] packed in single register...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "pack_spec", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_pack_spec", sig_ref);
  let (v_i32, v_i16, v_i8_a, v_i8_b);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // Create concurrently live values: I32, I16, I8, I8
    v_i32 = builder.iconst(I32, 100).unwrap().out;
    v_i16 = builder.iconst(I16, 200).unwrap().out;
    v_i8_a = builder.iconst(I8, 10).unwrap().out;
    v_i8_b = builder.iconst(I8, 20).unwrap().out;

    // Use them simultaneously at the end so all 4 are live together
    let r1 = builder.vadd(v_i32, v_i32).unwrap().out;
    let r2 = builder.vadd(v_i16, v_i16).unwrap().out;
    let r3 = builder.vadd(v_i8_a, v_i8_b).unwrap().out;
    let _ = (r1, r2, r3);
  }

  let result = func.regalloc(&module);
  let loc_i32 = result.get(v_i32).expect("v_i32 allocated");
  let loc_i16 = result.get(v_i16).expect("v_i16 allocated");
  let loc_i8_a = result.get(v_i8_a).expect("v_i8_a allocated");
  let loc_i8_b = result.get(v_i8_b).expect("v_i8_b allocated");

  println!("    Allocated locations:");
  println!("      I32  -> {:?} offset {}", loc_i32.reg, loc_i32.offset);
  println!("      I16  -> {:?} offset {}", loc_i16.reg, loc_i16.offset);
  println!("      I8_a -> {:?} offset {}", loc_i8_a.reg, loc_i8_a.offset);
  println!("      I8_b -> {:?} offset {}", loc_i8_b.reg, loc_i8_b.offset);

  // All 4 should be packed into the same physical register
  assert_eq!(loc_i32.reg, loc_i16.reg, "I32 and I16 packed in same register");
  assert_eq!(loc_i16.reg, loc_i8_a.reg, "I16 and I8_a packed in same register");
  assert_eq!(loc_i8_a.reg, loc_i8_b.reg, "I8_a and I8_b packed in same register");

  // Verify exact offsets according to specification:
  // I32 at Offset 0 (0 * 32-bit units, byte 0)
  assert_eq!(loc_i32.offset, 0, "I32 offset must be 0");
  assert_eq!(loc_i32.width, 4, "I32 width must be 4");

  // I16 at Offset 2 (2 * 16-bit units, byte 4)
  assert_eq!(loc_i16.offset, 2, "I16 offset must be 2");
  assert_eq!(loc_i16.width, 2, "I16 width must be 2");

  // I8_a at Offset 6 (6 * 8-bit units, byte 6)
  assert_eq!(loc_i8_a.offset, 6, "I8_a offset must be 6");
  assert_eq!(loc_i8_a.width, 1, "I8_a width must be 1");

  // I8_b at Offset 7 (7 * 8-bit units, byte 7)
  assert_eq!(loc_i8_b.offset, 7, "I8_b offset must be 7");
  assert_eq!(loc_i8_b.width, 1, "I8_b width must be 1");

  println!("  -> Passed: Exactly matched spec packing [I32@0, I16@2, I8@6, I8@7] in {:?}\n", loc_i32.reg);
}

/// Test 5: Intra-Register Packing - Four I16 sub-words ([I16; 4])
fn test_05_intra_register_packing_four_i16() {
  println!("[Test 5/12] Verifying Intra-Register Packing of four I16 values ([I16; 4])...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "pack_four_i16", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_four_i16", sig_ref);
  let (a, b, c, d);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    a = builder.iconst(I16, 1).unwrap().out;
    b = builder.iconst(I16, 2).unwrap().out;
    c = builder.iconst(I16, 3).unwrap().out;
    d = builder.iconst(I16, 4).unwrap().out;

    let t1 = builder.vadd(a, b).unwrap().out;
    let t2 = builder.vadd(c, d).unwrap().out;
    let _ = builder.vadd(t1, t2).unwrap().out;
  }

  let result = func.regalloc(&module);
  let loc_a = result.get(a).unwrap();
  let loc_b = result.get(b).unwrap();
  let loc_c = result.get(c).unwrap();
  let loc_d = result.get(d).unwrap();

  assert_eq!(loc_a.reg, loc_b.reg);
  assert_eq!(loc_b.reg, loc_c.reg);
  assert_eq!(loc_c.reg, loc_d.reg);

  assert_eq!(loc_a.offset, 0);
  assert_eq!(loc_b.offset, 1);
  assert_eq!(loc_c.offset, 2);
  assert_eq!(loc_d.offset, 3);
  println!("  -> Passed: Four I16 values packed in {:?} at offsets 0, 1, 2, 3\n", loc_a.reg);
}

/// Test 6: Intra-Register Packing - Eight I8 sub-words ([I8; 8])
fn test_06_intra_register_packing_eight_i8() {
  println!("[Test 6/12] Verifying Intra-Register Packing of eight I8 values ([I8; 8])...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "pack_eight_i8", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_eight_i8", sig_ref);
  let mut vals = Vec::new();
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    for i in 0..8 {
      vals.push(builder.iconst(I8, i as u64).unwrap().out);
    }

    // Keep all 8 live
    let mut acc = vals[0];
    for &v in &vals[1..] {
      acc = builder.vadd(acc, v).unwrap().out;
    }
  }

  let result = func.regalloc(&module);
  let base_reg = result.get(vals[0]).unwrap().reg;

  for (i, &v) in vals.iter().enumerate() {
    let loc = result.get(v).unwrap();
    assert_eq!(loc.reg, base_reg, "All 8 I8 values must pack in same register");
    assert_eq!(loc.offset, i as i8, "I8 offset must be {i}");
    assert_eq!(loc.width, 1, "I8 width must be 1");
  }
  println!("  -> Passed: Eight I8 values packed in {:?} with exact offsets 0..7\n", base_reg);
}

/// Test 7: Intra-Register Packing - Two I32 sub-words ([I32; 2])
fn test_07_intra_register_packing_two_i32() {
  println!("[Test 7/12] Verifying Intra-Register Packing of two I32 values ([I32; 2])...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "pack_two_i32", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_two_i32", sig_ref);
  let (x, y);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    x = builder.iconst(I32, 1000).unwrap().out;
    y = builder.iconst(I32, 2000).unwrap().out;

    let _ = builder.vadd(x, y).unwrap().out;
  }

  let result = func.regalloc(&module);
  let loc_x = result.get(x).unwrap();
  let loc_y = result.get(y).unwrap();

  assert_eq!(loc_x.reg, loc_y.reg);
  assert_eq!(loc_x.offset, 0);
  assert_eq!(loc_y.offset, 1);
  println!("  -> Passed: Two I32 values packed in {:?} at offsets 0 and 1\n", loc_x.reg);
}

/// Test 8: Liveness Tracking & Value Elimination - Dead value space reused in same block
fn test_08_liveness_dead_value_elimination_intra_block() {
  println!("[Test 8/12] Verifying Liveness Tracking & immediate register slot reuse upon value death...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "liveness_elim", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_liveness_elim", sig_ref);
  let (v1, v2, v3);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // v1 is defined and immediately consumed
    v1 = builder.iconst(I64, 10).unwrap().out;
    let _ = builder.vadd(v1, v1).unwrap().out; // v1 is dead after this point

    // v2 is defined AFTER v1 dies
    v2 = builder.iconst(I64, 20).unwrap().out;
    let _ = builder.vadd(v2, v2).unwrap().out; // v2 is dead after this point

    // v3 is defined AFTER v2 dies
    v3 = builder.iconst(I64, 30).unwrap().out;
    let _ = builder.vadd(v3, v3).unwrap().out;
  }

  let result = func.regalloc(&module);
  let loc1 = result.get(v1).unwrap();
  let loc2 = result.get(v2).unwrap();
  let loc3 = result.get(v3).unwrap();

  // All 3 non-overlapping sequential lifetimes can reuse the exact same register!
  assert_eq!(loc1.reg, loc2.reg, "v2 should reuse v1's register slot");
  assert_eq!(loc2.reg, loc3.reg, "v3 should reuse v2's register slot");
  println!("  -> Passed: Dead values v1, v2, v3 all reused register {:?} consecutively\n", loc1.reg);
}

/// Test 9: Liveness-Based Register Reuse Across Basic Block Boundaries
fn test_09_liveness_register_reuse_across_blocks() {
  println!("[Test 9/12] Verifying Liveness-based register reuse across basic block boundaries...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "liveness_cfg", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_liveness_cfg", sig_ref);
  let (local_val, passed_val, param_b1, new_val_b1);
  {
    let mut builder = func.builder(&module);
    let block1 = builder.block(&[I64]);

    builder.position_end(BLOCK_0);

    // local_val and passed_val are live concurrently in BLOCK_0
    local_val = builder.iconst(I64, 999).unwrap().out;
    passed_val = builder.iconst(I64, 111).unwrap().out;

    // local_val is used and dies in BLOCK_0 (NOT passed to block1)
    let _ = builder.vadd(local_val, local_val).unwrap().out;

    // passed_val is passed to block1
    builder.jump(block1, &[passed_val]).unwrap();

    // In block1, define a new value
    builder.position_end(block1);
    new_val_b1 = builder.iconst(I64, 222).unwrap().out;
    param_b1 = builder.block_params(block1).unwrap()[0];
    let _ = builder.vadd(param_b1, new_val_b1).unwrap().out;
  }

  let result = func.regalloc(&module);
  let loc_local = result.get(local_val).unwrap();
  let loc_passed = result.get(passed_val).unwrap();
  let loc_param = result.get(param_b1).unwrap();
  let loc_new = result.get(new_val_b1).unwrap();

  println!("    Allocations across blocks:");
  println!("      local_val (Block 0)  -> {:?}", loc_local.reg);
  println!("      passed_val (Block 0) -> {:?}", loc_passed.reg);
  println!("      param_b1 (Block 1)   -> {:?}", loc_param.reg);
  println!("      new_val_b1 (Block 1) -> {:?}", loc_new.reg);

  // local_val was not live-out to block1, so its register is reused in block1!
  assert_ne!(loc_local.reg, loc_passed.reg, "local_val and passed_val active concurrently in Block 0");
  assert_eq!(loc_param.reg, loc_local.reg, "Block 1 parameter reuses dead register from Block 0");
  assert_eq!(loc_new.reg, loc_passed.reg, "Block 1 value reuses dead register from Block 0");
  println!("  -> Passed: Cross-block dead value registers {:?} and {:?} successfully reused in Block 1\n", loc_local.reg, loc_passed.reg);
}

/// Test 10: Backtracking Allocator under Register Contention
fn test_10_backtracking_under_register_contention() {
  println!("[Test 10/12] Verifying Backtracking Allocator resolution under contention...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "backtracking_contention", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_backtrack", sig_ref);
  let mut active_vals = Vec::new();
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // Allocate 7 active 64-bit values to occupy registers
    for i in 0..7 {
      active_vals.push(builder.iconst(I64, (i + 1) as u64 * 10).unwrap().out);
    }

    // Operations peak at 8 concurrent values (7 inputs + 1 output) filling all R1..R8
    let s0 = builder.vadd(active_vals[0], active_vals[1]).unwrap().out;
    let s1 = builder.vadd(active_vals[2], active_vals[3]).unwrap().out;
    let s2 = builder.vadd(active_vals[4], active_vals[5]).unwrap().out;
    let t0 = builder.vadd(s0, s1).unwrap().out;
    let t1 = builder.vadd(s2, active_vals[6]).unwrap().out;
    let _ = builder.vadd(t0, t1).unwrap().out;
  }

  let result = func.regalloc(&module);
  for &v in &active_vals {
    let loc = result.get(v).unwrap();
    println!("    value -> {:?}", loc.reg);
    assert_ne!(loc.reg, VMLoc::Scratchpad, "Must be placed in a physical register");
    assert_ne!(loc.reg, VMLoc::Largepad, "Must be placed in a physical register");
  }

  assert_eq!(result.scratchpad_bytes, 0, "All values fit in physical registers R1..R8 without spill");
  println!("  -> Passed: Backtracking allocator placed concurrent 64-bit values into R1..R8 without spills\n");
}

/// Test 11: Partial Spilling to Scratchpad under High Register Pressure
fn test_11_partial_spilling_to_scratchpad() {
  println!("[Test 11/12] Verifying Partial Spilling to Scratchpad under high register pressure (> 8 concurrent I64s)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "spill_scratchpad", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_spill_scratchpad", sig_ref);
  let mut active_vals = Vec::new();
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // Create 12 concurrently live 64-bit values (exceeding 8 physical registers)
    for i in 0..12 {
      active_vals.push(builder.iconst(I64, (i + 1) as u64 * 100).unwrap().out);
    }

    // Keep all 12 live until the very end
    let mut sum = active_vals[0];
    for &v in &active_vals[1..] {
      sum = builder.vadd(sum, v).unwrap().out;
    }
  }

  let result = func.regalloc(&module);
  println!("    Allocations summary:");
  let mut scratchpad_count = 0;
  let mut phys_reg_count = 0;

  for &v in &active_vals {
    let loc = result.get(v).unwrap();
    if loc.reg == VMLoc::Scratchpad {
      scratchpad_count += 1;
    } else {
      phys_reg_count += 1;
    }
  }

  println!("      Physical Registers Used: {phys_reg_count}");
  println!("      Scratchpad Spilled Count: {scratchpad_count}");
  println!("      Scratchpad Bytes: {}B / {}B", result.scratchpad_bytes, SCRATCHPAD_MAX_BYTES);

  assert!(phys_reg_count >= 7, "Physical registers should be utilized");
  assert!(scratchpad_count >= 4, "Remaining values should be spilled to Scratchpad");
  assert!(result.scratchpad_bytes > 0 && result.scratchpad_bytes <= SCRATCHPAD_MAX_BYTES);
  assert_eq!(result.largepad_bytes, 0, "No Largepad should be needed when Scratchpad has space");
  println!("  -> Passed: Partial spilling placed values in physical registers and spilled excess to Scratchpad\n");
}

/// Test 12: Tiered Memory Fallback - Scratchpad to Largepad Overflow (> 192B)
fn test_12_tiered_memory_fallback_scratchpad_to_largepad() {
  println!("[Test 12/12] Verifying Tiered Memory Fallback (Scratchpad 192B capacity -> Largepad overflow)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "fallback_largepad", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_largepad", sig_ref);
  let mut vals = Vec::new();
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // Create 35 concurrently live 64-bit values:
    // 8 in physical registers R1..R8
    // 24 in Scratchpad (24 * 8 = 192 Bytes: exact scratchpad capacity)
    // 3 overflow to Largepad
    for i in 0..35 {
      vals.push(builder.iconst(I64, (i + 1) as u64 * 10).unwrap().out);
    }

    // Keep all 35 concurrently live until reduction at the end
    let mut sum = vals[0];
    for &v in &vals[1..] {
      sum = builder.vadd(sum, v).unwrap().out;
    }
  }

  let result = func.regalloc(&module);
  let mut scratchpad_count = 0;
  let mut largepad_count = 0;
  let mut phys_count = 0;

  for &v in &vals {
    let loc = result.get(v).unwrap();
    match loc.reg {
      VMLoc::Scratchpad => scratchpad_count += 1,
      VMLoc::Largepad => largepad_count += 1,
      _ => phys_count += 1,
    }
  }

  println!("    Allocations summary:");
  println!("      Physical Registers: {phys_count}");
  println!("      Scratchpad Count: {scratchpad_count} ({}B / {}B)", result.scratchpad_bytes, SCRATCHPAD_MAX_BYTES);
  println!("      Largepad Count: {largepad_count} ({}B)", result.largepad_bytes);

  assert!(phys_count >= 7, "Physical registers should be utilized");
  assert_eq!(result.scratchpad_bytes, SCRATCHPAD_MAX_BYTES, "Scratchpad must reach max capacity of 192B");
  assert!(largepad_count > 0, "Overflow values must transition to Largepad");
  assert!(result.largepad_bytes > 0, "Largepad must allocate memory for overflow");
  println!("  -> Passed: Scratchpad reached full 192B capacity and overflowed to Largepad\n");
}
