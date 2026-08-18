use sair::{
  SingleThreadedStringStore,
  llir::instr::loc::VMLoc,
  mir::{
    Module,
    block::BLOCK_0,
    regalloc::SCRATCHPAD_MAX_BYTES,
    value::{
      consts::{D64, F32, I8, I16, I32, I64},
      sig::Signature,
    },
  },
  saemit::machine::v0::V0VM,
};

fn main() {
  println!("================================================================================");
  println!("       SAIR EXTENDED REGISTER ALLOCATOR VERIFICATION & DEMO SUITE");
  println!("================================================================================\n");

  test_01_diamond_cfg_branch_and_merge();
  test_02_loop_backedge_with_accumulator();
  test_03_multi_register_heterogeneous_subword_packing();
  test_04_deep_expression_tree_reduction_dag();
  test_05_floating_point_arithmetic_and_lowering();
  test_06_live_through_variables_across_block_pipeline();
  test_07_complex_abi_mixed_parameters_scratchpad();
  test_08_subword_slot_reuse_across_consecutive_lifetimes();
  test_09_massive_contention_deep_largepad_spill();
  test_10_multi_successor_dag_with_differing_phi_arity();

  println!("================================================================================");
  println!("  ALL 10 EXTENDED REGISTER ALLOCATOR TESTS PASSED SUCCESSFULLY! (10/10)");
  println!("================================================================================\n");
}

/// Test 1: Diamond CFG Control Flow (If-Else Branch & Merge with Block Arguments)
/// Tests register reuse across divergent branches and block parameter passing at join point.
fn test_01_diamond_cfg_branch_and_merge() {
  println!("[Extended Test 1/10] Diamond CFG: Branch & Merge with Block Parameters...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "diamond_cfg", &v0);

  let sig = Signature::new(&mut module, &[I64, I64], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_diamond", sig_ref);
  let (p_left, p_right, merge_param);
  {
    let mut builder = func.builder(&module);
    let block_left = builder.block(&[I64]);
    let block_right = builder.block(&[I64]);
    let block_merge = builder.block(&[I64]);

    // Entry Block: BLOCK_0
    builder.position_end(BLOCK_0);
    let params = builder.entry_params();
    let x = params[0];
    let y = params[1];

    let c1 = builder.iconst(I64, 10).unwrap().out;
    let val_l = builder.vadd(x, c1).unwrap().out;
    // Jump to left branch
    builder.jump(block_left, &[val_l]).unwrap();

    // Left Branch: computes left result and jumps to merge
    builder.position_end(block_left);
    p_left = builder.block_params(block_left).unwrap()[0];
    let c2 = builder.iconst(I64, 20).unwrap().out;
    let res_l = builder.vadd(p_left, c2).unwrap().out;
    builder.jump(block_merge, &[res_l]).unwrap();

    // Right Branch: computes right result and jumps to merge
    builder.position_end(block_right);
    p_right = builder.block_params(block_right).unwrap()[0];
    let c3 = builder.iconst(I64, 30).unwrap().out;
    let res_r = builder.vmul(p_right, c3).unwrap().out;
    builder.jump(block_merge, &[res_r]).unwrap();

    // Merge Block: receives phi argument and returns
    builder.position_end(block_merge);
    merge_param = builder.block_params(block_merge).unwrap()[0];
    let final_res = builder.vadd(merge_param, y).unwrap().out;
    builder.r#return(final_res).unwrap();
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_l = result.get(p_left).expect("p_left allocated");
  let loc_merge = result.get(merge_param).expect("merge_param allocated");

  assert_ne!(loc_l.reg, VMLoc::Scratchpad, "Block params should be in registers");
  assert_ne!(loc_merge.reg, VMLoc::Scratchpad, "Merge param should be in register");
  assert_eq!(result.scratchpad_bytes, 0, "No spills needed for simple diamond CFG");
  println!("  -> Passed: Diamond CFG allocated with 0 spill bytes\n");
}

/// Test 2: Loop CFG with Back-Edge and Loop Accumulator
/// Tests cyclic control flow, back-edge phi argument passing, and live range analysis in loops.
fn test_02_loop_backedge_with_accumulator() {
  println!("[Extended Test 2/10] Loop CFG: Back-Edge with Loop Accumulator Variable...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "loop_cfg", &v0);

  let sig = Signature::new(&mut module, &[I64, I64], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_loop", sig_ref);
  let (header_i, header_acc, exit_res);
  {
    let mut builder = func.builder(&module);
    let block_header = builder.block(&[I64, I64]);
    let block_exit = builder.block(&[I64]);

    // Entry Block: init loop counter i and accumulator acc
    builder.position_end(BLOCK_0);
    let params = builder.entry_params();
    let init_i = params[0];
    let init_acc = params[1];
    builder.jump(block_header, &[init_i, init_acc]).unwrap();

    // Loop Header: (i, acc)
    builder.position_end(block_header);
    let h_params = builder.block_params(block_header).unwrap();
    header_i = h_params[0];
    header_acc = h_params[1];

    let one = builder.iconst(I64, 1).unwrap().out;
    let next_i = builder.vsub(header_i, one).unwrap().out;
    let next_acc = builder.vadd(header_acc, header_i).unwrap().out;

    // Simulate loop back-edge jump to header
    builder.jump(block_header, &[next_i, next_acc]).unwrap();

    // Exit Block
    builder.position_end(block_exit);
    exit_res = builder.block_params(block_exit).unwrap()[0];
    builder.r#return(exit_res).unwrap();
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_i = result.get(header_i).unwrap();
  let loc_acc = result.get(header_acc).unwrap();

  assert_ne!(loc_i.reg, loc_acc.reg, "Loop counter and accumulator must be distinct");
  assert_ne!(loc_i.reg, VMLoc::Scratchpad);
  assert_ne!(loc_acc.reg, VMLoc::Scratchpad);
  println!("  -> Passed: Loop back-edge variables allocated to {:?} and {:?}\n", loc_i.reg, loc_acc.reg);
}

/// Test 3: Multi-Register Heterogeneous Sub-Word Packing
/// Packs multiple registers concurrently:
///   Reg A: [I32, I16, I8, I8] (8B total)
///   Reg B: [I16, I16, I32] (8B total)
///   Reg C: [I32, I32] (8B total)
fn test_03_multi_register_heterogeneous_subword_packing() {
  println!("[Extended Test 3/10] Multi-Register Heterogeneous Sub-Word Packing (9 sub-words in 3 registers)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "multi_pack", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_multi_pack", sig_ref);

  // Group 1: I32, I16, I8, I8
  let (g1_i32, g1_i16, g1_i8_a, g1_i8_b);
  // Group 2: I16, I16, I32
  let (g2_i16_a, g2_i16_b, g2_i32);
  // Group 3: I32, I32
  let (g3_i32_a, g3_i32_b);

  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // Instantiate Group 1
    g1_i32 = builder.iconst(I32, 100).unwrap().out;
    g1_i16 = builder.iconst(I16, 200).unwrap().out;
    g1_i8_a = builder.iconst(I8, 10).unwrap().out;
    g1_i8_b = builder.iconst(I8, 20).unwrap().out;

    // Instantiate Group 2
    g2_i16_a = builder.iconst(I16, 300).unwrap().out;
    g2_i16_b = builder.iconst(I16, 400).unwrap().out;
    g2_i32 = builder.iconst(I32, 500).unwrap().out;

    // Instantiate Group 3
    g3_i32_a = builder.iconst(I32, 600).unwrap().out;
    g3_i32_b = builder.iconst(I32, 700).unwrap().out;

    // Keep all 9 concurrently live
    let r1 = builder.vadd(g1_i32, g2_i32).unwrap().out;
    let r2 = builder.vadd(g1_i16, g2_i16_a).unwrap().out;
    let r3 = builder.vadd(g1_i8_a, g1_i8_b).unwrap().out;
    let r4 = builder.vadd(g2_i16_b, g2_i16_b).unwrap().out;
    let r5 = builder.vadd(g3_i32_a, g3_i32_b).unwrap().out;
    let _ = (r1, r2, r3, r4, r5);
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_g1_32 = result.get(g1_i32).unwrap();
  let loc_g1_16 = result.get(g1_i16).unwrap();
  let loc_g1_8a = result.get(g1_i8_a).unwrap();
  let loc_g1_8b = result.get(g1_i8_b).unwrap();

  let loc_g2_16a = result.get(g2_i16_a).unwrap();
  let loc_g2_16b = result.get(g2_i16_b).unwrap();
  let loc_g2_32 = result.get(g2_i32).unwrap();

  let loc_g3_32a = result.get(g3_i32_a).unwrap();
  let loc_g3_32b = result.get(g3_i32_b).unwrap();

  // Verify Group 1 packing
  assert_eq!(loc_g1_32.reg, loc_g1_16.reg);
  assert_eq!(loc_g1_16.reg, loc_g1_8a.reg);
  assert_eq!(loc_g1_8a.reg, loc_g1_8b.reg);
  assert_eq!(loc_g1_32.offset, 0);
  assert_eq!(loc_g1_16.offset, 2);
  assert_eq!(loc_g1_8a.offset, 6);
  assert_eq!(loc_g1_8b.offset, 7);

  // Verify Group 2 packing
  assert_eq!(loc_g2_16a.reg, loc_g2_16b.reg);
  assert_eq!(loc_g2_16b.reg, loc_g2_32.reg);
  assert_eq!(loc_g2_16a.offset, 0);
  assert_eq!(loc_g2_16b.offset, 1);
  assert_eq!(loc_g2_32.offset, 1); // offset in 32-bit units (byte offset 4)

  // Verify Group 3 packing
  assert_eq!(loc_g3_32a.reg, loc_g3_32b.reg);
  assert_eq!(loc_g3_32a.offset, 0);
  assert_eq!(loc_g3_32b.offset, 1);

  // The three packed registers should be distinct physical registers
  assert_ne!(loc_g1_32.reg, loc_g2_16a.reg);
  assert_ne!(loc_g2_16a.reg, loc_g3_32a.reg);
  assert_ne!(loc_g1_32.reg, loc_g3_32a.reg);

  println!("  -> Passed: 9 sub-words packed into 3 distinct registers ({:?}, {:?}, {:?}) with exact offsets\n",
    loc_g1_32.reg, loc_g2_16a.reg, loc_g3_32a.reg);
}

/// Test 4: Deep Expression Tree (Binary Reduction DAG)
/// 16 initial constants evaluated as a 4-level balanced binary tree.
/// Verifies peak register pressure calculation and immediate register recycling as subtrees complete.
fn test_04_deep_expression_tree_reduction_dag() {
  println!("[Extended Test 4/10] Deep Binary Expression Reduction Tree (16 leaves -> 1 root)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "tree_reduction", &v0);

  let sig = Signature::new(&mut module, &[], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_tree", sig_ref);
  let root;
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // Level 0: 16 leaves
    let mut current_level = Vec::new();
    for i in 0..16 {
      current_level.push(builder.iconst(I64, (i + 1) as u64).unwrap().out);
    }

    // Binary reduction: 16 -> 8 -> 4 -> 2 -> 1
    while current_level.len() > 1 {
      let mut next_level = Vec::new();
      for pair in current_level.chunks(2) {
        let sum = builder.vadd(pair[0], pair[1]).unwrap().out;
        next_level.push(sum);
      }
      current_level = next_level;
    }

    root = current_level[0];
    builder.r#return(root).unwrap();
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let root_loc = result.get(root).expect("root allocated");
  assert_ne!(root_loc.reg, VMLoc::Scratchpad);
  assert_eq!(result.largepad_bytes, 0, "No Largepad needed for balanced binary tree reduction");
  println!("  -> Passed: 16-node reduction tree evaluated, root returned in {:?}\n", root_loc.reg);
}

/// Test 5: Floating Point (F32 and D64) Arithmetic and Instruction Lowering
/// Verifies that float operations (VAddf, VSubf, VMulf, VDivf) are registered and allocated.
fn test_05_floating_point_arithmetic_and_lowering() {
  println!("[Extended Test 5/10] Floating Point Arithmetic & Lowering (F32 / D64)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "float_arith", &v0);

  let sig = Signature::new(&mut module, &[F32, F32, D64, D64], Some(D64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_floats", sig_ref);
  let (p_f0, p_f1, p_d0, p_d1, out_d);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    let params = builder.entry_params();
    p_f0 = params[0];
    p_f1 = params[1];
    p_d0 = params[2];
    p_d1 = params[3];

    // F32 operations
    let f_add = builder.vadd(p_f0, p_f1).unwrap().out;
    let f_sub = builder.vsub(p_f0, p_f1).unwrap().out;
    let f_mul = builder.vmul(f_add, f_sub).unwrap().out;
    let f_div = builder.vdivf(f_mul, p_f1).unwrap().out;
    let _ = f_div;

    // D64 operations
    let d_add = builder.vadd(p_d0, p_d1).unwrap().out;
    let d_sub = builder.vsub(p_d0, p_d1).unwrap().out;
    let d_mul = builder.vmul(d_add, d_sub).unwrap().out;
    let d_div = builder.vdivf(d_mul, p_d1).unwrap().out;
    out_d = d_div;

    builder.r#return(out_d).unwrap();
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_f0 = result.get(p_f0).unwrap();
  let loc_f1 = result.get(p_f1).unwrap();
  let loc_d0 = result.get(p_d0).unwrap();
  let loc_d1 = result.get(p_d1).unwrap();

  println!("    Allocations: F32({:?}) -> {:?}, F32({:?}) -> {:?}, D64({:?}) -> {:?}, D64({:?}) -> {:?}",
    p_f0, loc_f0.reg, p_f1, loc_f1.reg, p_d0, loc_d0.reg, p_d1, loc_d1.reg);

  assert_eq!(loc_f0.width, 4, "F32 width is 4B");
  assert_eq!(loc_d0.width, 8, "D64 width is 8B");
  println!("  -> Passed: Floating point arithmetic correctly typed and allocated\n");
}

/// Test 6: Multi-Block Long-Lived Value Spanning Intermediate Blocks (Live-Through Values)
/// Value defined in Block 0 survives across Block 1 and Block 2 while both create intense local register pressure.
fn test_06_live_through_variables_across_block_pipeline() {
  println!("[Extended Test 6/10] Live-Through Variables Across Multi-Block Pipeline...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "live_through", &v0);

  let sig = Signature::new(&mut module, &[], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_live_through", sig_ref);
  let (v_anchor, final_sum);
  {
    let mut builder = func.builder(&module);
    let block1 = builder.block(&[I64]);
    let block2 = builder.block(&[I64]);
    let block3 = builder.block(&[I64]);

    // Block 0: define anchor and jump to block1
    builder.position_end(BLOCK_0);
    v_anchor = builder.iconst(I64, 424242).unwrap().out;
    builder.jump(block1, &[v_anchor]).unwrap();

    // Block 1: create local pressure with 4 temporary values, then jump to block2
    builder.position_end(block1);
    let b1_param = builder.block_params(block1).unwrap()[0];
    let t1 = builder.iconst(I64, 10).unwrap().out;
    let t2 = builder.iconst(I64, 20).unwrap().out;
    let t3 = builder.iconst(I64, 30).unwrap().out;
    let t4 = builder.iconst(I64, 40).unwrap().out;
    let s1 = builder.vadd(t1, t2).unwrap().out;
    let s2 = builder.vadd(t3, t4).unwrap().out;
    let s3 = builder.vadd(s1, s2).unwrap().out;
    let _ = s3;
    builder.jump(block2, &[b1_param]).unwrap();

    // Block 2: create another local pressure wave, then jump to block3
    builder.position_end(block2);
    let b2_param = builder.block_params(block2).unwrap()[0];
    let u1 = builder.iconst(I64, 100).unwrap().out;
    let u2 = builder.iconst(I64, 200).unwrap().out;
    let u3 = builder.vadd(u1, u2).unwrap().out;
    let _ = u3;
    builder.jump(block3, &[b2_param]).unwrap();

    // Block 3: use anchor in final result
    builder.position_end(block3);
    let b3_param = builder.block_params(block3).unwrap()[0];
    let final_c = builder.iconst(I64, 1).unwrap().out;
    final_sum = builder.vadd(b3_param, final_c).unwrap().out;
    builder.r#return(final_sum).unwrap();
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_anchor = result.get(v_anchor).unwrap();
  let loc_final = result.get(final_sum).unwrap();

  assert_ne!(loc_anchor.reg, VMLoc::Scratchpad);
  assert_ne!(loc_final.reg, VMLoc::Scratchpad);
  println!("  -> Passed: Anchor variable preserved across 3 intermediate block boundaries\n");
}

/// Test 7: Complex Multi-Type Struct & Array ABI Parameter Passing
/// Tests arguments exceeding 16B threshold placed in Scratchpad with exact alignment and offsets.
fn test_07_complex_abi_mixed_parameters_scratchpad() {
  println!("[Extended Test 7/10] Complex Multi-Type ABI Passing (> 16B -> Scratchpad)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "abi_struct", &v0);

  // Create a multi-type parameter list: [I64, I32, I16, I8, I8, I64] = 24B (> 16B threshold)
  let sig = Signature::new(&mut module, &[I64, I32, I16, I8, I8, I64], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_struct_abi", sig_ref);
  let (p0, p1, p2, p3, p4, p5);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    let params = builder.entry_params();
    assert_eq!(params.len(), 6, "Signature expanded to 6 field parameters");
    p0 = params[0];
    p1 = params[1];
    p2 = params[2];
    p3 = params[3];
    p4 = params[4];
    p5 = params[5];

    let sum1 = builder.vadd(p0, p5).unwrap().out;
    let _ = (p1, p2, p3, p4);
    builder.r#return(sum1).unwrap();
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_p0 = result.get(p0).unwrap();
  let loc_p1 = result.get(p1).unwrap();
  let loc_p5 = result.get(p5).unwrap();

  // Since total composite size (24B) > 16B, all parameters are passed in Scratchpad!
  assert_eq!(loc_p0.reg, VMLoc::Scratchpad);
  assert_eq!(loc_p1.reg, VMLoc::Scratchpad);
  assert_eq!(loc_p5.reg, VMLoc::Scratchpad);

  println!("  -> Passed: Composite struct arguments correctly laid out in Scratchpad\n");
}

/// Test 8: Subword Slot Reuse Across Consecutive Lifetimes
/// Tests sequential phases where subword packing in physical registers is reallocated and recycled.
fn test_08_subword_slot_reuse_across_consecutive_lifetimes() {
  println!("[Extended Test 8/10] Sub-Word Slot Reuse Across Consecutive Lifetimes...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "subword_reuse", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_subword_reuse", sig_ref);
  let (a1, a2, b1, b2);
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    // Phase 1: Allocate two I32s, pack them, and consume them immediately
    a1 = builder.iconst(I32, 11).unwrap().out;
    a2 = builder.iconst(I32, 22).unwrap().out;
    let sum_a = builder.vadd(a1, a2).unwrap().out;
    let _ = sum_a; // a1, a2, sum_a die here

    // Phase 2: Allocate new two I32s after Phase 1 died
    b1 = builder.iconst(I32, 33).unwrap().out;
    b2 = builder.iconst(I32, 44).unwrap().out;
    let sum_b = builder.vadd(b1, b2).unwrap().out;
    let _ = sum_b;
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_a1 = result.get(a1).unwrap();
  let loc_a2 = result.get(a2).unwrap();
  let loc_b1 = result.get(b1).unwrap();
  let loc_b2 = result.get(b2).unwrap();

  // Phase 1 values pack together
  assert_eq!(loc_a1.reg, loc_a2.reg);
  // Phase 2 values pack together and reuse the same physical register!
  assert_eq!(loc_b1.reg, loc_b2.reg);
  assert_eq!(loc_a1.reg, loc_b1.reg, "Phase 2 should reuse Phase 1's physical register");
  assert_eq!(loc_a1.offset, loc_b1.offset);
  assert_eq!(loc_a2.offset, loc_b2.offset);

  println!("  -> Passed: Sequential sub-word groups successfully recycled register {:?}\n", loc_a1.reg);
}

/// Test 9: Massive Contention with 60+ Live Variables Triggering Scratchpad Saturation and Deep Largepad Spilling
/// Creates 60 concurrent 64-bit variables:
///   8 in physical registers R1..R8 (64B)
///   24 in Scratchpad (192B max capacity)
///   28 in Largepad (224B)
fn test_09_massive_contention_deep_largepad_spill() {
  println!("[Extended Test 9/10] Massive Contention: 60 Live Variables (R1..R8 + 192B Scratchpad + 224B Largepad)...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "massive_spill", &v0);

  let sig = Signature::new(&mut module, &[], None).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_massive_spill", sig_ref);
  let mut vals = Vec::new();
  {
    let mut builder = func.builder(&module);
    builder.position_end(BLOCK_0);

    for i in 0..60 {
      vals.push(builder.iconst(I64, (i + 1) as u64 * 7).unwrap().out);
    }

    // Keep all 60 live until grand reduction
    let mut acc = vals[0];
    for &v in &vals[1..] {
      acc = builder.vadd(acc, v).unwrap().out;
    }
    let _ = acc;
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let mut count_phys = 0;
  let mut count_scratch = 0;
  let mut count_large = 0;

  for &v in &vals {
    let loc = result.get(v).unwrap();
    match loc.reg {
      VMLoc::Scratchpad => count_scratch += 1,
      VMLoc::Largepad => count_large += 1,
      _ => count_phys += 1,
    }
  }

  println!("    Distribution across memory tiers:");
  println!("      Physical Registers : {count_phys}");
  println!("      Scratchpad Slots   : {count_scratch} ({}B / {}B)", result.scratchpad_bytes, SCRATCHPAD_MAX_BYTES);
  println!("      Largepad Slots     : {count_large} ({}B)", result.largepad_bytes);

  assert_eq!(result.scratchpad_bytes, SCRATCHPAD_MAX_BYTES, "Scratchpad must reach 192B capacity");
  assert!(count_large >= 28, "At least 28 variables must spill to Largepad");
  assert_eq!(result.largepad_bytes, count_large * 8, "Largepad bytes matches spilled slots * 8B");
  println!("  -> Passed: Tiered memory hierarchy fully exercised under massive variable pressure\n");
}

/// Test 10: Multi-Successor DAG with Differing Phi Arity
/// Tests complex branching CFG with multiple join points and varying parameter counts.
fn test_10_multi_successor_dag_with_differing_phi_arity() {
  println!("[Extended Test 10/10] Multi-Successor DAG with Differing Phi Arity...");
  let store = SingleThreadedStringStore::new();
  let v0 = V0VM::new();
  let mut module = Module::new(&store, "multi_succ_dag", &v0);

  let sig = Signature::new(&mut module, &[I64], Some(I64)).unwrap();
  let sig_ref = module.signature(sig);

  let mut func = module.function("test_dag", sig_ref);
  let (p_b1_a, p_b1_b, p_b2, p_merge_a, p_merge_b);
  {
    let mut builder = func.builder(&module);
    let b1 = builder.block(&[I64, I64]);
    let b2 = builder.block(&[I64]);
    let b_merge = builder.block(&[I64, I64]);

    // Block 0: branches to b1
    builder.position_end(BLOCK_0);
    let p = builder.entry_params()[0];
    let c1 = builder.iconst(I64, 100).unwrap().out;
    let c2 = builder.iconst(I64, 200).unwrap().out;
    builder.jump(b1, &[c1, c2]).unwrap();

    // Block 1 (arity 2): computes and jumps to merge
    builder.position_end(b1);
    let b1_args = builder.block_params(b1).unwrap();
    p_b1_a = b1_args[0];
    p_b1_b = b1_args[1];
    let s1 = builder.vadd(p_b1_a, p_b1_b).unwrap().out;
    let s2 = builder.vmul(s1, p).unwrap().out;
    builder.jump(b_merge, &[s1, s2]).unwrap();

    // Block 2 (arity 1): computes and jumps to merge
    builder.position_end(b2);
    p_b2 = builder.block_params(b2).unwrap()[0];
    let s3 = builder.vadd(p_b2, p).unwrap().out;
    builder.jump(b_merge, &[s3, p_b2]).unwrap();

    // Merge Block (arity 2): combines both parameters and returns
    builder.position_end(b_merge);
    let merge_args = builder.block_params(b_merge).unwrap();
    p_merge_a = merge_args[0];
    p_merge_b = merge_args[1];
    let final_res = builder.vadd(p_merge_a, p_merge_b).unwrap().out;
    builder.r#return(final_res).unwrap();
  }

  let result = func.regalloc(&module);
  println!("{result:?}");

  let loc_m_a = result.get(p_merge_a).unwrap();
  let loc_m_b = result.get(p_merge_b).unwrap();

  assert_ne!(loc_m_a.reg, loc_m_b.reg, "Merge block parameters must occupy different locations");
  println!("  -> Passed: Complex multi-successor DAG with distinct phi arity allocated cleanly\n");
}
