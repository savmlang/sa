use std::collections::HashMap;
use rapidhash::fast::RandomState;

use crate::{
  StringStore,
  llir::instr::loc::VMLoc,
  mir::{
    Module,
    block::{BlockId, instr::{HLInstruction, loc::LocSrc}},
    function::{Function, ssa::ValueId},
    regalloc::{
      intra_register::RegisterPool,
      liveness::{LivenessInfo, ValueLiveRange},
      memory::TieredMemoryManager,
    },
  },
};

#[derive(Debug, Clone)]
pub struct AllocatedBlock {
  pub id: BlockId,
  pub params: Vec<(ValueId, LocSrc)>,
  pub instr: Vec<HLInstruction<LocSrc>>,
}

#[derive(Clone)]
pub struct RegAllocResult {
  pub allocations: HashMap<ValueId, LocSrc, RandomState>,
  pub scratchpad_bytes: usize,
  pub largepad_bytes: usize,
  pub lowered_blocks: Vec<AllocatedBlock>,
}

impl RegAllocResult {
  pub fn get(&self, val: ValueId) -> Option<LocSrc> {
    self.allocations.get(&val).copied()
  }
}

pub struct BacktrackingAllocator<'a, T: StringStore> {
  func: &'a Function<'a, T>,
  module: &'a Module<'a, T>,
  liveness: LivenessInfo,
  regs: RegisterPool,
  memory: TieredMemoryManager,
  allocations: HashMap<ValueId, LocSrc, RandomState>,
  pinned_values: HashMap<ValueId, LocSrc, RandomState>,
}

impl<'a, T: StringStore> BacktrackingAllocator<'a, T> {
  pub fn new(func: &'a Function<'a, T>, module: &'a Module<'a, T>) -> Self {
    let liveness = LivenessInfo::compute(func);
    Self {
      func,
      module,
      liveness,
      regs: RegisterPool::new(),
      memory: TieredMemoryManager::new(),
      allocations: HashMap::with_hasher(RandomState::default()),
      pinned_values: HashMap::with_hasher(RandomState::default()),
    }
  }

  fn get_value_size_and_align(&self, val: ValueId) -> (usize, usize) {
    let tag = match self.func.get_ssa(val) {
      Some(ssa) => ssa.typetag,
      None => return (8, 8),
    };

    let ty = match self.module.type_data(tag) {
      Some(t) => t,
      None => return (8, 8),
    };

    (ty.size(self.module), ty.align(self.module))
  }

  /// Handles ABI input and output calling conventions according to Specification Section 2
  fn handle_abi_conventions(&mut self) {
    let sig_ref = self.func.sig();
    let sig = match self.module.signature_data(sig_ref) {
      Some(s) => s,
      None => return,
    };

    // 1. Handle Inputs (Entry block params)
    if let Some(entry_block) = self.func.blocks.first() {
      if !entry_block.params.is_empty() {
        if let Some(arg_type_ref) = sig.args {
          let arg_type = self.module.type_data(arg_type_ref).unwrap();
          let total_arg_size = arg_type.size(self.module);

          if total_arg_size <= 16 {
            // ABI Rule: <= 16B passed directly in R7 and R8
            let mut current_byte_offset: usize = 0;

            for &param in &entry_block.params {
              let (size, align) = self.get_value_size_and_align(param);
              current_byte_offset = current_byte_offset.next_multiple_of(align.max(1));

              let loc = if current_byte_offset + size <= 8 {
                // Fits in R7
                let offset = (current_byte_offset / size.min(8).max(1)) as i8;
                LocSrc {
                  reg: VMLoc::R7,
                  offset,
                  width: size.min(8).max(1),
                  count: 1,
                }
              } else if current_byte_offset < 16 {
                // Spans or placed in R8
                let r8_byte = current_byte_offset.saturating_sub(8);
                let offset = (r8_byte / size.min(8).max(1)) as i8;
                LocSrc {
                  reg: VMLoc::R8,
                  offset,
                  width: size.min(8).max(1),
                  count: 1,
                }
              } else {
                // Fallback if needed
                LocSrc {
                  reg: VMLoc::R8,
                  offset: 0,
                  width: 8,
                  count: 1,
                }
              };

              self.pinned_values.insert(param, loc);
              self.allocations.insert(param, loc);

              // Reserve in register pool for the initial range of the param
              if let Some(range) = self.liveness.value_ranges.get(&param) {
                let byte_in_reg = if loc.reg == VMLoc::R7 {
                  current_byte_offset
                } else {
                  current_byte_offset.saturating_sub(8)
                };
                self.regs.try_assign_specific(
                  param,
                  loc.reg,
                  byte_in_reg,
                  loc.width,
                  &range.intervals,
                );
              }

              current_byte_offset += size;
            }
          } else {
            // ABI Rule: > 16B placed in Scratchpad, R7 holds pointer address
            let mut current_offset: usize = 0;
            for &param in &entry_block.params {
              let (size, align) = self.get_value_size_and_align(param);
              current_offset = current_offset.next_multiple_of(align.max(1));

              let loc = LocSrc {
                reg: VMLoc::Scratchpad,
                offset: (current_offset / align.max(1).min(8)) as i8,
                width: size,
                count: 1,
              };

              self.pinned_values.insert(param, loc);
              self.allocations.insert(param, loc);

              if let Some(range) = self.liveness.value_ranges.get(&param) {
                let start_p = range.intervals.first().map(|x| x.0).unwrap_or(0);
                let end_p = range.intervals.last().map(|x| x.1).unwrap_or(0);
                self.memory.reserve_scratchpad_fixed(param, size, align, (start_p, end_p));
              }

              current_offset += size;
            }
          }
        }
      }
    }

    // 2. Handle Outputs (Return instructions)
    for block in &self.func.blocks {
      for inst in &block.instr {
        if let HLInstruction::Return { out: out_val } = inst {
          let (size, _) = self.get_value_size_and_align(*out_val);
          let loc = if size <= 8 {
            LocSrc {
              reg: VMLoc::R7,
              offset: 0,
              width: size.max(1),
              count: 1,
            }
          } else {
            LocSrc {
              reg: VMLoc::R7,
              offset: 0,
              width: 8,
              count: 2,
            }
          };

          // If not already pinned by input, prefer R7 for return
          if !self.pinned_values.contains_key(out_val) {
            self.pinned_values.insert(*out_val, loc);
          }
        }
      }
    }
  }

  /// Run the function-wide backtracking allocation
  pub fn run(mut self) -> RegAllocResult {
    self.handle_abi_conventions();

    // Collect all SSA values to allocate
    let mut unallocated: Vec<ValueId> = (0..self.func.ssa.len())
      .map(ValueId)
      .filter(|v| !self.allocations.contains_key(v))
      .collect();

    // Sort unallocated values by priority:
    // Higher priority (allocated first):
    // 1. Pinned values
    // 2. Values with shorter live ranges and high spill weights
    // 3. Smaller sub-word widths (to pack them together first)
    unallocated.sort_by(|&a, &b| {
      let a_pinned = self.pinned_values.contains_key(&a);
      let b_pinned = self.pinned_values.contains_key(&b);
      if a_pinned != b_pinned {
        return b_pinned.cmp(&a_pinned);
      }

      let (a_size, _) = self.get_value_size_and_align(a);
      let (b_size, _) = self.get_value_size_and_align(b);

      let a_range = self.liveness.value_ranges.get(&a);
      let b_range = self.liveness.value_ranges.get(&b);

      let a_start = a_range.map(|r| r.def_point).unwrap_or(0);
      let b_start = b_range.map(|r| r.def_point).unwrap_or(0);

      let a_weight = a_range.map(|r| r.spill_weight).unwrap_or(0.0);
      let b_weight = b_range.map(|r| r.spill_weight).unwrap_or(0.0);

      a_start
        .cmp(&b_start)
        .then_with(|| b_size.cmp(&a_size))
        .then_with(|| b_weight.partial_cmp(&a_weight).unwrap_or(std::cmp::Ordering::Equal))
    });

    // Backtracking allocation loop
    self.allocate_values(&unallocated);

    // Build the lowered blocks with HLInstruction<LocSrc>
    let mut lowered_blocks = Vec::with_capacity(self.func.blocks.len());

    for (b_idx, block) in self.func.blocks.iter().enumerate() {
      let b_id = BlockId(b_idx);

      let params = block
        .params
        .iter()
        .map(|&p| {
          let loc = self.allocations.get(&p).copied().unwrap_or(LocSrc {
            reg: VMLoc::R1,
            offset: 0,
            width: 8,
            count: 1,
          });
          (p, loc)
        })
        .collect();

      let mut lowered_instr = Vec::with_capacity(block.instr.len());
      for inst in &block.instr {
        let lowered = inst.map(|&v| {
          self.allocations.get(&v).copied().unwrap_or(LocSrc {
            reg: VMLoc::R1,
            offset: 0,
            width: 8,
            count: 1,
          })
        });
        lowered_instr.push(lowered);
      }

      lowered_blocks.push(AllocatedBlock {
        id: b_id,
        params,
        instr: lowered_instr,
      });
    }

    RegAllocResult {
      allocations: self.allocations,
      scratchpad_bytes: self.memory.scratchpad_used(),
      largepad_bytes: self.memory.largepad_used(),
      lowered_blocks,
    }
  }

  fn allocate_values(&mut self, values: &[ValueId]) {
    for &val in values {
      if self.allocations.contains_key(&val) {
        continue;
      }

      self.allocate_single_value_backtracking(val, 0);
    }
  }

  fn allocate_single_value_backtracking(&mut self, val: ValueId, depth: usize) -> bool {
    let (size, align) = self.get_value_size_and_align(val);
    let range = match self.liveness.value_ranges.get(&val) {
      Some(r) => r.clone(),
      None => ValueLiveRange {
        val,
        def_point: 0,
        use_points: Vec::new(),
        last_use_point: 0,
        intervals: vec![(0, 0)],
        is_block_param: false,
        is_live_across_blocks: false,
        spill_weight: 1.0,
      },
    };

    // Step 1: If value is pinned to a specific location (e.g. Return or Input)
    if let Some(&pinned_loc) = self.pinned_values.get(&val) {
      if pinned_loc.reg == VMLoc::Scratchpad || pinned_loc.reg == VMLoc::Largepad {
        self.allocations.insert(val, pinned_loc);
        return true;
      }

      // Try assigning to the pinned physical register
      let byte_offset = pinned_loc.byte_offset();
      if let Some(loc) = self.regs.try_assign_specific(
        val,
        pinned_loc.reg,
        byte_offset,
        pinned_loc.width,
        &range.intervals,
      ) {
        self.allocations.insert(val, loc);
        return true;
      }
    }

    // Step 2: Try physical register allocation with Intelligent Intra-Register Fitting (Optimization A)
    let candidate_regs = if let Some(&pinned_loc) = self.pinned_values.get(&val) {
      vec![pinned_loc.reg]
    } else {
      vec![
        VMLoc::R1,
        VMLoc::R2,
        VMLoc::R3,
        VMLoc::R4,
        VMLoc::R5,
        VMLoc::R6,
        VMLoc::R7,
        VMLoc::R8,
      ]
    };

    if let Some(loc) = self.regs.find_best_fit(val, size, &range, &candidate_regs) {
      self.allocations.insert(val, loc);
      return true;
    }

    // Step 3: Backtracking & Eviction
    // If maximum backtracking depth (e.g. 3) not exceeded, try evicting lower-priority conflicting values
    if depth < 3 {
      if let Some((reg, byte_offset, conflicts)) =
        self.regs.find_eviction_candidate(size, &range, &candidate_regs)
      {
        // Check if all conflicting values have lower spill weight than the current value
        let all_lower_weight = conflicts.iter().all(|&c| {
          let c_weight = self
            .liveness
            .value_ranges
            .get(&c)
            .map(|r| r.spill_weight)
            .unwrap_or(0.0);
          c_weight <= range.spill_weight && !self.pinned_values.contains_key(&c)
        });

        if all_lower_weight {
          // Save state for potential backtrack
          let mut evicted_allocs = Vec::new();
          for &c in &conflicts {
            if let Some(c_loc) = self.regs.remove_allocation(c) {
              self.allocations.remove(&c);
              evicted_allocs.push((c, c_loc));
            }
          }

          // Assign current value to the cleared slot
          if let Some(loc) = self.regs.try_assign_specific(
            val,
            reg,
            byte_offset,
            size,
            &range.intervals,
          ) {
            self.allocations.insert(val, loc);

            // Re-allocate evicted values (into alternative physical registers or tiered memory)
            let mut all_reallocated = true;
            for (c, _) in evicted_allocs {
              if !self.allocate_single_value_backtracking(c, depth + 1) {
                all_reallocated = false;
                break;
              }
            }

            if all_reallocated {
              return true;
            }
          }
        }
      }
    }

    // Step 4: Tiered Memory Fallback (Optimization D & Partial Spilling C)
    // Scratchpad (<= 192B) -> Largepad
    let loc = self.memory.allocate_slot(val, size, align, &range);
    self.allocations.insert(val, loc);
    true
  }
}
