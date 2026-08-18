use std::collections::{HashMap, HashSet};
use rapidhash::fast::RandomState;

use crate::{
  StringStore,
  mir::{
    block::{BlockId, instr::HLInstruction},
    function::{Function, ssa::ValueId},
  },
};

pub type ProgramPoint = usize;

#[derive(Debug, Clone)]
pub struct BlockLiveness {
  pub block_id: BlockId,
  pub start_point: ProgramPoint,
  pub end_point: ProgramPoint,
  pub use_gen: HashSet<ValueId, RandomState>,
  pub def_kill: HashSet<ValueId, RandomState>,
  pub live_in: HashSet<ValueId, RandomState>,
  pub live_out: HashSet<ValueId, RandomState>,
}

#[derive(Debug, Clone)]
pub struct ValueLiveRange {
  pub val: ValueId,
  pub def_point: ProgramPoint,
  pub use_points: Vec<ProgramPoint>,
  pub last_use_point: ProgramPoint,
  pub intervals: Vec<(ProgramPoint, ProgramPoint)>,
  pub is_block_param: bool,
  pub is_live_across_blocks: bool,
  pub spill_weight: f64,
}

impl ValueLiveRange {
  pub fn is_live_at(&self, point: ProgramPoint) -> bool {
    self.intervals.iter().any(|&(s, e)| point >= s && point <= e)
  }

  pub fn overlaps(&self, other: &ValueLiveRange) -> bool {
    for &(s1, e1) in &self.intervals {
      for &(s2, e2) in &other.intervals {
        if s1 <= e2 && s2 <= e1 {
          return true;
        }
      }
    }
    false
  }

  pub fn overlaps_range(&self, start: ProgramPoint, end: ProgramPoint) -> bool {
    self.intervals.iter().any(|&(s, e)| s <= end && start <= e)
  }
}

#[derive(Debug, Clone)]
pub struct LivenessInfo {
  pub block_liveness: Vec<BlockLiveness>,
  pub value_ranges: HashMap<ValueId, ValueLiveRange, RandomState>,
  pub inst_points: HashMap<(BlockId, usize), ProgramPoint, RandomState>,
  pub point_to_inst: HashMap<ProgramPoint, (BlockId, usize), RandomState>,
  pub max_point: ProgramPoint,
}

impl LivenessInfo {
  pub fn compute<T: StringStore>(func: &Function<'_, T>) -> Self {
    let mut inst_points = HashMap::with_hasher(RandomState::default());
    let mut point_to_inst = HashMap::with_hasher(RandomState::default());
    let mut block_bounds: Vec<(ProgramPoint, ProgramPoint)> = Vec::with_capacity(func.blocks.len());

    let mut current_point: ProgramPoint = 0;

    // Step 1: Assign global program points to instructions
    for (b_idx, block) in func.blocks.iter().enumerate() {
      let b_id = BlockId(b_idx);
      let b_start = current_point;

      // Program point for block parameters definition
      current_point += 2;

      for (i_idx, _) in block.instr.iter().enumerate() {
        let p = current_point;
        inst_points.insert((b_id, i_idx), p);
        point_to_inst.insert(p, (b_id, i_idx));
        current_point += 2;
      }

      let b_end = current_point;
      current_point += 2;
      block_bounds.push((b_start, b_end));
    }

    let max_point = current_point;

    // Step 2: Extract Defs and Uses for each block
    let mut block_liveness = Vec::with_capacity(func.blocks.len());
    let mut def_points: HashMap<ValueId, ProgramPoint, RandomState> = HashMap::with_hasher(RandomState::default());
    let mut use_points_map: HashMap<ValueId, Vec<ProgramPoint>, RandomState> = HashMap::with_hasher(RandomState::default());
    let mut is_param_map: HashMap<ValueId, bool, RandomState> = HashMap::with_hasher(RandomState::default());

    for (b_idx, block) in func.blocks.iter().enumerate() {
      let b_id = BlockId(b_idx);
      let (b_start, b_end) = block_bounds[b_idx];

      let mut use_gen = HashSet::with_hasher(RandomState::default());
      let mut def_kill = HashSet::with_hasher(RandomState::default());

      // Block parameters are defined at block entry
      for &param in &block.params {
        def_kill.insert(param);
        def_points.insert(param, b_start);
        is_param_map.insert(param, true);
      }

      // Instructions
      for (i_idx, inst) in block.instr.iter().enumerate() {
        let read_p = inst_points[&(b_id, i_idx)];
        let write_p = read_p + 1;

        // Collect uses (sources and jump args) at read_p
        let mut uses = Vec::new();
        inst.src(|&v| {
          uses.push(v);
        });

        // Check Jump / JumpIf args
        match inst {
          HLInstruction::Jump { args, .. } => {
            for &arg in args.iter() {
              uses.push(arg);
            }
          }
          HLInstruction::JumpIf { args, .. } => {
            for &arg in args.iter() {
              uses.push(arg);
            }
          }
          _ => {}
        }

        for u in uses {
          if !def_kill.contains(&u) {
            use_gen.insert(u);
          }
          use_points_map.entry(u).or_default().push(read_p);
        }

        // Collect defs (outputs) at write_p
        inst.outputs(|&out| {
          def_kill.insert(out);
          def_points.insert(out, write_p);
          is_param_map.entry(out).or_insert(false);
        });
      }

      block_liveness.push(BlockLiveness {
        block_id: b_id,
        start_point: b_start,
        end_point: b_end,
        use_gen,
        def_kill,
        live_in: HashSet::with_hasher(RandomState::default()),
        live_out: HashSet::with_hasher(RandomState::default()),
      });
    }

    // Step 3: Backward dataflow iteration to compute LiveIn and LiveOut
    let mut changed = true;
    while changed {
      changed = false;

      for b_idx in (0..func.blocks.len()).rev() {
        let block = &func.blocks[b_idx];

        // LiveOut[B] = Union over S in Succ(B) of LiveIn[S]
        let mut new_live_out = HashSet::with_hasher(RandomState::default());
        for &succ in &block.succ {
          if let Some(succ_live) = block_liveness.get(succ.0) {
            for &v in &succ_live.live_in {
              new_live_out.insert(v);
            }
          }
        }

        // Also, any argument passed in Jump targeting successor is live-out of B
        if let Some(last_inst) = block.instr.last() {
          match last_inst {
            HLInstruction::Jump { args, .. } => {
              for &arg in args.iter() {
                new_live_out.insert(arg);
              }
            }
            HLInstruction::JumpIf { args, .. } => {
              for &arg in args.iter() {
                new_live_out.insert(arg);
              }
            }
            _ => {}
          }
        }

        // LiveIn[B] = UseGen[B] Union (LiveOut[B] \ DefKill[B])
        let mut new_live_in = block_liveness[b_idx].use_gen.clone();
        for &v in &new_live_out {
          if !block_liveness[b_idx].def_kill.contains(&v) {
            new_live_in.insert(v);
          }
        }

        if new_live_out != block_liveness[b_idx].live_out || new_live_in != block_liveness[b_idx].live_in {
          changed = true;
          block_liveness[b_idx].live_out = new_live_out;
          block_liveness[b_idx].live_in = new_live_in;
        }
      }
    }

    // Step 4: Construct Live Ranges for each SSA Value
    let mut value_ranges = HashMap::with_hasher(RandomState::default());

    for (ssa_idx, _) in func.ssa.iter().enumerate() {
      let val = ValueId(ssa_idx);
      let def_p = def_points.get(&val).copied().unwrap_or(0);
      let uses = use_points_map.remove(&val).unwrap_or_default();
      let is_param = is_param_map.get(&val).copied().unwrap_or(false);

      let mut intervals = Vec::new();
      let mut is_live_across_blocks = false;

      // Find all blocks where val is live
      for bl in block_liveness.iter() {
        let is_def_in_b = bl.def_kill.contains(&val);
        let is_live_in_b = bl.live_in.contains(&val);
        let is_live_out_b = bl.live_out.contains(&val);

        if is_live_in_b || is_def_in_b {
          let start = if is_def_in_b {
            def_p
          } else {
            bl.start_point
          };

          let end = if is_live_out_b {
            is_live_across_blocks = true;
            bl.end_point
          } else {
            // Last use within this block
            let last_use_in_block = uses
              .iter()
              .filter(|&&p| p >= bl.start_point && p <= bl.end_point)
              .copied()
              .max()
              .unwrap_or(start);
            last_use_in_block
          };

          if end >= start {
            intervals.push((start, end));
          }
        }
      }

      // If no interval recorded (e.g. single block def and uses)
      if intervals.is_empty() {
        let last_u = uses.iter().copied().max().unwrap_or(def_p);
        intervals.push((def_p, last_u));
      }

      // Merge contiguous/overlapping intervals
      intervals.sort_by_key(|&(s, _)| s);
      let mut merged_intervals: Vec<(ProgramPoint, ProgramPoint)> = Vec::new();
      for (s, e) in intervals {
        if let Some(last) = merged_intervals.last_mut() {
          if s <= last.1 + 2 {
            last.1 = last.1.max(e);
            continue;
          }
        }
        merged_intervals.push((s, e));
      }

      let last_use_point = uses.iter().copied().max().unwrap_or(def_p);
      let range_len = if let Some(last) = merged_intervals.last() {
        let first = merged_intervals.first().unwrap();
        last.1.saturating_sub(first.0).max(1)
      } else {
        1
      };

      // Spill weight = use_count / live_range_length (higher weight = less likely to spill)
      let spill_weight = (uses.len() as f64 + 1.0) / (range_len as f64);

      value_ranges.insert(
        val,
        ValueLiveRange {
          val,
          def_point: def_p,
          use_points: uses,
          last_use_point,
          intervals: merged_intervals,
          is_block_param: is_param,
          is_live_across_blocks,
          spill_weight,
        },
      );
    }

    Self {
      block_liveness,
      value_ranges,
      inst_points,
      point_to_inst,
      max_point,
    }
  }
}
