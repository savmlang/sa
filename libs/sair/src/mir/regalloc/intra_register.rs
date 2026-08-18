use std::collections::HashMap;
use rapidhash::fast::RandomState;

use crate::{
  llir::instr::loc::VMLoc,
  mir::{
    block::instr::loc::LocSrc,
    function::ssa::ValueId,
    regalloc::liveness::{ProgramPoint, ValueLiveRange},
  },
};

pub const NUM_PHYS_REGS: usize = 8;

pub const PHYS_REGS: [VMLoc; 8] = [
  VMLoc::R1,
  VMLoc::R2,
  VMLoc::R3,
  VMLoc::R4,
  VMLoc::R5,
  VMLoc::R6,
  VMLoc::R7,
  VMLoc::R8,
];

#[derive(Debug, Clone)]
pub struct SubwordReservation {
  pub val: ValueId,
  pub byte_start: usize,
  pub byte_width: usize,
  pub intervals: Vec<(ProgramPoint, ProgramPoint)>,
}

#[derive(Debug, Clone)]
pub struct PhysicalRegisterState {
  pub reg: VMLoc,
  pub reservations: Vec<SubwordReservation>,
}

impl PhysicalRegisterState {
  pub fn new(reg: VMLoc) -> Self {
    Self {
      reg,
      reservations: Vec::new(),
    }
  }

  /// Checks if bytes [byte_start, byte_start + width) are free during all intervals of range
  pub fn is_free(
    &self,
    byte_start: usize,
    width: usize,
    intervals: &[(ProgramPoint, ProgramPoint)],
  ) -> bool {
    let byte_end = byte_start + width;
    if byte_end > 8 {
      return false;
    }

    for res in &self.reservations {
      let res_end = res.byte_start + res.byte_width;
      // Check byte overlap
      if byte_start < res_end && res.byte_start < byte_end {
        // Check time overlap
        for &(s1, e1) in intervals {
          for &(s2, e2) in &res.intervals {
            if s1 <= e2 && s2 <= e1 {
              return false;
            }
          }
        }
      }
    }

    true
  }

  /// Collects all ValueIds that conflict with the proposed byte slice and intervals
  pub fn get_conflicts(
    &self,
    byte_start: usize,
    width: usize,
    intervals: &[(ProgramPoint, ProgramPoint)],
  ) -> Vec<ValueId> {
    let mut conflicts = Vec::new();
    let byte_end = byte_start + width;

    for res in &self.reservations {
      let res_end = res.byte_start + res.byte_width;
      if byte_start < res_end && res.byte_start < byte_end {
        let mut time_overlap = false;
        for &(s1, e1) in intervals {
          for &(s2, e2) in &res.intervals {
            if s1 <= e2 && s2 <= e1 {
              time_overlap = true;
              break;
            }
          }
          if time_overlap {
            break;
          }
        }

        if time_overlap && !conflicts.contains(&res.val) {
          conflicts.push(res.val);
        }
      }
    }

    conflicts
  }

  pub fn add_reservation(
    &mut self,
    val: ValueId,
    byte_start: usize,
    byte_width: usize,
    intervals: Vec<(ProgramPoint, ProgramPoint)>,
  ) {
    self.reservations.push(SubwordReservation {
      val,
      byte_start,
      byte_width,
      intervals,
    });
  }

  pub fn remove_reservation(&mut self, val: ValueId) -> bool {
    let initial_len = self.reservations.len();
    self.reservations.retain(|r| r.val != val);
    self.reservations.len() < initial_len
  }

  /// Returns how many sub-words are currently packed in this register
  pub fn active_subword_count(&self) -> usize {
    self.reservations.len()
  }
}

#[derive(Debug, Clone)]
pub struct RegisterPool {
  pub registers: [PhysicalRegisterState; NUM_PHYS_REGS],
  pub val_locations: HashMap<ValueId, LocSrc, RandomState>,
}

impl RegisterPool {
  pub fn new() -> Self {
    Self {
      registers: [
        PhysicalRegisterState::new(VMLoc::R1),
        PhysicalRegisterState::new(VMLoc::R2),
        PhysicalRegisterState::new(VMLoc::R3),
        PhysicalRegisterState::new(VMLoc::R4),
        PhysicalRegisterState::new(VMLoc::R5),
        PhysicalRegisterState::new(VMLoc::R6),
        PhysicalRegisterState::new(VMLoc::R7),
        PhysicalRegisterState::new(VMLoc::R8),
      ],
      val_locations: HashMap::with_hasher(RandomState::default()),
    }
  }

  pub fn reg_index(reg: VMLoc) -> Option<usize> {
    match reg {
      VMLoc::R1 => Some(0),
      VMLoc::R2 => Some(1),
      VMLoc::R3 => Some(2),
      VMLoc::R4 => Some(3),
      VMLoc::R5 => Some(4),
      VMLoc::R6 => Some(5),
      VMLoc::R7 => Some(6),
      VMLoc::R8 => Some(7),
      _ => None,
    }
  }

  pub fn get_allocation(&self, val: ValueId) -> Option<LocSrc> {
    self.val_locations.get(&val).copied()
  }

  /// Attempt to assign a specific register and sub-word byte offset (e.g. for ABI requirements)
  pub fn try_assign_specific(
    &mut self,
    val: ValueId,
    reg: VMLoc,
    byte_offset: usize,
    width: usize,
    intervals: &[(ProgramPoint, ProgramPoint)],
  ) -> Option<LocSrc> {
    let idx = Self::reg_index(reg)?;
    let width = width.min(8).max(1);

    if self.registers[idx].is_free(byte_offset, width, intervals) {
      self.registers[idx].add_reservation(val, byte_offset, width, intervals.to_vec());
      let offset = (byte_offset / width) as i8;
      let loc = LocSrc {
        reg,
        offset,
        width,
        count: 1,
      };
      self.val_locations.insert(val, loc);
      return Some(loc);
    }

    None
  }

  /// Intelligent Intra-Register Fitting (Optimization A):
  /// Searches physical registers to pack sub-words efficiently.
  /// Prefers registers that already contain sub-words to achieve maximum intra-register packing.
  pub fn find_best_fit(
    &mut self,
    val: ValueId,
    width: usize,
    range: &ValueLiveRange,
    allowed_regs: &[VMLoc],
  ) -> Option<LocSrc> {
    if width > 8 || width == 0 {
      return None;
    }
    let intervals = &range.intervals;

    // Sub-word candidate byte offsets: 0, width, 2*width, ..., up to 8 - width
    let mut candidate_byte_offsets = Vec::new();
    let mut b = 0;
    while b + width <= 8 {
      candidate_byte_offsets.push(b);
      b += width;
    }

    // Sort candidate registers: prioritize registers with existing subwords for dense packing
    let mut reg_indices: Vec<usize> = allowed_regs
      .iter()
      .filter_map(|&r| Self::reg_index(r))
      .collect();

    // Prefer registers that already have sub-words (< 8B width) allocated (most packed first), then empty registers
    reg_indices.sort_by_key(|&idx| {
      let count = self.registers[idx].active_subword_count();
      if count > 0 {
        (0, -(count as isize), idx)
      } else {
        (1, 0, idx)
      }
    });

    for idx in reg_indices {
      for &byte_offset in &candidate_byte_offsets {
        if self.registers[idx].is_free(byte_offset, width, intervals) {
          self.registers[idx].add_reservation(val, byte_offset, width, intervals.to_vec());
          let reg = PHYS_REGS[idx];
          let offset = (byte_offset / width) as i8;
          let loc = LocSrc {
            reg,
            offset,
            width,
            count: 1,
          };
          self.val_locations.insert(val, loc);
          return Some(loc);
        }
      }
    }

    None
  }

  /// Remove allocation for a value (used for backtracking and eviction)
  pub fn remove_allocation(&mut self, val: ValueId) -> Option<LocSrc> {
    let loc = self.val_locations.remove(&val)?;
    if let Some(idx) = Self::reg_index(loc.reg) {
      self.registers[idx].remove_reservation(val);
    }
    Some(loc)
  }

  /// Find eviction candidates: returns a candidate physical register, byte offset, and conflicting ValueIds
  pub fn find_eviction_candidate(
    &self,
    width: usize,
    range: &ValueLiveRange,
    allowed_regs: &[VMLoc],
  ) -> Option<(VMLoc, usize, Vec<ValueId>)> {
    if width > 8 || width == 0 {
      return None;
    }
    let intervals = &range.intervals;

    let mut candidate_byte_offsets = Vec::new();
    let mut b = 0;
    while b + width <= 8 {
      candidate_byte_offsets.push(b);
      b += width;
    }

    let mut best_candidate = None;
    let mut min_conflict_weight = f64::MAX;

    for &reg in allowed_regs {
      let idx = match Self::reg_index(reg) {
        Some(i) => i,
        None => continue,
      };

      for &byte_offset in &candidate_byte_offsets {
        let conflicts = self.registers[idx].get_conflicts(byte_offset, width, intervals);
        if !conflicts.is_empty() {
          let conflict_count = conflicts.len();
          // Lower conflict count and lower weight is preferred
          let weight = conflict_count as f64;
          if weight < min_conflict_weight {
            min_conflict_weight = weight;
            best_candidate = Some((reg, byte_offset, conflicts));
          }
        }
      }
    }

    best_candidate
  }
}
