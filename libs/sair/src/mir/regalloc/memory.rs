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

pub const SCRATCHPAD_MAX_BYTES: usize = 192;

#[derive(Debug, Clone)]
pub struct MemorySlot {
  pub is_scratchpad: bool,
  pub byte_offset: usize,
  pub size: usize,
  pub align: usize,
  pub interval: (ProgramPoint, ProgramPoint),
}

#[derive(Debug, Clone)]
pub struct TieredMemoryManager {
  allocated_slots: Vec<MemorySlot>,
  scratchpad_high_watermark: usize,
  largepad_high_watermark: usize,
  value_allocations: HashMap<ValueId, LocSrc, RandomState>,
}

impl TieredMemoryManager {
  pub fn new() -> Self {
    Self {
      allocated_slots: Vec::new(),
      scratchpad_high_watermark: 0,
      largepad_high_watermark: 0,
      value_allocations: HashMap::with_hasher(RandomState::default()),
    }
  }

  pub fn scratchpad_used(&self) -> usize {
    self.scratchpad_high_watermark
  }

  pub fn largepad_used(&self) -> usize {
    self.largepad_high_watermark
  }

  pub fn get_allocation(&self, val: ValueId) -> Option<LocSrc> {
    self.value_allocations.get(&val).copied()
  }

  /// Reserve a fixed block in Scratchpad (e.g. for >16B input structs at offset 0)
  pub fn reserve_scratchpad_fixed(
    &mut self,
    val: ValueId,
    size: usize,
    align: usize,
    interval: (ProgramPoint, ProgramPoint),
  ) -> LocSrc {
    let slot = MemorySlot {
      is_scratchpad: true,
      byte_offset: 0,
      size,
      align,
      interval,
    };
    self.scratchpad_high_watermark = self.scratchpad_high_watermark.max(size);
    self.allocated_slots.push(slot);

    let loc = LocSrc {
      reg: VMLoc::Scratchpad,
      offset: 0,
      width: size,
      count: 1,
    };
    self.value_allocations.insert(val, loc);
    loc
  }

  /// Allocate a spill or storage slot for a value with tiered fallback:
  /// Scratchpad (<= 192B) -> Largepad (overflow)
  pub fn allocate_slot(
    &mut self,
    val: ValueId,
    size: usize,
    align: usize,
    range: &ValueLiveRange,
  ) -> LocSrc {
    let align = align.max(1);
    let size = size.max(1);

    // Compute overall interval for this range
    let start_p = range.intervals.first().map(|x| x.0).unwrap_or(range.def_point);
    let end_p = range.intervals.last().map(|x| x.1).unwrap_or(range.last_use_point);
    let interval = (start_p, end_p);

    // Step 1: Try to fit in Scratchpad by recycling non-overlapping slots or expanding
    if let Some(offset) = self.find_scratchpad_fit(size, align, interval) {
      let slot = MemorySlot {
        is_scratchpad: true,
        byte_offset: offset,
        size,
        align,
        interval,
      };
      self.scratchpad_high_watermark = self.scratchpad_high_watermark.max(offset + size);
      self.allocated_slots.push(slot);

      let unit = size.max(1);
      let loc = LocSrc {
        reg: VMLoc::Scratchpad,
        offset: (offset / unit) as i8,
        width: size,
        count: 1,
      };
      self.value_allocations.insert(val, loc);
      return loc;
    }

    // Step 2: Fall back to Largepad
    let offset = self.allocate_largepad(size, align, interval);
    let slot = MemorySlot {
      is_scratchpad: false,
      byte_offset: offset,
      size,
      align,
      interval,
    };
    self.largepad_high_watermark = self.largepad_high_watermark.max(offset + size);
    self.allocated_slots.push(slot);

    let unit = size.max(1);
    let loc = LocSrc {
      reg: VMLoc::Largepad,
      offset: (offset / unit) as i8,
      width: size,
      count: 1,
    };
    self.value_allocations.insert(val, loc);
    loc
  }

  fn find_scratchpad_fit(
    &self,
    size: usize,
    align: usize,
    interval: (ProgramPoint, ProgramPoint),
  ) -> Option<usize> {
    // Check candidate offsets from 0 up to SCRATCHPAD_MAX_BYTES - size
    let mut candidate = 0;
    while candidate + size <= SCRATCHPAD_MAX_BYTES {
      // Check aligned
      candidate = candidate.next_multiple_of(align);
      if candidate + size > SCRATCHPAD_MAX_BYTES {
        break;
      }

      // Check for conflict with existing scratchpad slots overlapping in time
      let mut conflict = false;
      for slot in &self.allocated_slots {
        if !slot.is_scratchpad {
          continue;
        }
        // Check time overlap
        if slot.interval.0 <= interval.1 && interval.0 <= slot.interval.1 {
          // Check spatial overlap
          let slot_end = slot.byte_offset + slot.size;
          let cand_end = candidate + size;
          if candidate < slot_end && slot.byte_offset < cand_end {
            conflict = true;
            // Advance candidate past this slot
            candidate = slot_end;
            break;
          }
        }
      }

      if !conflict {
        return Some(candidate);
      }
    }

    None
  }

  fn allocate_largepad(
    &self,
    size: usize,
    align: usize,
    interval: (ProgramPoint, ProgramPoint),
  ) -> usize {
    let mut candidate: usize = 0;
    loop {
      candidate = candidate.next_multiple_of(align);

      let mut conflict = false;
      for slot in &self.allocated_slots {
        if slot.is_scratchpad {
          continue;
        }
        if slot.interval.0 <= interval.1 && interval.0 <= slot.interval.1 {
          let slot_end = slot.byte_offset + slot.size;
          let cand_end = candidate + size;
          if candidate < slot_end && slot.byte_offset < cand_end {
            conflict = true;
            candidate = slot_end;
            break;
          }
        }
      }

      if !conflict {
        return candidate;
      }
    }
  }
}
