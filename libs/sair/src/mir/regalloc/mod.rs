use std::fmt::{Debug, Formatter, Result};

use crate::{
  StringStore,
  mir::{
    Module,
    block::instr::AHQF,
    function::Function,
  },
};

pub mod backtracking;
pub mod intra_register;
pub mod liveness;
pub mod memory;

pub use backtracking::{AllocatedBlock, BacktrackingAllocator, RegAllocResult};
pub use intra_register::{NUM_PHYS_REGS, PHYS_REGS, RegisterPool};
pub use liveness::{BlockLiveness, LivenessInfo, ProgramPoint, ValueLiveRange};
pub use memory::{SCRATCHPAD_MAX_BYTES, TieredMemoryManager};

/// Perform function-wide backtracking register allocation adhering to the specification
pub fn allocate<'a, T: StringStore>(
  func: &'a Function<'a, T>,
  module: &'a Module<'a, T>,
) -> RegAllocResult {
  let allocator = BacktrackingAllocator::new(func, module);
  allocator.run()
}

impl Debug for RegAllocResult {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    writeln!(f, "; RegAlloc Result:")?;
    writeln!(
      f,
      "; Scratchpad: {}B / {}B, Largepad: {}B",
      self.scratchpad_bytes, SCRATCHPAD_MAX_BYTES, self.largepad_bytes
    )?;

    writeln!(f, "; Allocations ({} values):", self.allocations.len())?;
    let mut sorted_allocs: Vec<_> = self.allocations.iter().collect();
    sorted_allocs.sort_by_key(|(v, _)| v.0);
    for (v, loc) in sorted_allocs {
      write!(f, ";   v{} -> ", v.0)?;
      loc.reg.f(f)?;
      if loc.offset != 0 || loc.width != 8 || loc.count != 1 {
        write!(f, ".of({}:{}x{})", loc.offset, loc.width, loc.count)?;
      }
      writeln!(f)?;
    }

    writeln!(f, "; Lowered MIR:")?;
    for block in &self.lowered_blocks {
      write!(f, "  block #{}", block.id.0)?;
      if !block.params.is_empty() {
        write!(f, "(")?;
        for (i, (v, loc)) in block.params.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "v{}: ", v.0)?;
          loc.reg.f(f)?;
          if loc.offset != 0 {
            write!(f, ".of({})", loc.offset)?;
          }
        }
        write!(f, ")")?;
      }
      writeln!(f, ":")?;

      for inst in &block.instr {
        write!(f, "    ")?;
        inst.format(f)?;
        writeln!(f)?;
      }
    }

    Ok(())
  }
}
