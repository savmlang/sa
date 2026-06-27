use crate::mir::function::ssa::ValueId;

/// Represents a HighLevelInstruction with a register
/// class
pub enum HLInstruction<T: Register> {
  // Break the SaVM add into 3 separate ADD instructions
  // All the 3 map to `vadd` though
  Add {
    src1: T,
    src2: T,
    out: T,
  },
  Adc {
    src1: T,
    src2: T,
    out: T,
  }, // Forced scalar
  Ads {
    src1: T,
    src2: T,
    out: T,
  },

  /// A virtual RETURN instruction
  Return {
    output: T,
  },
}

#[allow(private_bounds)]
pub trait Register: Internal {}
pub(crate) trait Internal {}

impl Internal for ValueId {}
impl Register for ValueId {}
