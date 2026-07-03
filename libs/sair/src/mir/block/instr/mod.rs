use crate::mir::{block::BlockId, function::ssa::ValueId};

pub mod loc;

macro_rules! instloader {
  (
    $(
      $(#[$meta:meta])*
      $name:ident $({ $( $imm:ident: $ty:ident ),* })? ($($arg:ident),*) -> ($($out:ident),*)
    )*
  ) => {
    /// `V*` instructions support BOTH vector and scalar values
    /// non `V` prefixed instructions are scalar only
    ///
    /// `V_` prefixed instructions mean that they selectively accept vectors
    pub enum HLInstruction<T: Register> {
      $(
        $(#[$meta])*
        $name {
          $(
            $arg: T,
          )*

          $(
            $out: T,
          )*

          $(
            $(
              $imm: $ty,
            )*
          )?
        }
      ),*
    }

    impl<T: Register> HLInstruction<T> {
      pub fn all_vals<E>(&self, mut cb: E)
      where
        E: FnMut(&T)
      {
        match self {
          $(
            Self::$name {
              $(
                $arg,
              )*
              $(
                $out,
              )* ..
            } => {
              $(
                cb($arg);
              )*
              $(
                cb($out);
              )*
            }
          ),*
        }
      }

      pub fn src<E>(&self, mut cb: E)
      where
        E: FnMut(&T)
      {
        match self {
          $(
            Self::$name {
              $(
                $arg,
              )* ..
            } => {
              $(
                cb($arg);
              )*
            }
          ),*
        }
      }

      pub fn outputs<E>(&self, mut cb: E)
      where
        E: FnMut(&T)
      {
        match self {
          $(
            Self::$name {
              $(
                $out,
              )* ..
            } => {
              $(
                cb($out);
              )*
            }
          ),*
        }
      }
    }
  };
}

instloader! {
  // --- INTEGRAL ARITHMATIC ---

  // Addition Portal
  Vadd(src1, src2) -> (out)
  Vads(src1, src2) -> (out)
  Adc(src1, src2) -> (out)

  // Subtraction Portal
  VSub(src1, src2) -> (out)
  VSsat(src1, src2) -> (out)
  Sbb(src1, src2) -> (out)

  // Multiplication
  VMulLo(src1, src2) -> (out)
  VMulHi(src1, src2) -> (out)

  /// out is 2x the size of src
  VMulWide(src1, src2) -> (out)

  // Division & Reminder
  Div(src, divisor) -> (out)
  Rem(src, divisor) -> (out)

  // --- FLOAT ARITHMATIC ---

  VAddf(a, b) -> (out)
  VSubf(a, b) -> (out)
  VMulf(a, b) -> (out)
  VDivf(a, b) -> (out)

  /// Computes `a*b + c` as a single step
  VFma(a, b, c) -> (out)

  // --- CONTROL FLOW ---

  Jump { block: BlockId } () -> ()
  JumpIf { zero: BlockId, nonzero: BlockId } (val) -> ()
  ICompare { comparison: IntComparison } (a, b) -> (result)
  FCompare { comparison: FloatComparison } (a, b) -> (result)

  // --- MEMORY FLOW ---

  /// Set an immediate upto 8 bytes
  Set { value: u64 } () -> (out)

  Return(out) -> ()
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum IntComparison {
  Equal = 0,
  NotEqual = 1,
  LessThan = 2,
  LessThanEqual = 3,
  GreaterThan = 4,
  GreaterThanEqual = 5,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum FloatComparison {
  Ordered = 10,
  Unordered = 11,
  Equal = 12,
  NotEqual = 13,

  OrderedNotEqual = 14,
  UnorderedOrEqual = 15,
  LessThan = 16,
  LessThanOrEqual = 17,
  GreaterThan = 18,
  GreaterThanOrEqual = 19,
  UnorderedOrLessThan = 20,
  UnorderedOrLessThanOrEqual = 21,
  UnorderedOrGreaterThan = 22,
  UnorderedOrGreaterThanOrEqual = 23,
}

#[allow(private_bounds)]
pub trait Register: Internal {}
pub(crate) trait Internal {}

impl Internal for ValueId {}
impl Register for ValueId {}
