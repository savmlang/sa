use crate::mir::{block::BlockId, function::ssa::ValueId, value::BaseType};
use const_str::convert_ascii_case;
use std::fmt::Formatter;

pub mod loc;

macro_rules! instloader {
  (
    $(
      $(#[$meta:meta])*
      $name:ident $({ $( $imm:ident: $ty:ty ),* })? ($($arg:ident),*) -> ($($out:ident),*)
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
      pub fn format(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
          $(
            Self::$name {
              $(
                $arg,
              )*
              $(
                $out,
              )*
              $($(
                $imm,
              )*)?
            } => {
              let name = convert_ascii_case!(lower, stringify!($name));

              // Show output if available
              #[allow(unused)]
              let mut outputs = false;
              $(
                $out.f(f)?;
                write!(f, " ")?;
                outputs = true;
              )*

              if outputs {
                write!(f, "= ")?;
              }

              write!(f, "{name}")?;

              // Pass Args First
              $(
                write!(f, " ")?;
                $arg.f(f)?;
              )*

              // Pass Immediates
              $(
                $(
                  write!(f, " ")?;
                  $imm.f(f)?;
                )*
              )?
            }
          ),*
        }
        write!(f, "")
      }

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

  Jump { block: BlockId, args: Box<[ValueId]> } () -> ()
  JumpIf { zero: BlockId, nonzero: BlockId, args: Box<[ValueId]> } (val) -> ()
  ICompare { comparison: IntComparison } (a, b) -> (result)
  FCompare { comparison: FloatComparison } (a, b) -> (result)

  // --- MEMORY FLOW ---

  /// Set an immediate upto 8 bytes
  Set { typedata: BaseType, value: u64 } () -> (out)

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
impl IntComparison {
  pub(crate) fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Equal => "eq",
        Self::NotEqual => "ne",
        Self::LessThan => "lt",
        Self::LessThanEqual => "le",
        Self::GreaterThan => "gt",
        Self::GreaterThanEqual => "ge",
      }
    )
  }
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

impl FloatComparison {
  pub(crate) fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Ordered => "ord",
        Self::Unordered => "nord",
        Self::Equal => "eq",
        Self::NotEqual => "ne",

        Self::OrderedNotEqual => "one",
        Self::UnorderedOrEqual => "ue",
        Self::LessThan => "lt",
        Self::LessThanOrEqual => "le",
        Self::GreaterThan => "gt",
        Self::GreaterThanOrEqual => "ge",
        Self::UnorderedOrLessThan => "ult",
        Self::UnorderedOrLessThanOrEqual => "ule",
        Self::UnorderedOrGreaterThan => "ugt",
        Self::UnorderedOrGreaterThanOrEqual => "uge",
      }
    )
  }
}

#[allow(private_bounds)]
pub trait Register: Internal {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
}
pub(crate) trait Internal {}

impl Internal for ValueId {}
impl Register for ValueId {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "v{}", self.0)
  }
}

trait AHQF {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
}

impl AHQF for u64 {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{self}")
  }
}

impl AHQF for BaseType {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    self.format(f)
  }
}

impl AHQF for &Box<[ValueId]> {
  fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "(")?;

    for item in self.iter() {
      write!(f, " ")?;
      item.f(f)?;
    }

    write!(f, " )")
  }
}
