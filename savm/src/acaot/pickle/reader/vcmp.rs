#[cfg(feature = "cranelift")]
use cranelift::codegen::ir::condcodes::{FloatCC, IntCC};

use crate::{acaot::pickle::def::PickleInstruction, wspickle};

pub enum CMPOp {
  FloatOp(FloatOP),
  IntOp(IntOP),
}

impl CMPOp {
  pub fn to_classical(self) -> u8 {
    match self {
      Self::IntOp(i) => match i {
        IntOP::Equal => 0,
        IntOP::NotEqual => 1,

        // LT
        IntOP::SignedLessThan => 2,
        IntOP::UnsignedLessThan => 3,
        IntOP::SignedLessThanOrEqual => 4,
        IntOP::UnsignedLessThanOrEqual => 5,

        // GT
        IntOP::SignedGreaterThan => 6,
        IntOP::UnsignedGreaterThan => 7,
        IntOP::SignedGreaterThanOrEqual => 8,
        IntOP::UnsignedGreaterThanOrEqual => 9,
      },
      Self::FloatOp(f) => match f {
        FloatOP::Ordered => 10,
        FloatOP::Unordered => 11,
        FloatOP::Equal => 12,
        FloatOP::NotEqual => 13,
        FloatOP::OrderedNotEqual => 14,
        FloatOP::UnorderedOrEqual => 15,
        FloatOP::LessThan => 16,
        FloatOP::LessThanOrEqual => 17,
        FloatOP::GreaterThan => 18,
        FloatOP::GreaterThanOrEqual => 19,
        FloatOP::UnorderedOrLessThan => 20,
        FloatOP::UnorderedOrLessThanOrEqual => 21,
        FloatOP::UnorderedOrGreaterThan => 22,
        FloatOP::UnorderedOrGreaterThanOrEqual => 23,
      },
    }
  }
}

pub enum IntOP {
  Equal,
  NotEqual,

  // LT
  SignedLessThan,
  UnsignedLessThan,
  SignedLessThanOrEqual,
  UnsignedLessThanOrEqual,

  // GT
  SignedGreaterThan,
  UnsignedGreaterThan,
  SignedGreaterThanOrEqual,
  UnsignedGreaterThanOrEqual,
}

#[cfg(feature = "cranelift")]
impl IntOP {
  pub fn to_clir(&self) -> IntCC {
    match &self {
      Self::Equal => IntCC::Equal,
      Self::NotEqual => IntCC::NotEqual,

      // LT
      Self::SignedLessThan => IntCC::SignedLessThan,
      Self::UnsignedLessThan => IntCC::UnsignedLessThan,
      Self::SignedLessThanOrEqual => IntCC::SignedLessThanOrEqual,
      Self::UnsignedLessThanOrEqual => IntCC::UnsignedLessThanOrEqual,

      // GT
      Self::SignedGreaterThan => IntCC::SignedGreaterThan,
      Self::UnsignedGreaterThan => IntCC::UnsignedGreaterThan,
      Self::SignedGreaterThanOrEqual => IntCC::SignedGreaterThanOrEqual,
      Self::UnsignedGreaterThanOrEqual => IntCC::UnsignedGreaterThanOrEqual,
    }
  }
}

pub enum FloatOP {
  Ordered,
  Unordered,
  Equal,
  NotEqual,
  OrderedNotEqual,
  UnorderedOrEqual,
  LessThan,
  LessThanOrEqual,
  GreaterThan,
  GreaterThanOrEqual,
  UnorderedOrLessThan,
  UnorderedOrLessThanOrEqual,
  UnorderedOrGreaterThan,
  UnorderedOrGreaterThanOrEqual,
}

#[cfg(feature = "cranelift")]
impl FloatOP {
  pub fn to_clir(&self) -> FloatCC {
    match self {
      Self::Ordered => FloatCC::Ordered,
      Self::Unordered => FloatCC::Unordered,
      Self::Equal => FloatCC::Equal,
      Self::NotEqual => FloatCC::NotEqual,
      Self::OrderedNotEqual => FloatCC::OrderedNotEqual,
      Self::UnorderedOrEqual => FloatCC::UnorderedOrEqual,
      Self::LessThan => FloatCC::LessThan,
      Self::LessThanOrEqual => FloatCC::LessThanOrEqual,
      Self::GreaterThan => FloatCC::GreaterThan,
      Self::GreaterThanOrEqual => FloatCC::GreaterThanOrEqual,
      Self::UnorderedOrLessThan => FloatCC::UnorderedOrLessThan,
      Self::UnorderedOrLessThanOrEqual => FloatCC::UnorderedOrLessThanOrEqual,
      Self::UnorderedOrGreaterThan => FloatCC::UnorderedOrGreaterThan,
      Self::UnorderedOrGreaterThanOrEqual => FloatCC::UnorderedOrGreaterThanOrEqual,
    }
  }
}

pub struct VCMP {
  pub datawdt: u8,
  pub cmpop: CMPOp,

  pub count: u32,

  pub src1: u8,
  pub src2: u8,
  pub tgt: u8,

  pub of_src1: i32,
  pub of_src2: i32,
  pub of_tgt: i32,
}

pub fn parse_vcmp(pickle: &PickleInstruction, meta: &[u8]) -> VCMP {
  let op = pickle.u1;
  let datawdt = pickle.u2;

  let cmpop = match op {
    0..=9 => CMPOp::IntOp(match op {
      0 => IntOP::Equal,
      1 => IntOP::NotEqual,

      // LT
      2 => IntOP::SignedLessThan,
      3 => IntOP::UnsignedLessThan,
      4 => IntOP::SignedLessThanOrEqual,
      5 => IntOP::UnsignedLessThanOrEqual,

      // GT
      6 => IntOP::SignedGreaterThan,
      7 => IntOP::UnsignedGreaterThan,
      8 => IntOP::SignedGreaterThanOrEqual,
      9 => IntOP::UnsignedGreaterThanOrEqual,
      _ => unreachable!(),
    }),
    10.. => CMPOp::FloatOp(match op {
      10 => FloatOP::Ordered,
      11 => FloatOP::Unordered,
      12 => FloatOP::Equal,
      13 => FloatOP::NotEqual,
      14 => FloatOP::OrderedNotEqual,
      15 => FloatOP::UnorderedOrEqual,
      16 => FloatOP::LessThan,
      17 => FloatOP::LessThanOrEqual,
      18 => FloatOP::GreaterThan,
      19 => FloatOP::GreaterThanOrEqual,
      20 => FloatOP::UnorderedOrLessThan,
      21 => FloatOP::UnorderedOrLessThanOrEqual,
      22 => FloatOP::UnorderedOrGreaterThan,
      23 => FloatOP::UnorderedOrGreaterThanOrEqual,
      _ => unreachable!(),
    }),
  };

  let srcflags = wspickle!(meta, start = 0, stop = 2, u16);

  let count = wspickle!(meta, start = 2, stop = 6, u32);
  let src1 = {
    let src = (srcflags >> 12) as u8 & 0xF;

    src
  };
  let of_src1 = wspickle!(meta, start = 6, stop = 10, i32);

  let src2 = {
    let src = (srcflags >> 8) as u8 & 0xF;

    src
  };
  let of_src2 = wspickle!(meta, start = 10, stop = 14, i32);

  let tgt = {
    let src = (srcflags >> 4) as u8 & 0xF;

    src
  };
  let of_tgt = wspickle!(meta, start = 14, stop = 18, i32);

  VCMP {
    datawdt,
    cmpop,
    count,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  }
}
