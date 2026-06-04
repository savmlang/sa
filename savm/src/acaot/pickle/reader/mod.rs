use std::sync::atomic::Ordering;

use crate::acaot::pickle::def::PickleInstruction;

pub mod au;
pub mod cast;
pub mod corevm;
pub mod fp;
pub mod spawn;
pub mod vfop;
pub mod vminimax;
pub mod vsh;

#[macro_export]
macro_rules! wspickle {
  ($ws:ident, start = $start:expr, stop = $stop:expr, $t:ty) => {
    <$t>::from_ne_bytes($ws[$start..$stop].try_into().unwrap())
  };
}

#[macro_export]
macro_rules! constdef {
  (
    $t:ty

    {
      $(
        $name:ident $val:expr
      ),*
    }
  ) => {
    $(
      pub const $name: $t = $val;
    )*
  };
}

constdef! {
  u8

  {
    ATOMIC_CAS 0,
    ATOMIC_LOAD 1,
    ATOMIC_RMW 2,
    ATOMIC_STORE 3
  }
}

pub enum ATOMIC {
  CAS {
    typedata: u8,

    ptr_loc: u8,
    ptr_loc_of: u8,

    val_stored_loc: u8,
    val_store_of: u8,

    expected_loc: u8,
    expected_of: u8,

    ret_loc: u8,
    ret_of: u8,

    ord_success: Ordering,
    ord_failure: Ordering,
  },
  RMW {
    typedata: u8,

    ptr_loc: u8,
    ptr_loc_of: u8,

    load_loc: u8,
    load_loc_of: u8,

    rhs_loc: u8,
    rhs_loc_of: u8,

    op: ATOMICRmwOp,

    ord: Ordering,
  },
  STORE {
    typedata: u8,

    ptr_loc: u8,
    ptr_loc_of: u8,

    val_stored_loc: u8,
    val_store_of: u8,

    ord: Ordering,
  },
  LOAD {
    typedata: u8,

    ptr_loc: u8,
    ptr_loc_of: u8,

    load_loc: u8,
    load_loc_of: u8,

    ord: Ordering,
  },
}

pub enum ATOMICRmwOp {
  Add,
  Sub,
  And,
  Nand,
  Or,
  Xor,
  Xchg,
  Min,
  Max,
}

pub fn parse_atomic(pickle: &PickleInstruction, ws: &[u8]) -> ATOMIC {
  let flags = pickle.u1;

  let ordering = flags & 0x7;
  let ordering2 = wspickle!(ws, start = 0, stop = 1, u8);

  let ty = (flags >> 3) & 0x7;
  let subop = flags >> 6;

  let of_v0 = pickle.u2;
  let of_v1 = pickle.u3;
  let of_v2 = wspickle!(ws, start = 1, stop = 2, u8);
  let of_v3 = wspickle!(ws, start = 2, stop = 3, u8);

  let instdefined = wspickle!(ws, start = 3, stop = 5, u16);

  let v0 = (instdefined as u8) & 0x0F;
  let v1 = (instdefined >> 4) as u8 & 0x0F;
  let v2 = (instdefined >> 8) as u8 & 0x0F;
  let v3 = (instdefined >> 12) as u8 & 0x0F;

  let ord = match ordering {
    0 => Ordering::SeqCst,
    1 => Ordering::Relaxed,
    2 => Ordering::Acquire,
    3 => Ordering::Release,
    4 => Ordering::AcqRel,
    e => panic!("Unknown {e}"),
  };
  let ord2 = match ordering2 {
    0 => Ordering::SeqCst,
    1 => Ordering::Relaxed,
    2 => Ordering::Acquire,
    3 => Ordering::Release,
    4 => Ordering::AcqRel,
    e => panic!("Unknown {e}"),
  };

  match subop {
    ATOMIC_CAS => ATOMIC::CAS {
      typedata: ty,

      ptr_loc: v0,
      ptr_loc_of: of_v0,

      val_stored_loc: v1,
      val_store_of: of_v1,

      expected_loc: v2,
      expected_of: of_v2,

      ret_loc: v3,
      ret_of: of_v3,

      ord_success: ord,
      ord_failure: ord2,
    },
    ATOMIC_LOAD => ATOMIC::LOAD {
      typedata: ty,

      ptr_loc: v0,
      ptr_loc_of: of_v0,

      load_loc: v1,
      load_loc_of: of_v1,

      ord,
    },
    ATOMIC_RMW => ATOMIC::RMW {
      typedata: ty,
      ptr_loc: v0,
      ptr_loc_of: of_v0,
      load_loc: v1,
      load_loc_of: of_v1,
      rhs_loc: v2,
      rhs_loc_of: of_v2,
      op: match v3 {
        0 => ATOMICRmwOp::Add,
        1 => ATOMICRmwOp::Sub,
        2 => ATOMICRmwOp::And,
        3 => ATOMICRmwOp::Nand,
        4 => ATOMICRmwOp::Or,
        5 => ATOMICRmwOp::Xor,
        6 => ATOMICRmwOp::Xchg,
        7 => ATOMICRmwOp::Min,
        8 => ATOMICRmwOp::Max,
        _ => unreachable!(),
      },
      ord,
    },
    ATOMIC_STORE => ATOMIC::STORE {
      typedata: ty,

      ptr_loc: v0,
      ptr_loc_of: of_v0,

      val_stored_loc: v1,
      val_store_of: of_v1,

      ord,
    },
    _ => unimplemented!(),
  }
}
