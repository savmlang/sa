use std::sync::atomic::{
  AtomicI8, AtomicI16, AtomicI32, AtomicI64, AtomicU8, AtomicU16, AtomicU32, AtomicU64, Ordering,
};

use sart::ctr::VMTaskState;

use crate::{
  acaot::pickle::{
    def::PickleInstruction,
    implementation::WorkingSet,
    reader::{ATOMIC, ATOMICRmwOp, parse_atomic},
  },
  resolve_location_src,
};

// [Sub Opcode (2-bits)] [type (3-bit)] [ordering (3-bits)]
// [offset v0 (i8)] [offset v1 (i8)]
// [offset v2 (i8)] [offset v3 (i8)] [instruction defined (16-bit)]
pub fn call_atomic(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let ts = taskstate;
  match parse_atomic(pickle, &ws.arr) {
    ATOMIC::LOAD {
      typedata,
      ptr_loc,
      ptr_loc_of,
      load_loc,
      load_loc_of,
      ord,
    } => {
      let _d = &[
        call_load::<u64>,
        call_load::<u32>,
        call_load::<u16>,
        call_load::<u8>,
        call_load::<i64>,
        call_load::<i32>,
        call_load::<i16>,
        call_load::<i8>,
      ];

      unsafe {
        (_d.get_unchecked(typedata as usize))(ts, ord, ptr_loc, load_loc, ptr_loc_of, load_loc_of)
      }
    }
    ATOMIC::STORE {
      typedata,
      ptr_loc,
      ptr_loc_of,
      val_stored_loc,
      val_store_of,
      ord,
    } => {
      let _d = &[
        call_store::<u64>,
        call_store::<u32>,
        call_store::<u16>,
        call_store::<u8>,
        call_store::<i64>,
        call_store::<i32>,
        call_store::<i16>,
        call_store::<i8>,
      ];

      unsafe {
        (_d.get_unchecked(typedata as usize))(
          ts,
          ord,
          ptr_loc,
          val_stored_loc,
          ptr_loc_of,
          val_store_of,
        )
      }
    }

    ATOMIC::CAS {
      typedata,
      ptr_loc,
      ptr_loc_of,
      val_stored_loc,
      val_store_of,
      expected_loc,
      expected_of,
      ret_loc,
      ret_of,
      ord_success,
      ord_failure,
    } => {
      let _d = &[
        call_cas::<u64>,
        call_cas::<u32>,
        call_cas::<u16>,
        call_cas::<u8>,
        call_cas::<i64>,
        call_cas::<i32>,
        call_cas::<i16>,
        call_cas::<i8>,
      ];

      unsafe {
        (_d.get_unchecked(typedata as usize))(
          ts,
          ptr_loc,
          ptr_loc_of,
          val_stored_loc,
          val_store_of,
          expected_loc,
          expected_of,
          ret_loc,
          ret_of,
          ord_success,
          ord_failure,
        )
      }
    }

    ATOMIC::RMW {
      typedata,
      ptr_loc,
      ptr_loc_of,
      load_loc,
      load_loc_of,
      rhs_loc,
      rhs_loc_of,
      op,
      ord,
    } => {
      let _d = &[
        call_rmw::<u64>,
        call_rmw::<u32>,
        call_rmw::<u16>,
        call_rmw::<u8>,
        call_rmw::<i64>,
        call_rmw::<i32>,
        call_rmw::<i16>,
        call_rmw::<i8>,
      ];

      unsafe {
        (_d.get_unchecked(typedata as usize))(
          ts,
          ord,
          ptr_loc,
          ptr_loc_of,
          load_loc,
          load_loc_of,
          rhs_loc,
          rhs_loc_of,
          op,
        )
      }
    }
  }
}

trait Atomicable {
  fn a_store(pt: *mut Self, value: Self, order: Ordering);

  fn a_load(pt: *mut Self, out: *mut Self, order: Ordering);

  fn a_cas(
    pt: *mut Self,
    stored: *mut Self,
    expected: *mut Self,
    ret: *mut Self,
    order1: Ordering,
    order2: Ordering,
  );

  fn a_rmw(pt: *mut Self, out: *mut Self, op: *mut Self, rmwop: ATOMICRmwOp, ord: Ordering);
}

macro_rules! atomicable {
  (
    $($a:ty => $b:ty),*
  ) => {
    $(
      impl Atomicable for $a {
        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
          unsafe {
            <$b>::from_ptr(pt).store(value, order);
          }
        }

        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
          unsafe {
            *ret = <$b>::from_ptr(pt).load(order);
          }
        }

        fn a_cas(pt: *mut Self, stored: *mut Self, expected: *mut Self, ret: *mut Self, order1: Ordering, order2: Ordering) {
          unsafe {
            let stored = *stored;
            let expected = *expected;

            let [out, succ] = <$b>::from_ptr(pt).compare_exchange_weak(expected, stored, order1, order2).map_or_else(|e| [e, !0], |x| [x, 0]);
            *ret = out;
            *ret.add(1) = succ;
          }
        }

        fn a_rmw(pt: *mut Self, out: *mut Self, op: *mut Self, rmwop: ATOMICRmwOp, ord: Ordering) {
          unsafe {
            let atomic = <$b>::from_ptr(pt);

            *out = match rmwop {
              ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
              ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
              ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
              ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
              ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
              ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
              ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
              ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
              ATOMICRmwOp::Max => atomic.fetch_max(*op, ord)
            };
          }
        }
      }
    )*
  };
}

atomicable! {
  u64 => AtomicU64,
  u32 => AtomicU32,
  u16 => AtomicU16,
  u8 => AtomicU8,

  i64 => AtomicI64,
  i32 => AtomicI32,
  i16 => AtomicI16,
  i8 => AtomicI8
}

#[allow(unused)]
fn call_store<T: Atomicable + Clone + Copy>(
  taskstate: &mut VMTaskState,
  ord: Ordering,

  o1: u8,
  o2: u8,

  of1: u8,
  of2: u8,
) {
  unsafe {
    let o1 = (std::ptr::read_unaligned(resolve_location_src!(taskstate => o1).add(of1 as _)).pointer
      as *mut T);

    let o2 = (resolve_location_src!(taskstate => o2) as *mut T).offset(of2 as _);

    Atomicable::a_store(o1, *o2, ord)
  };
}

#[allow(unused)]
fn call_load<T: Atomicable + Clone + Copy>(
  taskstate: &mut VMTaskState,
  ord: Ordering,
  o1: u8,
  o2: u8,

  of1: u8,
  of2: u8,
) {
  unsafe {
    let o1 = (std::ptr::read_unaligned(resolve_location_src!(taskstate => o1).add(of1 as _)))
      .pointer as *mut T;
    let o2 = (resolve_location_src!(taskstate => o2) as *mut T).add(of2 as _);

    Atomicable::a_load(o1, o2, ord)
  };
}

#[allow(unused)]
fn call_cas<T: Atomicable + Clone + Copy>(
  taskstate: &mut VMTaskState,
  ptr_loc: u8,
  ptr_loc_of: u8,

  val_stored_loc: u8,
  val_store_of: u8,

  expected_loc: u8,
  expected_of: u8,

  ret_loc: u8,
  ret_of: u8,

  ord: Ordering,
  ord2: Ordering,
) {
  unsafe {
    let o1 =
      (std::ptr::read_unaligned(resolve_location_src!(taskstate => ptr_loc).add(ptr_loc_of as _))
        .pointer as *mut T);

    let o2 = (resolve_location_src!(taskstate => val_stored_loc) as *mut T).add(val_store_of as _);
    let o3 = (resolve_location_src!(taskstate => expected_loc) as *mut T).offset(expected_of as _);
    let o4 = (resolve_location_src!(taskstate => ret_loc) as *mut T).add(ret_of as _);

    Atomicable::a_cas(o1, o2, o3, o4, ord, ord2)
  };
}

#[allow(unused)]
fn call_rmw<T: Atomicable + Clone + Copy>(
  taskstate: &mut VMTaskState,
  ord: Ordering,

  o1: u8,
  of1: u8,

  o2: u8,
  of2: u8,

  o3: u8,
  of3: u8,

  op: ATOMICRmwOp,
) {
  unsafe {
    let ptr = (std::ptr::read_unaligned(resolve_location_src!(taskstate => o1).add(of1 as _)))
      .pointer as *mut T;

    let load_to = (resolve_location_src!(taskstate => o2) as *mut T).add(of2 as _);
    let rhs = (resolve_location_src!(taskstate => o3) as *mut T).add(of3 as _);

    Atomicable::a_rmw(ptr, load_to, rhs, op, ord);
  };
}
