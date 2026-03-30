use std::sync::atomic::{
  AtomicI8, AtomicI16, AtomicI32, AtomicI64, AtomicU8, AtomicU16, AtomicU32, AtomicU64, Ordering,
};

use sart::ctr::VMTaskState;

use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};

const TOTAL_ORDERINGS: u8 = 5;

// [Sub Opcode (2-bits)] [type (3-bit)] [ordering (3-bits)]
// [offset v0 (i8)] [offset v1 (i8)]
// [offset v2 (i8)] [offset v3 (i8)] [instruction defined (16-bit)]
pub fn call_atomic(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let flags = pickle.u1;

  let ordering = flags & 0x7;

  debug_assert!(ordering < TOTAL_ORDERINGS);

  let ty = (flags >> 3) & 0x7;
  let subop = flags >> 6;

  let of1 = pickle.u2.cast_signed();
  let of2 = pickle.u3.cast_signed();
  let of3 = arrcastint!(ws, start = 0, stop = 1, i8);
  let of4 = arrcastint!(ws, start = 1, stop = 2, i8);

  // Now, lets resolve to 4x pointers
  // SAFETY: A zeroed pattern is effective a no-issue since
  // it creates a pointer that just isn't used
  let instdefined = arrcastint!(ws, start = 2, stop = 4, u16);

  {}

  // seqcst, relaxed, acq, rel, acqrel
  let ord = match ordering {
    0 => Ordering::SeqCst,
    1 => Ordering::Relaxed,
    2 => Ordering::Acquire,
    3 => Ordering::Release,
    4 => Ordering::AcqRel,
    e => panic!("Unknown {e}"),
  };

  let ts = taskstate;

  match subop {
    0 => {
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

      unsafe { (_d.get_unchecked(ty as usize))(ts, ord, instdefined, of1, of2, of3, of4) }
    }
    1 => {
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

      unsafe { (_d.get_unchecked(ty as usize))(ts, ord, instdefined, of1, of2, of3, of4) }
    }
    2 => {
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

      unsafe { (_d.get_unchecked(ty as usize))(ts, ord, instdefined, of1, of2, of3, of4) }
    }
    3 => {
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

      unsafe { (_d.get_unchecked(ty as usize))(ts, ord, instdefined, of1, of2, of3, of4) }
    }
    _ => unreachable!(),
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

            *ret = <$b>::from_ptr(pt).compare_exchange_weak(expected, stored, order1, order2).map_or_else(|e| e, |x| x);
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
  instdefined: u16,
  of1: i8,
  of2: i8,
  of3: i8,
  of4: i8,
) {
  unsafe {
    let o1_raw = (instdefined as u8) & 0x0F;
    let o2_raw = (instdefined >> 4) as u8 & 0x0F;
    let o3_raw = (instdefined >> 8) as u8 & 0x0F;
    let o4_raw = (instdefined >> 12) as u8 & 0x0F;

    let forgiving = std::ptr::null_mut();

    let o1 =
      (std::ptr::read_unaligned(resolve_location_src!(taskstate => o1_raw forgiving)).pointer
        as *mut T)
        .offset(of1 as _);
    let o2 = (resolve_location_src!(taskstate => o2_raw forgiving) as *mut T).offset(of2 as _);
    let o3 = (resolve_location_src!(taskstate => o3_raw forgiving) as *mut T).offset(of3 as _);
    let o4 = (resolve_location_src!(taskstate => o4_raw forgiving) as *mut T).offset(of4 as _);

    Atomicable::a_store(o1, *o2, ord)
  };
}

#[allow(unused)]
fn call_load<T: Atomicable + Clone + Copy>(
  taskstate: &mut VMTaskState,
  ord: Ordering,
  instdefined: u16,
  of1: i8,
  of2: i8,
  of3: i8,
  of4: i8,
) {
  unsafe {
    let o1_raw = (instdefined as u8) & 0x0F;
    let o2_raw = (instdefined >> 4) as u8 & 0x0F;
    let o3_raw = (instdefined >> 8) as u8 & 0x0F;
    let o4_raw = (instdefined >> 12) as u8 & 0x0F;

    let forgiving = std::ptr::null_mut();

    let o1 =
      (std::ptr::read_unaligned(resolve_location_src!(taskstate => o1_raw forgiving)).pointer
        as *mut T)
        .offset(of1 as _);
    let o2 = (resolve_location_src!(taskstate => o2_raw forgiving) as *mut T).offset(of2 as _);
    let o3 = (resolve_location_src!(taskstate => o3_raw forgiving) as *mut T).offset(of3 as _);
    let o4 = (resolve_location_src!(taskstate => o4_raw forgiving) as *mut T).offset(of4 as _);

    Atomicable::a_load(o1, o2, ord)
  };
}

#[allow(unused)]
fn call_cas<T: Atomicable + Clone + Copy>(
  taskstate: &mut VMTaskState,
  ord: Ordering,
  instdefined: u16,
  of1: i8,
  of2: i8,
  of3: i8,
  of4: i8,
) {
  unsafe {
    let o1_raw = (instdefined as u8) & 0x0F;
    let o2_raw = (instdefined >> 4) as u8 & 0x0F;
    let o3_raw = (instdefined >> 8) as u8 & 0x0F;
    let o4_raw = (instdefined >> 12) as u8 & 0x0F;

    let forgiving = std::ptr::null_mut();

    let o1 =
      (std::ptr::read_unaligned(resolve_location_src!(taskstate => o1_raw forgiving)).pointer
        as *mut T)
        .offset(of1 as _);
    let o2 = (resolve_location_src!(taskstate => o2_raw forgiving) as *mut T).offset(of2 as _);
    let o3 = (resolve_location_src!(taskstate => o3_raw forgiving) as *mut T).offset(of3 as _);
    let o4 = (resolve_location_src!(taskstate => o4_raw forgiving) as *mut T).offset(of4 as _);

    // Move that in CAS
    let ord2 = unsafe {
      match &ord {
        Ordering::SeqCst => match taskstate.r8.u8 {
          1 => Ordering::Relaxed,
          2 => Ordering::Acquire,
          0 => Ordering::SeqCst,
          e => panic!("Illegal ordering for SeqCst : {e}"),
        },
        Ordering::Relaxed | Ordering::Release => match taskstate.r8.u8 {
          1 => Ordering::Relaxed,
          e => panic!("Illegal ordering for Relaxed (or Release) : {e}"),
        },
        Ordering::Acquire | Ordering::AcqRel => match taskstate.r8.u8 {
          2 => Ordering::Acquire,
          1 => Ordering::Relaxed,
          e => panic!("Illegal ordering for Acq(or AcqRel) : {e}"),
        },
        e => panic!("Illegal ordering for CAS : {e:?}"),
      }
    };

    Atomicable::a_cas(o1, o2, o3, o4, ord, ord2)
  };
}

#[allow(unused)]
fn call_rmw<T: Atomicable + Clone + Copy>(
  taskstate: &mut VMTaskState,
  ord: Ordering,
  instdefined: u16,
  of1: i8,
  of2: i8,
  of3: i8,
  of4: i8,
) {
  unsafe {
    let o1_raw = (instdefined as u8) & 0x0F;
    let o2_raw = (instdefined >> 4) as u8 & 0x0F;
    let o3_raw = (instdefined >> 8) as u8 & 0x0F;

    // pub enum AtomicRmwOp {
    //   Add,
    //   Sub,
    //   And,
    //   Nand,
    //   Or,
    //   Xor,
    //   Xchg,
    //   min,
    //   max,
    // }
    let op = (instdefined >> 12) as u8 & 0x0F;

    let forgiving = std::ptr::null_mut();

    let o1 =
      (std::ptr::read_unaligned(resolve_location_src!(taskstate => o1_raw forgiving)).pointer
        as *mut T)
        .offset(of1 as _);
    let o2 = (resolve_location_src!(taskstate => o2_raw forgiving) as *mut T).offset(of2 as _);
    let o3 = (resolve_location_src!(taskstate => o3_raw forgiving) as *mut T).offset(of3 as _);
  };
}
