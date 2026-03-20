use std::{intrinsics::*, ptr};

use sart::ctr::VMTaskState;

use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint, resolve_location_src,
};

const TOTAL_ORDERINGS: u8 = 5;

macro_rules! atomicops {
  (
    $(
      { $($typeset:ty),* } $co:ident $f:ident => |$o1:ident, $o2:ident, $o3:ident, $o4:ident, $o5:ident, $o1_raw:ident, $o2_raw:ident, $o3_raw:ident, $o4_raw:ident, $ts:ident| $c:expr
    ),*
  ) => {
    pastey::paste! {
      $(
        $(
          atomicops! {
            @permute
            $f
            $co
            |$o1, $o2, $o3, $o4, $o5, $o1_raw, $o2_raw, $o3_raw, $o4_raw, $ts| { $c }
            $typeset
          }
        )*
      )*

      // SUBOP -> ORDERING -> TYPE

      const _DISPATCHER: [
        [
          // 8 types
          [
            // 5 ordering
            fn(_: i8, _: i8, _: i8, _: i8, _: u16, _: &mut VMTaskState); 5
          ];
          8
        ]; 4
      ] = [
        $(
          atomicops! (
            @ar
            $f
            $($typeset),*
          )
        ),*
      ];
    }
  };

  (
    @ar
    $f:ident
    $($typeset:ty),*
  ) => {
    pastey::paste! {
      [
        $(
          [
            [<atomic_op_ $f _ $typeset _seqcst>],
            [<atomic_op_ $f _ $typeset _relaxed>],
            [<atomic_op_ $f _ $typeset _acquire>],
            [<atomic_op_ $f _ $typeset _release>],
            [<atomic_op_ $f _ $typeset _acqrel>],
          ]
        ),*
      ]
    }
  };

  (
    @gen
    $f:ident
    $co:ident
    $typeset:ty
    |
      $o1:ident, $o2:ident, $o3:ident, $o4:ident, $o5:ident,
      $o1_raw:ident, $o2_raw:ident, $o3_raw:ident, $o4_raw:ident, $ts:ident
    | { $c:expr }
    $o:ident => { $ord:expr }
  ) => {
    pastey::paste! {
      fn [<atomic_op_ $f _ $typeset _ $o>](
        of1: i8,
        of2: i8,
        of3: i8,
        of4: i8,
        instdefined: u16,
        $ts: &mut VMTaskState
      ) {
        let $o1_raw = (instdefined as u8) & 0x0F;
        let $o2_raw = (instdefined >> 4) as u8 & 0x0F;
        let $o3_raw = (instdefined >> 8) as u8 & 0x0F;
        let $o4_raw = (instdefined >> 12) as u8 & 0x0F;

        const $co: AtomicOrdering = $ord;

        let forgiving = std::ptr::null_mut();
        unsafe {
          let $o1 = (ptr::read_unaligned(resolve_location_src!($ts => $o1_raw forgiving)).pointer as *mut $typeset).offset(of1 as _);
          let $o2 = (resolve_location_src!($ts => $o2_raw forgiving) as *mut $typeset).offset(of2 as _);
          let $o3 = (resolve_location_src!($ts => $o3_raw forgiving) as *mut $typeset).offset(of3 as _);
          let $o4 = (resolve_location_src!($ts => $o4_raw forgiving) as *mut $typeset).offset(of4 as _);

          $c
        }
      }
    }
  };

  (
    @permute
    $f:ident
    $co:ident
    |
      $o1:ident, $o2:ident, $o3:ident, $o4:ident, $o5:ident,
      $o1_raw:ident, $o2_raw:ident, $o3_raw:ident, $o4_raw:ident, $ts:ident
    | { $c:expr }
    $typeset:ty
  ) => {
    atomicops! {
      @gen
      $f
      $co
      $typeset
      |$o1, $o2, $o3, $o4, $o5, $o1_raw, $o2_raw, $o3_raw, $o4_raw, $ts| { $c }
      seqcst => { AtomicOrdering::SeqCst }
    }
    atomicops! {
      @gen
      $f
      $co
      $typeset
      |$o1, $o2, $o3, $o4, $o5, $o1_raw, $o2_raw, $o3_raw, $o4_raw, $ts| { $c }
      relaxed => { AtomicOrdering::Relaxed }
    }
    atomicops! {
      @gen
      $f
      $co
      $typeset
      |$o1, $o2, $o3, $o4, $o5, $o1_raw, $o2_raw, $o3_raw, $o4_raw, $ts| { $c }
      acquire => { AtomicOrdering::Acquire }
    }
    atomicops! {
      @gen
      $f
      $co
      $typeset
      |$o1, $o2, $o3, $o4, $o5, $o1_raw, $o2_raw, $o3_raw, $o4_raw, $ts| { $c }
      release => { AtomicOrdering::Release }
    }
    atomicops! {
      @gen
      $f
      $co
      $typeset
      |$o1, $o2, $o3, $o4, $o5, $o1_raw, $o2_raw, $o3_raw, $o4_raw, $ts| { $c }
      acqrel => { AtomicOrdering::AcqRel }
    }
  };
}

trait AtomicOps {
  unsafe fn do_min<const ORD: AtomicOrdering>(p: *mut Self, val: Self) -> Self;
  unsafe fn do_max<const ORD: AtomicOrdering>(p: *mut Self, val: Self) -> Self;

  unsafe fn do_umin<const ORD: AtomicOrdering>(p: *mut Self, val: Self) -> Self;
  unsafe fn do_umax<const ORD: AtomicOrdering>(p: *mut Self, val: Self) -> Self;
}

macro_rules! implatomicop {
  (
    $(
      { $($t:ty),* } => {
        min: $min:ident,
        max: $max:ident
      }
    ),*
  ) => {
    $(
      $(
      impl AtomicOps for $t {
        unsafe fn do_min<const ORD: AtomicOrdering>(p: *mut $t, val: $t) -> $t {
          unsafe { $min::<_, ORD>(p, val) }
        }

        unsafe fn do_umin<const ORD: AtomicOrdering>(p: *mut $t, val: $t) -> $t {
          unsafe { $min::<_, ORD>(p, val) }
        }

        unsafe fn do_max<const ORD: AtomicOrdering>(p: *mut $t, val: $t) -> $t {
          unsafe { $max::<_, ORD>(p, val) }
        }

        unsafe fn do_umax<const ORD: AtomicOrdering>(p: *mut $t, val: $t) -> $t {
          unsafe { $max::<_, ORD>(p, val) }
        }
      }
      )*
    )*
  };
}

implatomicop! {
  { u64, u32, u16, u8 } => {
    min: atomic_umin,
    max: atomic_umax
  },
  { i64, i32, i16, i8 } => {
    min: atomic_min,
    max: atomic_max
  }
}

atomicops! {
  { u64, u32, u16, u8, i64, i32, i16, i8 } ORD cas => |p1, x, e, ret, instdef, o1_raw, o2_raw, o3_raw, o4_raw, ts| {
    let (out, succ) = match ts.r8.u8 {
      1 => atomic_cxchgweak::<_, ORD, { AtomicOrdering::Relaxed }>(p1, ptr::read_unaligned(e), ptr::read_unaligned(x)),
      2 => atomic_cxchgweak::<_, ORD, { AtomicOrdering::Acquire }>(p1, ptr::read_unaligned(e), ptr::read_unaligned(x)),
      3 => atomic_cxchgweak::<_, ORD, { AtomicOrdering::Release }>(p1, ptr::read_unaligned(e), ptr::read_unaligned(x)),
      4 => atomic_cxchgweak::<_, ORD, { AtomicOrdering::AcqRel }>(p1, ptr::read_unaligned(e), ptr::read_unaligned(x)),
      _ => atomic_cxchgweak::<_, ORD, { AtomicOrdering::SeqCst }>(p1, ptr::read_unaligned(e), ptr::read_unaligned(x))
    };

    ptr::write_unaligned(ret, out);
    ptr::write_unaligned(ret.add(1), if succ { !0 } else { 0 });
  },
  { u64, u32, u16, u8, i64, i32, i16, i8 } ORD load => |p1, ret, _o3, _o4, instdef, _r1_raw, _2raw, _3raw, _4raw, _ts| {
    ptr::write_unaligned(ret, atomic_load::<_, ORD>(p1));
  },
  { u64, u32, u16, u8, i64, i32, i16, i8 } ORD rmw => |p1, operand, ret, _o4, instdef, _r1_raw, _2raw, _3raw, rmw_op, _ts| {
    let o = ptr::read_unaligned(operand);
    let rmw = match rmw_op {
      // Add
      0 =>  atomic_xadd::<_, _, ORD>(p1, o),
      // Sub
      1 =>  atomic_xsub::<_, _, ORD>(p1, o),
      // And
      2 =>  atomic_and::<_, _, ORD>(p1, o),
      // Nand
      3 =>  atomic_nand::<_, _, ORD>(p1, o),
      // Or
      4 =>  atomic_or::<_, _, ORD>(p1, o),
      // Xor
      5 =>  atomic_xor::<_, _, ORD>(p1, o),
      // Xchg
      6 =>  atomic_xchg::<_, ORD>(p1, o),
      // Umin
      7 =>  AtomicOps::do_umin::<ORD>(p1, o),
      // Umax
      8 =>  AtomicOps::do_umax::<ORD>(p1, o),
      // Smin
      9 =>  AtomicOps::do_min::<ORD>(p1, o),
      // Smax
      10 =>  AtomicOps::do_max::<ORD>(p1, o),
      e => panic!("Unknown Atomic Op: {e}")
    };

    ptr::write_unaligned(ret, rmw);
  },
  { u64, u32, u16, u8, i64, i32, i16, i8 } ORD store => |p1, x, _o3, _o4, instdef, _r1_raw, _2raw, _3raw, _4raw, _ts| {
    atomic_store::<_, ORD>(p1, ptr::read_unaligned(x));
  }
}

// [Sub Opcode (2-bits)] [type (3-bit)] [ordering (3-bits)]
// [offset v0 (i8)] [offset v1 (i8)]
// [offset v2 (i8)] [offset v3 (i8)] [instruction defined (16-bit)]
pub fn call_atomic(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let flags = pickle.u1;

  let ordering = flags & 0x7;

  debug_assert!(ordering < TOTAL_ORDERINGS);

  let ty = (flags >> 3) & 0x7;
  let subop = flags >> 6;

  let offset1 = pickle.u2.cast_signed();
  let offset2 = pickle.u3.cast_signed();
  let offset3 = arrcastint!(ws, start = 0, stop = 1, i8);
  let offset4 = arrcastint!(ws, start = 1, stop = 2, i8);

  // Now, lets resolve to 4x pointers
  // SAFETY: A zeroed pattern is effective a no-issue since
  // it creates a pointer that just isn't used
  let instdefined = arrcastint!(ws, start = 2, stop = 4, u16);

  unsafe {
    (_DISPATCHER
      .get_unchecked(subop as usize)
      .get_unchecked(ty as usize)
      .get_unchecked(ordering as usize))(
      offset1, offset2, offset3, offset4, instdefined, taskstate
    )
  }
}
