//! Arithmatic-Logic-Memory Unit

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{
      COUNTSPILLER_USES_SIMD, StoreResolver, TypeOrWidth, resolve_location_src_load,
      resolve_location_src_store, resolve_reg,
      vector::{abstract_extractlane, abstract_insertlane},
    },
  },
  pickle::{
    def::PickleInstruction,
    reader::{
      Immediate, REG,
      au::{ARITH, DIVLIKE, parse_arith, parse_divlike},
      parse_reg,
    },
  },
};
use cranelift::{
  codegen::ir::{
    Endianness,
    types::{I8, I16, I32, I64},
  },
  prelude::{MemFlagsData as MemFlags, *},
};

mod atomic;
mod fp;
mod libcall;
mod mark;
mod spawn;
mod vbit;
mod vcmp;
mod vdataop;
mod vmcore;
mod vscncop;

pub use atomic::*;
pub use fp::*;
pub use libcall::*;
pub use mark::*;
pub use spawn::*;
pub use vbit::*;
pub use vcmp::*;
pub use vdataop::*;
pub use vmcore::*;
pub use vscncop::*;

#[macro_export]
macro_rules! readws {
  ($meta:expr, start = $start:expr, stop = $stop:expr, $t:ty) => {
    <$t>::from_ne_bytes($meta.ws[$start..$stop].try_into().unwrap())
  };
}

pub fn handle_reg(
  pickle: &PickleInstruction,
  meta: &mut CompilerMeta,
  builder: &mut FunctionBuilder,
) {
  let REG {
    src,
    offset,
    width,
    immediate,
  } = parse_reg(pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Width(width);
  let mut store = resolve_location_src_store(builder, meta, typ, src, None, offset as _, 1);

  let val = match immediate {
    Immediate::U64(x) => builder.ins().iconst(I64, x.cast_signed()),
    Immediate::U32(x) => builder.ins().iconst(I32, x as i64),
    Immediate::U16(x) => builder.ins().iconst(I16, x as i64),
    Immediate::U8(x) => builder.ins().iconst(I8, x as i64),
  };
  store.store(builder, 0, val);

  store.synchronize(builder, meta);
}

pub fn divlike(
  pickle: &PickleInstruction,
  meta: &mut CompilerMeta,
  builder: &mut FunctionBuilder,
) -> (TypeOrWidth, Value, Value, StoreResolver) {
  let DIVLIKE {
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_divlike(pickle, &meta.ws);

  let typ = TypeOrWidth::Type(datatype);

  let src1 = {
    let [src] = *resolve_location_src_load(builder, meta, typ, src1, None, of_src1, 1) else {
      unreachable!()
    };
    src
  };

  let src2 = {
    let [src] = *resolve_location_src_load(builder, meta, typ, src2 as u8, None, of_src2, 1) else {
      unreachable!()
    };
    src
  };

  let target = { resolve_location_src_store(builder, meta, typ, tgt, None, of_tgt, 1) };

  (typ, src1, src2, target)
}

pub fn hwnd_div(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, pickle: PickleInstruction) {
  let (typ, src1, src2, mut target) = divlike(&pickle, meta, builder);

  let tgt = if typ.clif_mapping().signed {
    builder.ins().sdiv(src1, src2)
  } else {
    builder.ins().udiv(src1, src2)
  };

  target.store(builder, 0, tgt);
  target.synchronize(builder, meta);
}

pub fn hwnd_rem(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, pickle: PickleInstruction) {
  let (typ, src1, src2, mut target) = divlike(&pickle, meta, builder);

  let tgt = if typ.clif_mapping().signed {
    builder.ins().srem(src1, src2)
  } else {
    builder.ins().urem(src1, src2)
  };

  target.store(builder, 0, tgt);
  target.synchronize(builder, meta);
}

#[inline(always)]
fn arithprelude(
  meta: &mut CompilerMeta,
  builder: &mut FunctionBuilder,
) -> (
  u16,
  u32,
  TypeOrWidth,
  Box<[Value]>,
  Box<[Value]>,
  (StoreResolver, u8, i32),
) {
  let ARITH {
    datatype,
    count,
    instdefined,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_arith(&meta.ws);

  let typ = TypeOrWidth::Type(datatype);
  let src1 =
    { resolve_location_src_load(builder, meta, typ, src1 as u8, None, of_src1 as _, count) };
  let src2 =
    { resolve_location_src_load(builder, meta, typ, src2 as u8, None, of_src2 as _, count) };
  let target = {
    (
      resolve_location_src_store(builder, meta, typ, tgt as u8, None, of_tgt as _, count),
      tgt,
      of_tgt,
    )
  };

  (instdefined, count, typ, src1, src2, target)
}

pub fn hwnd_vadd(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, _: PickleInstruction) {
  let (instdefined, count, typ, src1, src2, (mut target, _, _)) = arithprelude(meta, builder);

  // [<Carry/Sigflow bit>] [<saturation bit>] [Padding (14bits)] (16b)
  let carry = (instdefined >> 15) == 1; // gets the last bit
  let saturate = (instdefined >> 14 & 0b01) == 1; // gets the saturation bit

  debug_assert!(!(carry && saturate));
  debug_assert!(count != 0);
  debug_assert!(!(carry && count != 1));

  let clif = typ.clif_mapping();

  if carry {
    let r5 = resolve_reg(builder, meta, 4);

    let r5_val = builder.use_var(r5);

    let old = builder.ins().bitcast(
      clif.xreg,
      MemFlags::new().with_endianness(Endianness::Little),
      r5_val,
    );

    let r5_carry_bit = if clif.width != 8 {
      abstract_extractlane(builder, meta, old, 0)
    } else {
      old
    };

    let (&[src1], &[src2]) = (src1.as_ref(), src2.as_ref()) else {
      unimplemented!()
    };

    let (ans, of) = if clif.signed {
      builder.ins().sadd_overflow_cin(src1, src2, r5_carry_bit)
    } else {
      builder.ins().uadd_overflow_cin(src1, src2, r5_carry_bit)
    };
    target.store(builder, 0, ans);

    if clif.width != 8 {
      let extend = abstract_insertlane(builder, meta, r5_val, of, 0);
      builder.def_var(r5, extend);
    } else {
      builder.def_var(r5, of);
    };
  } else if saturate {
    src1
      .into_iter()
      .zip(src2.into_iter())
      .enumerate()
      .for_each(|(idx, (src1, src2))| {
        let val = if clif.signed {
          builder.ins().sadd_sat(src1, src2)
        } else {
          builder.ins().uadd_sat(src1, src2)
        };
        target.store(builder, idx, val);
      });
  } else {
    src1
      .into_iter()
      .zip(src2.into_iter())
      .enumerate()
      .for_each(|(idx, (src1, src2))| {
        let val = builder.ins().iadd(src1, src2);
        target.store(builder, idx, val);
      });
  }

  target.synchronize(builder, meta);
}

pub fn hwnd_vsub(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, _: PickleInstruction) {
  let (instdefined, count, typ, src1, src2, (mut target, _, _)) = arithprelude(meta, builder);

  // [<borrow bit>] [<saturation bit>] [Padding (14bits)] (16b)
  let borrow = (instdefined >> 15) == 1; // gets the last bit
  let saturate = (instdefined >> 14 & 0b01) == 1; // gets the saturation bit

  debug_assert!(!(borrow && saturate));
  debug_assert!(count != 0);
  debug_assert!(!(borrow && count != 1));

  let clif = typ.clif_mapping();

  if borrow {
    let r5 = resolve_reg(builder, meta, 4);

    let r5_val = builder.use_var(r5);

    let old = builder.ins().bitcast(
      clif.xreg,
      MemFlags::new().with_endianness(Endianness::Little),
      r5_val,
    );

    let r5_carry_bit = if clif.width != 8 {
      abstract_extractlane(builder, meta, old, 0)
    } else {
      old
    };

    let (&[src1], &[src2]) = (src1.as_ref(), src2.as_ref()) else {
      unimplemented!()
    };

    let (ans, of) = if clif.signed {
      builder.ins().ssub_overflow_bin(src1, src2, r5_carry_bit)
    } else {
      builder.ins().usub_overflow_bin(src1, src2, r5_carry_bit)
    };
    target.store(builder, 0, ans);

    if clif.width != 8 {
      let extend = abstract_insertlane(builder, meta, r5_val, of, 0);
      builder.def_var(r5, extend);
    } else {
      builder.def_var(r5, of);
    };
  } else if saturate {
    src1
      .into_iter()
      .zip(src2.into_iter())
      .enumerate()
      .for_each(|(idx, (src1, src2))| {
        let val = if clif.signed {
          builder.ins().ssub_sat(src1, src2)
        } else {
          builder.ins().usub_sat(src1, src2)
        };
        target.store(builder, idx, val);
      });
  } else {
    src1
      .into_iter()
      .zip(src2.into_iter())
      .enumerate()
      .for_each(|(idx, (src1, src2))| {
        let val = builder.ins().isub(src1, src2);
        target.store(builder, idx, val);
      });
  }

  target.synchronize(builder, meta);
}

// VMUL_WIDE depends on unguaranteed behaviour of target resolver
// Hence it'll blow off once countsplitter.rs starts using SIMD
const _VMUL_SANITY: () = assert!(!COUNTSPILLER_USES_SIMD);

pub fn hwnd_vmul(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, _: PickleInstruction) {
  let (instdefined, count, typ, src1, src2, target) = arithprelude(meta, builder);

  let (mut target, t, of_t) = target;

  // [<Extended Flags (2 bits)>] [Padding (14 bits)]
  // The extended flags:
  // - 1x: we use Wide Multiplication (target must be able to store upto 2x the count)
  // - 0x: we use Lossy Multiplication (this is only time the other bit is read)
  // - 00: Output the 1st 32-bits (i.e. low bits)
  // - 01: Output the 2nd 32-bit (i.e. high bits)
  let eflags = (instdefined >> 14) as u8;

  let wide = (eflags & 0x03) == 1;
  let lowbits = (eflags & 0x01) == 0;

  let clif = typ.clif_mapping();

  if wide {
    target = resolve_location_src_store(builder, meta, typ, t, None, of_t, 2 * count);

    src1
      .into_iter()
      .zip(src2.into_iter())
      .enumerate()
      .for_each(|(idx, (src1, src2))| {
        let lo = builder.ins().imul(src1, src2);
        let hi = if clif.signed {
          builder.ins().smulhi(src1, src2)
        } else {
          builder.ins().umulhi(src1, src2)
        };

        target.store(builder, idx * 2, lo);
        target.store(builder, idx * 2 + 1, hi);
      });
  } else {
    src1
      .into_iter()
      .zip(src2.into_iter())
      .enumerate()
      .for_each(|(idx, (src1, src2))| {
        let val = if lowbits {
          builder.ins().imul(src1, src2)
        } else if clif.signed {
          builder.ins().smulhi(src1, src2)
        } else {
          builder.ins().umulhi(src1, src2)
        };

        target.store(builder, idx, val);
      });
  }

  target.synchronize(builder, meta);
}
