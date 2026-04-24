//! Arithmatic-Logic-Memory Unit

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{TypeOrWidth, resolve_location_src_load, resolve_location_src_store, resolve_reg},
  },
  pickle::def::PickleInstruction,
};
use cranelift::{codegen::ir::Endianness, prelude::*};

mod atomic;
mod fp;
mod vbit;
mod vcmp;
mod vdataop;
mod vmcore;
mod vscncop;

pub use atomic::*;
pub use fp::*;
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

#[macro_export]
macro_rules! divlike {
  ($pickle:ident, $meta:ident, $builder:ident) => {{
    let args = u16::from_ne_bytes([$pickle.u1, $pickle.u2]);

    let typ = TypeOrWidth::Type((args >> 12) as u8);

    let of_src1 = readws!($meta, start = 0, stop = 4, i32);
    let of_src2 = readws!($meta, start = 4, stop = 8, i32);
    let of_src3 = readws!($meta, start = 8, stop = 12, i32);

    let src1 = {
      let src = (args >> 8 as u8) & 0x0F;

      let [src] = *resolve_location_src_load($builder, $meta, typ, src as u8, None, of_src1, 1)
      else {
        unreachable!()
      };
      src
    };
    let src2 = {
      let src = (args as u8) >> 4;

      let [src] = *resolve_location_src_load($builder, $meta, typ, src as u8, None, of_src2, 1)
      else {
        unreachable!()
      };
      src
    };
    let target = {
      let src = (args as u8) & 0x0F;

      resolve_location_src_store($builder, $meta, typ, src as u8, None, of_src3, 1)
    };

    (typ, src1, src2, target)
  }};
}

pub fn hwnd_div(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, pickle: PickleInstruction) {
  let (typ, src1, src2, mut target) = divlike!(pickle, meta, builder);

  let tgt = if typ.clif_mapping().signed {
    builder.ins().sdiv(src1, src2)
  } else {
    builder.ins().udiv(src1, src2)
  };

  target.store(builder, 0, tgt);
  target.synchronize(builder, meta);
}

pub fn hwnd_rem(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, pickle: PickleInstruction) {
  let (typ, src1, src2, mut target) = divlike!(pickle, meta, builder);

  let tgt = if typ.clif_mapping().signed {
    builder.ins().srem(src1, src2)
  } else {
    builder.ins().urem(src1, src2)
  };

  target.store(builder, 0, tgt);
  target.synchronize(builder, meta);
}

#[macro_export]
macro_rules! arithprelude {
  ($pickle:ident, $meta:ident, $builder:ident) => {{
    let flags = readws!($meta, start = 0, stop = 4, u32);

    let instdefined = flags as u16;

    let topflags = (flags >> 16) as u16;

    let typ = TypeOrWidth::Type((topflags >> 12) as u8);

    let count = readws!($meta, start = 4, stop = 8, u32);

    let ofset1 = readws!($meta, start = 8, stop = 12, i32);
    let ofset2 = readws!($meta, start = 12, stop = 16, i32);
    let ofset3 = readws!($meta, start = 16, stop = 20, i32);

    let src1 = {
      let src = (topflags >> 8 as u8) & 0x0F;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset1, count)
    };

    let src2 = {
      let src = (topflags as u8) >> 4;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset2, count)
    };

    let target = {
      let src = (topflags as u8) & 0x0F;

      (
        resolve_location_src_store($builder, $meta, typ, src as u8, None, ofset3, count),
        src,
        ofset3,
      )
    };

    (instdefined, count, typ, src1, src2, target)
  }};
}

pub fn hwnd_vadd(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (instdefined, count, typ, src1, src2, (mut target, _, _)) =
    arithprelude!(pickle, meta, builder);

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
      builder.ins().extractlane(old, 0)
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
      let extend = builder.ins().insertlane(r5_val, of, 0);
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

pub fn hwnd_vsub(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (instdefined, count, typ, src1, src2, (mut target, _, _)) =
    arithprelude!(pickle, meta, builder);

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
      builder.ins().extractlane(old, 0)
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
      let extend = builder.ins().insertlane(r5_val, of, 0);
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

pub fn hwnd_vmul(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (instdefined, count, typ, src1, src2, target) = arithprelude!(pickle, meta, builder);

  let (mut target, t_src, t_offset) = target;

  // [<Extended Flags (2 bits)>] [Padding (14 bits)]
  // The extended flags:
  // - x0: Output the 1st 32-bits (i.e. low bits)
  // - x1: Output the 2nd 32-bit (i.e. high bits)
  // - 1x: we use Wide Multiplication (target must be able to store upto 2x the count)
  // - 0x: we use Lossy Multiplication (this is only time the other bit is read)
  let eflags = (instdefined >> 14) as u8;

  let wide = (eflags & 0x03) == 1;
  let lowbits = (eflags & 0x01) == 0;

  let clif = typ.clif_mapping();

  if wide {
    let basetype = clif.x1;
    target = resolve_location_src_store(builder, meta, typ, t_src, None, t_offset, count * 2);

    // Split into MAANY individual values, like it or not
    let mulresult = src1
      .into_iter()
      .zip(src2.into_iter())
      .enumerate()
      .map(|(idx, (src1, src2))| {
        let lo = builder.ins().imul(src1, src2);
        let hi = if clif.signed {
          builder.ins().smulhi(src1, src2)
        } else {
          builder.ins().umulhi(src1, src2)
        };
        let tp = builder.func.dfg.value_type(lo);

        // we're looking at scalars
        if tp.lane_count() == 1 {
          return vec![lo, hi];
        }

        let mut out = vec![];

        (0..tp.lane_count()).for_each(|laneid| {
          let hi = builder.ins().extractlane(hi, laneid as u8);
          let lo = builder.ins().extractlane(lo, laneid as u8);

          out.push(lo);
          out.push(hi);
        });

        out
      })
      .flatten()
      .collect::<Box<[_]>>();

    let control = builder.ins().iconst(clif.x1, 0);

    let mut idxmul = 0usize;
    target
      .waterfall_typerating()
      .into_iter()
      .enumerate()
      .for_each(|(idx, ty)| {
        let lanes = ty.lane_count();

        if lanes == 1 {
          target.store(builder, idx, mulresult[idxmul]);
          idxmul += 1;
        } else {
          let mut datavect = builder.ins().scalar_to_vector(ty, control);

          (0..lanes).for_each(|idx| {
            datavect = builder
              .ins()
              .insertlane(datavect, mulresult[idxmul], idx as u8);

            idxmul += 1;
          });

          target.store(builder, idx, datavect);
        }
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
