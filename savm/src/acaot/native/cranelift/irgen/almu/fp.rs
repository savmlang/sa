use cranelift::prelude::{FunctionBuilder, InstBuilder};

use crate::acaot::native::cranelift::{
  CompilerMeta,
  irgen::{
    TypeOrWidth,
    reg::{resolve_location_src_load, resolve_location_src_store},
  },
};
use crate::acaot::pickle::def::PickleInstruction;
use crate::readws;

macro_rules! arithprelude {
  ($pickle:ident, $builder:ident, $meta:ident, $task:ident) => {{
    let f1 = $pickle.u1;
    let f2 = $pickle.u2;

    let flags = u16::from_ne_bytes([f1, f2]);

    let fptype = ((flags >> 12) & 0x01) as u8;

    let typ = TypeOrWidth::Type(match fptype {
      0 => 8,
      1 => 9,
      _ => unreachable!(),
    });

    let _inst = ((flags >> 14) & 0x01) as u8;

    let count = readws!($meta, start = 0, stop = 4, u32);

    let ofset1 = readws!($meta, start = 4, stop = 8, i32);
    let ofset2 = readws!($meta, start = 8, stop = 12, i32);
    let ofset3 = readws!($meta, start = 12, stop = 16, i32);

    let src1 = {
      let src = (flags >> 8 as u8) & 0x0F;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset1, count)
    };

    let src2 = {
      let src = (flags as u8) >> 4;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset2, count)
    };

    let target = {
      let src = (flags as u8) & 0x0F;

      resolve_location_src_store($builder, $meta, typ, src as u8, None, ofset3, count)
    };

    (typ, src1, src2, target)
  }};
}

pub fn handle_vaddf(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, src1, src2, mut target) = arithprelude!(pickle, builder, meta, taskstate);

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .for_each(|(idx, (src1, src2))| {
      let val = builder.ins().fadd(src1, src2);

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}

pub fn handle_vsubf(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, src1, src2, mut target) = arithprelude!(pickle, builder, meta, taskstate);

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .for_each(|(idx, (src1, src2))| {
      let val = builder.ins().fsub(src1, src2);

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}

pub fn handle_vmulf(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, src1, src2, mut target) = arithprelude!(pickle, builder, meta, taskstate);

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .for_each(|(idx, (src1, src2))| {
      let val = builder.ins().fmul(src1, src2);

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}

pub fn handle_vdivf(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, src1, src2, mut target) = arithprelude!(pickle, builder, meta, taskstate);

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .for_each(|(idx, (src1, src2))| {
      let val = builder.ins().fdiv(src1, src2);

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}

macro_rules! fmaprelude {
  ($pickle:ident, $builder:ident, $meta:ident, $task:ident) => {{
    let f1 = $pickle.u1;
    let f2 = $pickle.u2;

    let flags = u16::from_ne_bytes([f1, f2]);

    let fptype = ((flags >> 12) & 0x01) as u8;

    let typ = TypeOrWidth::Type(match fptype {
      0 => 8,
      1 => 9,
      _ => unreachable!(),
    });

    let _inst = ((flags >> 14) & 0x01) as u8;

    let count = readws!($meta, start = 0, stop = 4, u32);

    let ofset1 = readws!($meta, start = 4, stop = 8, i32);
    let ofset2 = readws!($meta, start = 8, stop = 12, i32);
    let ofset3 = readws!($meta, start = 12, stop = 16, i32);
    let ofset4 = readws!($meta, start = 16, stop = 20, i32);

    let src1 = {
      let src = (flags >> 12) & 0x0F;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset1, count)
    };

    let src2 = {
      let src = (flags >> 8) & 0x0F;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset2, count)
    };

    let src3 = {
      let src = (flags as u8) >> 4;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset3, count)
    };

    let target = {
      let src = (flags as u8) & 0x0F;

      resolve_location_src_store($builder, $meta, typ, src as u8, None, ofset4, count)
    };

    (typ, src1, src2, src3, target)
  }};
}

pub fn handle_vfma(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, src1, src2, src3, mut target) = fmaprelude!(pickle, builder, meta, taskstate);

  src1
    .into_iter()
    .zip(src2.into_iter())
    .zip(src3.into_iter())
    .enumerate()
    .for_each(|(idx, ((src1, src2), src3))| {
      let val = builder.ins().fma(src1, src2, src3);

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}
