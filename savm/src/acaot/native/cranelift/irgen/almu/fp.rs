use crate::acaot::pickle::reader::fp::{VFMA, parse_vfma};
use crate::acaot::pickle::{def::PickleInstruction, reader::fp::parse_vfp};
use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::{
      TypeOrWidth,
      reg::{StoreResolver, resolve_location_src_load, resolve_location_src_store},
    },
  },
  pickle::reader::fp::VFP,
};
use cranelift::{
  codegen::ir::Value,
  prelude::{FunctionBuilder, InstBuilder},
};

fn arithprelude(
  pickle: &PickleInstruction,
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
) -> (TypeOrWidth, Box<[Value]>, Box<[Value]>, StoreResolver) {
  let VFP {
    count,
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
    ..
  } = parse_vfp(pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Type(datatype);
  let src1 = { resolve_location_src_load(builder, meta, typ, src1 as u8, None, of_src1, count) };
  let src2 = { resolve_location_src_load(builder, meta, typ, src2 as u8, None, of_src2, count) };
  let target = { resolve_location_src_store(builder, meta, typ, tgt as u8, None, of_tgt, count) };

  (typ, src1, src2, target)
}

pub fn handle_vaddf(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, src1, src2, mut target) = arithprelude(&pickle, builder, meta);

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
  let (_, src1, src2, mut target) = arithprelude(&pickle, builder, meta);

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
  let (_, src1, src2, mut target) = arithprelude(&pickle, builder, meta);

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
  let (_, src1, src2, mut target) = arithprelude(&pickle, builder, meta);

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

fn fmaprelude(
  pickle: &PickleInstruction,
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
) -> (
  TypeOrWidth,
  Box<[Value]>,
  Box<[Value]>,
  Box<[Value]>,
  StoreResolver,
) {
  let VFMA {
    datatype,
    count,
    src1,
    of_src1,
    src2,
    of_src2,
    src3,
    of_src3,
    tgt,
    of_tgt,
  } = parse_vfma(pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Type(datatype);

  let src1 = { resolve_location_src_load(builder, meta, typ, src1 as u8, None, of_src1, count) };
  let src2 = { resolve_location_src_load(builder, meta, typ, src2 as u8, None, of_src2, count) };
  let src3 = { resolve_location_src_load(builder, meta, typ, src3 as u8, None, of_src3, count) };
  let target = { resolve_location_src_store(builder, meta, typ, tgt as u8, None, of_tgt, count) };

  (typ, src1, src2, src3, target)
}

pub fn handle_vfma(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, src1, src2, src3, mut target) = fmaprelude(&pickle, builder, meta);

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
