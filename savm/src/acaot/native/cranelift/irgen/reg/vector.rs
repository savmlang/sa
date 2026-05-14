use cranelift::{
  codegen::ir::Endianness,
  prelude::{types::*, *},
};

use crate::acaot::native::cranelift::CompilerMeta;

pub fn reglane_insert(builder: &mut FunctionBuilder, reg: Value, valadd: Value, idx: u8) -> Value {
  let cliftype = builder.func.dfg.value_type(valadd);
  let single_elem_width = cliftype.lane_type().bytes();

  let valadd_extended = {
    let cast = builder.ins().bitcast(
      canonical(cliftype),
      MemFlags::new().with_endianness(Endianness::Little),
      valadd,
    );

    let ext = builder.ins().uextend(I64, cast);

    builder
      .ins()
      .ishl_imm(ext, single_elem_width as i64 * 8 * idx as i64)
  };

  let reg = {
    let mask = ((1u64 << (single_elem_width * 8)) - 1) << (single_elem_width * 8 * idx as u32);

    builder.ins().band_imm(reg, !mask.cast_signed())
  };

  builder.ins().bor(reg, valadd_extended)
}

pub fn reglane_extract(builder: &mut FunctionBuilder, reg: Value, shrink: Type, idx: u8) -> Value {
  let cliftype = shrink;
  let single_elem_width = cliftype.lane_type().bytes();

  let reg = {
    let mask = ((1u64 << (single_elem_width * 8)) - 1) << (single_elem_width * 8 * idx as u32);

    let masked = builder.ins().band_imm(reg, mask.cast_signed());

    builder
      .ins()
      .ushr_imm(masked, single_elem_width as i64 * 8 * idx as i64)
  };

  let ireduce = canonical(cliftype);
  let canoical = builder.ins().ireduce(ireduce, reg);

  if shrink.is_float() {
    builder.ins().bitcast(
      cliftype,
      MemFlags::new().with_endianness(Endianness::Little),
      canoical,
    )
  } else {
    canoical
  }
}

pub fn abstract_insertlane(
  builder: &mut FunctionBuilder,
  _: &mut CompilerMeta,
  vector: Value,
  valadd: Value,
  idx: u8,
) -> Value {
  return builder.ins().insertlane(vector, valadd, idx);
}

pub fn abstract_extractlane(
  builder: &mut FunctionBuilder,
  _: &mut CompilerMeta,
  vector: Value,
  idx: u8,
) -> Value {
  return builder.ins().extractlane(vector, idx);
}

fn canonical(cliftype: Type) -> Type {
  match cliftype.bytes() {
    8 => I64,
    4 => I32,
    2 => I16,
    1 => I8,
    _ => INVALID,
  }
}
