use cranelift::{
  codegen::ir::Endianness,
  prelude::{types::*, *},
};

use crate::acaot::native::cranelift::CompilerMeta;

pub fn abstract_insertlane(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  vector: Value,
  valadd: Value,
  idx: u8,
) -> Value {
  let cliftype = builder.func.dfg.value_type(vector);

  let single_elem_width = cliftype.lane_type().bytes();

  // If bytes > 8, i.e. >64-bits
  if cliftype.bytes() > 8 {
    return builder.ins().insertlane(vector, valadd, idx);
  }

  builder.ins().stack_store(vector, meta.regspill, 0);

  let canoical = canonical(cliftype);

  // Largen both sides
  let vect = {
    let val = builder.ins().stack_load(canoical, meta.regspill, 0);

    let mask = ((1u64 << single_elem_width * 8) - 1) << (single_elem_width * 8 * idx as u32);

    builder.ins().band_imm(val, !mask.cast_signed())
  };

  let val_shifted = {
    let bits_shl = single_elem_width * 8 * (idx as u32);
    let val = builder.ins().uextend(canoical, valadd);

    builder.ins().ishl_imm(val, bits_shl as i64)
  };

  let vect = builder.ins().bor(vect, val_shifted);

  builder.ins().stack_store(vect, meta.regspill, 0);

  builder.ins().stack_load(cliftype, meta.regspill, 0)
}

pub fn abstract_extractlane(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  vector: Value,
  idx: u8,
) -> Value {
  let cliftype = builder.func.dfg.value_type(vector);

  let single_elem_width = cliftype.lane_type().bytes();

  // If bytes > 8, i.e. >64-bits
  if cliftype.bytes() > 8 {
    return builder.ins().extractlane(vector, idx);
  }

  builder.ins().stack_store(vector, meta.regspill, 0);

  let canoical = canonical(cliftype);

  // Largen both sides
  let masked = {
    let val = builder.ins().stack_load(canoical, meta.regspill, 0);

    let mask = ((1u64 << single_elem_width * 8) - 1) << (single_elem_width * 8 * idx as u32);

    builder.ins().band_imm(val, mask.cast_signed())
  };

  let extracted = builder
    .ins()
    .ushr_imm(masked, (single_elem_width * 8 * (idx as u32)) as i64);

  builder.ins().ireduce(cliftype.lane_type(), extracted)
}

fn canonical(cliftype: Type) -> Type {
  match cliftype.bytes() {
    8 => {
      // if cliftype.is_float() {
      // F64
      // } else {
      I64
      // }
    }
    4 => {
      // if cliftype.is_float() {
      //   F32
      // } else {
      I32
      // }
    }
    2 => I16,
    _ => INVALID,
  }
}
