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
  // let cliftype = builder.func.dfg.value_type(vector);

  // let single_elem_width = cliftype.lane_type().bytes();

  // if cliftype.bytes() > 8 {
  return builder.ins().insertlane(vector, valadd, idx);
  // }

  // let canoical = canonical(cliftype);

  // let lanes = 16 / single_elem_width;

  // let can = builder.ins().bitcast(
  //   canoical,
  //   MemFlags::new().with_endianness(Endianness::Little),
  //   vector,
  // );
  // let can_ext = builder.ins().uextend(I128, can);
  // let ext = builder.ins().bitcast(
  //   cliftype.lane_type().by(lanes).unwrap(),
  //   MemFlags::new().with_endianness(Endianness::Little),
  //   can_ext,
  // );
}

pub fn abstract_extractlane(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  vector: Value,
  idx: u8,
) -> Value {
  // let cliftype = builder.func.dfg.value_type(vector);

  // let single_elem_width = cliftype.lane_type().bytes();

  // // If bytes > 8, i.e. >64-bits
  // if cliftype.bytes() > 8 {
  return builder.ins().extractlane(vector, idx);
  // }

  // let canoical = canonical(cliftype);

  // // Largen both sides
  // let masked = {
  //   let val = builder.ins().bitcast(
  //     canoical,
  //     MemFlags::new().with_endianness(Endianness::Little),
  //     vector,
  //   );

  //   let mask = ((1u64 << single_elem_width * 8) - 1) << (single_elem_width * 8 * idx as u32);

  //   builder.ins().band_imm(val, mask.cast_signed())
  // };

  // let extracted = builder
  //   .ins()
  //   .ushr_imm(masked, (single_elem_width * 8 * (idx as u32)) as i64);

  // builder.ins().ireduce(cliftype.lane_type(), extracted)
}

// fn canonical(cliftype: Type) -> Type {
//   match cliftype.bytes() {
//     8 => {
//       // if cliftype.is_float() {
//       // F64
//       // } else {
//       I64
//       // }
//     }
//     4 => {
//       // if cliftype.is_float() {
//       //   F32
//       // } else {
//       I32
//       // }
//     }
//     2 => I16,
//     _ => INVALID,
//   }
// }
