use std::ops::{Range, Rem};

use sart::structures::QuadPackedData;

use crate::acaot::native::llvm_compiler::irgen::reg::LLVMTypeMapping;

pub fn regmapper(
  reg0: u8,
  offset_bytes: i32,
  typedata: LLVMTypeMapping,
  count: u32,
  assumedwdt: Option<u32>,
) -> RegMapOut {
  let width = typedata.width as u32;

  let counts_in_1_reg = typedata.width / size_of::<QuadPackedData>() as u8;

  let offset_knot_in_count = offset_bytes as u32 / width;

  let regs = {
    let bytes_touched = width * count + offset_bytes as u32;
    let totalregstouched = bytes_touched.div_ceil(8) as u8;

    reg0..(reg0 + totalregstouched)
  };

  // let waterfall = break_simd_waterfall(
  //   if offset_knot_in_count == 0 { 8 } else { 1 },
  //   typedata,
  //   count,
  //   assumedwdt,
  // );

  // let map = waterfall
  //   .iter()
  //   .map(|&(additive_offset, dtype, _)| {
  //     let offset_cnt_additive = additive_offset / width;

  //     let total_lanes = dtype.lane_count();

  //     let lanes = (0..total_lanes)
  //       .map(|add| {
  //         let full_offset = offset_knot_in_count + offset_cnt_additive + add;

  //         let regidx = full_offset / counts_in_1_reg;
  //         let laneid = full_offset.rem(counts_in_1_reg);

  //         LaneData { regidx, laneid }
  //       })
  //       .collect();

  //     RegMapped {
  //       lanes,
  //       cliftype: typedata.x1.by(total_lanes).unwrap(),
  //     }
  //   })
  //   .collect();

  RegMapOut {
    regstouched: regs,
    vectmask: vec![],
  }
}

#[derive(Debug, Clone)]
pub struct RegMapOut {
  pub regstouched: Range<u8>,
  pub vectmask: Vec<u8>,
}
