use std::ops::{Range, Rem};

use cranelift::prelude::{MemFlags, Type};

use crate::acaot::native::cranelift::irgen::reg::{ClifTypeMapping, break_simd_waterfall};

pub fn regmapper(
  reg0: u8,
  offset_bytes: i32,
  typedata: ClifTypeMapping,
  count: u32,
  assumedwdt: Option<u32>,
) -> RegMapOut {
  let wdt = typedata.width();

  let cnts_in_1_reg = typedata.xreg.lane_count();

  let offset_count = offset_bytes as u32 / wdt;

  let bytes_touched = wdt * count + offset_bytes as u32;
  let totalregstouched = bytes_touched.div_ceil(8) as u8;

  let regs = (reg0..(reg0 + totalregstouched));

  let waterfall = break_simd_waterfall(8, typedata, count, assumedwdt);

  let map = waterfall
    .iter()
    .map(|&(additive_offset, dtype, _)| {
      let offset_cnt_additive = additive_offset / wdt;

      let total_lanes = dtype.lane_count();

      let lanes = (0..total_lanes)
        .map(|add| {
          let parity = (offset_count + offset_cnt_additive + add);

          let regidx = parity / cnts_in_1_reg;
          let laneid = parity.rem(cnts_in_1_reg);

          LaneData { regidx, laneid }
        })
        .collect();

      RegMapped {
        lanes,
        cliftype: typedata.x1.by(total_lanes).unwrap(),
      }
    })
    .collect();

  RegMapOut {
    regstouched: regs,
    waterfall,
    map,
  }
}

#[derive(Debug, Clone)]
pub struct RegMapOut {
  pub regstouched: Range<u8>,
  pub waterfall: Vec<(u32, Type, MemFlags)>,
  pub map: Vec<RegMapped>,
}

#[derive(Debug, Clone)]
pub struct RegMapped {
  // Lanes
  pub lanes: Box<[LaneData]>,
  pub cliftype: Type,
}

#[derive(Debug, Clone, Copy)]

pub struct LaneData {
  pub regidx: u32,
  pub laneid: u32,
}
