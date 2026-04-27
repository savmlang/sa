use std::arch::is_aarch64_feature_detected;

use cranelift::prelude::{
  types::{I8, I16, I32, I64},
  *,
};

use crate::acaot::native::cranelift::irgen::reg::ClifTypeMapping;

fn memflags(alignment: u8, width: u8) -> MemFlags {
  let mut m = MemFlags::new();

  if alignment.is_multiple_of(width) && alignment.is_power_of_two() {
    m.set_aligned();
  }

  m
}

pub fn break_simd_waterfall(
  alignment: u8,
  map: ClifTypeMapping,
  count: u32,
  assumedwdt: Option<u32>,
) -> Vec<(u32, Type, MemFlags)> {
  if let Some(assumed) = assumedwdt {
    debug_assert!(assumed >= map.width());
    let cheat_type = get_cheat_type(assumed);

    let mut cheat = break_simd_waterfall_inner(
      alignment,
      ClifTypeMapping {
        float: false,
        signed: false,
        width: assumed.cast_signed(),
        x1: cheat_type,
        x1i: cheat_type,
        xreg: cheat_type,
      },
      count,
    );

    let mut offsetctr = 0;
    cheat.iter_mut().for_each(|(offset, ty, mflags)| {
      let lanes = ty.lane_count();

      *ty = map.x1.by(lanes).unwrap();
      *offset = offsetctr;

      let truewidth = lanes * map.width();
      offsetctr += truewidth;

      *mflags = memflags(alignment, truewidth as u8);
    });

    return cheat;
  }

  break_simd_waterfall_inner(alignment, map, count)
}

fn get_cheat_type(assumed: u32) -> Type {
  match assumed {
    8 => I64,
    4 => I32,
    2 => I16,
    1 => I8,
    _ => unreachable!(),
  }
}

pub fn break_simd_waterfall_inner(
  alignment: u8,
  map: ClifTypeMapping,
  mut count: u32,
) -> Vec<(u32, Type, MemFlags)> {
  let mut loadinst = vec![];

  // Over reserve
  loadinst.reserve((count / map.width()).max(1) as usize);

  let width = map.width();

  // Vector (Threshold in terms of count infact)
  let b512_threshold = 64 / width;
  let b256_threshold = 32 / width;
  let b128_threshold = 16 / width;
  // let b64_threshold = 8 / width;
  // let b32_threshold = 4 / width;
  // let b16_threshold = 2 / width;
  // let b8_threshold = 1 / width;

  // MemFlags (Vector)
  let b512_flags = memflags(alignment, 64);
  let b256_flags = memflags(alignment, 32);
  let b128_flags = memflags(alignment, 16);

  // MemFlags (Scalar)
  // let b64_flags = memflags(alignment, 8);
  // let b32_flags = memflags(alignment, 4);
  // let b16_flags = memflags(alignment, 2);
  let memflags = memflags(alignment, map.width() as u8);

  let mut offset: u32 = 0;

  // Waterfall load
  {
    // Don't even think of SIMD if payload < 16bytes
    if width * count >= 16 {
      #[cfg(target_arch = "x86_64")]
      {
        // Don't emit only 1x AVX512 hint
        // atleast 2
        //
        // Because it slows down systems
        // + calculate the offset as well
        if is_x86_feature_detected!("avx512f")
          && is_x86_feature_detected!("avx512bitalg")
          && is_x86_feature_detected!("avx512dq")
          && is_x86_feature_detected!("avx512vl")
          && is_x86_feature_detected!("avx512vbmi")
        {
          if b512_threshold > 0 {
            while count >= 2 * b512_threshold {
              loadinst.extend_from_slice(&[
                (offset, map.simd_width_type(64), b512_flags),
                (offset + 64, map.simd_width_type(64), b512_flags),
              ]);
              offset += 2 * 64;

              count -= 2 * b512_threshold;
            }
          }
        }

        if is_x86_feature_detected!("avx2") {
          if b256_threshold > 0 {
            while count >= b256_threshold {
              loadinst.push((offset, map.simd_width_type(32), b256_flags));
              offset += 32;

              count -= b256_threshold;
            }
          }
        }

        if is_x86_feature_detected!("sse2") {
          if b128_threshold > 0 {
            while count >= b128_threshold {
              loadinst.push((offset, map.simd_width_type(16), b128_flags));
              offset += 16;

              count -= b128_threshold;
            }
          }
        }
      }

      if b128_threshold > 0 {
        #[cfg(target_arch = "aarch64")]
        if is_aarch64_feature_detected!("neon") {
          while count >= b128_threshold {
            loadinst.push((offset, map.simd_width_type(16), b128_flags));
            offset += 16;

            count -= b128_threshold;
          }
        }
      }
    }

    // if b64_threshold > 0 {
    //   while count >= b64_threshold {
    //     loadinst.push((offset, map.simd_width_type(8), b64_flags));
    //     offset += 8;

    //     count -= b64_threshold;
    //   }
    // }

    // if b32_threshold > 0 {
    //   while count >= b32_threshold {
    //     loadinst.push((offset, map.simd_width_type(4), b32_flags));
    //     offset += 4;

    //     count -= b32_threshold;
    //   }
    // }

    // if b16_threshold > 0 {
    //   while count >= b16_threshold {
    //     loadinst.push((offset, map.simd_width_type(2), b16_flags));
    //     offset += 2;

    //     count -= b16_threshold;
    //   }
    // }

    // if b8_threshold > 0 {
    //   while count >= b8_threshold {
    //     loadinst.push((offset, map.simd_width_type(1), b8_flags));
    //     offset += 1;
    //
    //     count -= b8_threshold;
    //   }
    // }

    while count > 0 {
      loadinst.push((offset, map.x1, memflags));
      offset += 1;

      count -= 1;
    }
  }

  loadinst
}
