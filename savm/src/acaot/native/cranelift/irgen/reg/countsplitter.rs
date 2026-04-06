use cranelift::prelude::*;

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
  mut count: u32,
) -> Vec<(u32, Type, MemFlags)> {
  let mut loadinst = vec![];

  // Over reserve
  loadinst.reserve(count as usize);

  let width = map.width();

  // Vector
  let b512_threshold = 64 / width;
  let b256_threshold = 32 / width;
  let b128_threshold = 16 / width;

  // Scalar
  let b64_threshold = 8 / width;
  let b32_threshold = 4 / width;
  let b16_threshold = 2 / width;
  let b8_threshold = 1 / width;

  // MemFlags (Vector)
  let b512_flags = memflags(alignment, 64);
  let b256_flags = memflags(alignment, 32);
  let b128_flags = memflags(alignment, 16);

  // MemFlags (Scalar)
  let b64_flags = memflags(alignment, 8);
  let b32_flags = memflags(alignment, 4);
  let b16_flags = memflags(alignment, 2);
  let b8_flags = memflags(alignment, 1);

  let mut offset: u32 = 0;

  // Waterfall load
  {
    // Don't emit only 1x AVX512 hint
    // atleast 2
    //
    // Because it slows down systems
    // + calculate the offset as well
    while count >= 2 * b512_threshold {
      loadinst.extend_from_slice(&[
        (offset, map.simd_width_type(64), b512_flags),
        (offset + 64, map.simd_width_type(64), b512_flags),
      ]);
      offset += 2 * 64;

      count -= 2 * b512_threshold;
    }

    while count >= b256_threshold {
      loadinst.push((offset, map.simd_width_type(32), b256_flags));
      offset += 32;

      count -= b256_threshold;
    }

    while count >= b128_threshold {
      loadinst.push((offset, map.simd_width_type(16), b128_flags));
      offset += 16;

      count -= b128_threshold;
    }

    while count >= b64_threshold {
      loadinst.push((offset, map.simd_width_type(8), b64_flags));
      offset += 8;

      count -= b64_threshold;
    }

    if b32_threshold > 0 {
      while count >= b32_threshold {
        loadinst.push((offset, map.simd_width_type(4), b32_flags));
        offset += 4;

        count -= b32_threshold;
      }
    }

    if b16_threshold > 0 {
      while count >= b16_threshold {
        loadinst.push((offset, map.simd_width_type(2), b16_flags));
        offset += 2;

        count -= b16_threshold;
      }
    }

    if b8_threshold > 0 {
      while count >= b8_threshold {
        loadinst.push((offset, map.simd_width_type(1), b8_flags));
        offset += 1;

        count -= b8_threshold;
      }
    }
  }

  loadinst
}
