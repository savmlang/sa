use super::*;
use cranelift::prelude::{
  types::{
    I8X16, I8X32, I8X64, I16X8, I16X16, I16X32, I32X4, I32X8, I32X16, I64X2, I64X4, I64X8, INVALID,
  },
  *,
};

pub fn resolve_location_src_load(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  typ: TypeOrWidth,

  // Location-src
  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,
) -> Box<[Value]> {
  let typedata = typ.clif_mapping();
  let ofset = offset * typedata.width as i32;

  match locsrc {
    0..=7 => {
      let rg = resolve_reg(builder, meta, locsrc);

      todo!()
    }
    // Scratchpad
    8 => {
      let ptr = builder.ins().stack_addr(I64, meta.scratchpad, ofset);
      let alignment = get_max_alignment(64, ofset);

      break_simd_waterfall(alignment, typedata, count)
        .into_iter()
        .map(|(offset, mem, memflags)| builder.ins().load(mem, memflags, ptr, offset.cast_signed()))
        .collect::<Box<[_]>>()
    }
    9 => {
      let baseptr = builder.use_var(meta.largepad);

      let ptr = builder.ins().iadd_imm(baseptr, ofset as i64);
      let alignment = get_max_alignment(alignment.unwrap_or(8), ofset);

      break_simd_waterfall(alignment, typedata, count)
        .into_iter()
        .map(|(offset, mem, memflags)| builder.ins().load(mem, memflags, ptr, offset.cast_signed()))
        .collect::<Box<[_]>>()
    }
    // The pointer is to be read from r2
    10 => {
      let r2 = resolve_reg(builder, meta, 1);

      let ptr = builder.use_var(r2);
      let alignment = alignment.unwrap_or(1);

      break_simd_waterfall(alignment, typedata, count)
        .into_iter()
        .map(|(offset, mem, memflags)| builder.ins().load(mem, memflags, ptr, offset.cast_signed()))
        .collect::<Box<[_]>>()
    }
    _ => unreachable!(),
  }
}

fn get_max_alignment(base_align: u8, byteoffset: i32) -> u8 {
  if byteoffset == 0 {
    return base_align;
  }

  // The alignment of the offset is the largest power of 2 that divides it.
  // .trailing_zeros() gives us the 'n' in 2^n.
  let offset_align = 1 << byteoffset.trailing_zeros();

  // The resulting alignment is the minimum of the base and the offset's alignment.
  // (If base is 64-aligned and offset is 8-aligned, the result is 8-aligned).
  u8::min(base_align, offset_align)
}

pub enum ResolvedLocSrc {
  Registers { val: Value },
  ScratchpadPtr { value: Value, align_max: u8 },
  LargepadPtr { value: Value, align_max: u8 },
}

#[derive(Debug, Clone, Copy)]
pub enum TypeOrWidth {
  Type(u8),
  Width(u8),
}

impl TypeOrWidth {
  pub fn clif_mapping(&self) -> ClifTypeMapping {
    match self {
      Self::Type(typ) => match *typ {
        0 | 4 => ClifTypeMapping {
          width: 8,
          x1: I64,
          x2: INVALID,
          x4: INVALID,
          x8: INVALID,
          signed: *typ == 4,
        },
        1 | 5 => ClifTypeMapping {
          width: 4,
          x1: I32,
          x2: I32X2,
          x4: INVALID,
          x8: INVALID,
          signed: *typ == 5,
        },
        2 | 6 => ClifTypeMapping {
          width: 2,
          x1: I16,
          x2: I16X2,
          x4: I16X4,
          x8: INVALID,
          signed: *typ == 6,
        },
        3 | 7 => ClifTypeMapping {
          width: 1,
          x1: I8,
          x2: I8X2,
          x4: I8X4,
          x8: I8X8,
          signed: *typ == 7,
        },
        8 => ClifTypeMapping {
          width: 8,
          x1: F64,
          x2: INVALID,
          x4: INVALID,
          x8: INVALID,
          signed: false,
        },
        9 => ClifTypeMapping {
          width: 4,
          x1: F32,
          x2: F32X2,
          x4: INVALID,
          x8: INVALID,
          signed: false,
        },
        _ => unreachable!(),
      },
      Self::Width(wdt) => match *wdt {
        0 => Self::Type(0).clif_mapping(),
        1 => Self::Type(1).clif_mapping(),
        2 => Self::Type(2).clif_mapping(),
        3 => Self::Type(3).clif_mapping(),
        _ => unreachable!(),
      },
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct ClifTypeMapping {
  pub width: i32,
  pub x1: Type,
  pub x2: Type,
  pub x4: Type,
  pub x8: Type,
  pub signed: bool,
}

impl ClifTypeMapping {
  pub fn width(&self) -> u32 {
    self.width as _
  }

  pub fn simd_width_type(&self, simdbytes: u8) -> Type {
    let width = self.width();
    match simdbytes {
      // AVX512
      64 => match width {
        8 => I64X8,
        4 => I32X16,
        2 => I16X32,
        1 => I8X64,
        _ => INVALID,
      },
      // AVX256
      32 => match width {
        8 => I64X4,
        4 => I32X8,
        2 => I16X16,
        1 => I8X32,
        _ => INVALID,
      },
      // SEE 128B
      16 => match width {
        8 => I64X2,
        4 => I32X4,
        2 => I16X8,
        1 => I8X16,
        _ => INVALID,
      },
      8 => match width {
        8 => I64,
        4 => I32X2,
        2 => I16X4,
        1 => I8X8,
        _ => INVALID,
      },
      4 => match width {
        4 => I32,
        2 => I16X2,
        1 => I8X4,
        _ => INVALID,
      },
      2 => match width {
        2 => I16,
        1 => I8X2,
        _ => INVALID,
      },
      1 => match width {
        1 => I8,
        _ => INVALID,
      },
      _ => INVALID,
    }
  }
}
