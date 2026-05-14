use crate::acaot::native::cranelift::irgen::reg::{
  regmap::{RegMapOut, regmapper},
  vector::{abstract_extractlane, abstract_insertlane, reglane_extract, reglane_insert},
};

use super::*;
use cranelift::{
  codegen::ir::Endianness,
  prelude::{types::INVALID, *},
};

#[inline(always)]
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
  resolve_location_src_load_assumedwdt(builder, meta, typ, locsrc, alignment, offset, count, None)
}

pub fn resolve_location_src_load_assumedwdt(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  typ: TypeOrWidth,

  // Location-src
  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,

  assumedwdt: Option<u32>,
) -> Box<[Value]> {
  let typedata = typ.clif_mapping();
  let ofset = offset * typedata.width as i32;

  match locsrc {
    0..=7 => {
      let out = regmapper(locsrc, ofset, typedata, count, assumedwdt);

      let regs = out
        .regstouched
        .map(|x| {
          let v = resolve_reg(builder, meta, x);
          let r = builder.use_var(v);

          // builder.ins().bitcast(
          //   typedata.xreg,
          //   MemFlags::new().with_endianness(Endianness::Little),
          //   r,
          // )
          r
        })
        .collect::<Box<[_]>>();

      let ctrl = if typedata.float {
        match typedata.width {
          8 => builder.ins().f64const(0.0),
          4 => builder.ins().f32const(0.0),
          _ => unreachable!(),
        }
      } else {
        builder.ins().iconst(typedata.x1, 0)
      };

      out
        .map
        .into_iter()
        .map(|x| {
          let singlelane = x.lanes.len() == 1;

          let mut simdval = if singlelane {
            ctrl
          } else {
            builder.ins().scalar_to_vector(x.cliftype, ctrl)
          };

          for (idx, lane) in x.lanes.into_iter().enumerate() {
            let reg = regs[lane.regidx as usize];

            let typ = builder.func.dfg.value_type(reg);

            // The register only has one lane
            // i.e. its guaranteed to be u64
            let val = if typ == typedata.x1 {
              reg
            } else if typ == typedata.x1i {
              builder.ins().bitcast(
                typedata.x1,
                MemFlags::new().with_endianness(Endianness::Little),
                reg,
              )
            } else {
              reglane_extract(builder, reg, typedata.x1, lane.laneid as u8)
            };

            simdval = if singlelane {
              val
            } else {
              abstract_insertlane(builder, meta, simdval, val, idx as u8)
            };
          }

          simdval
        })
        .collect()
    }
    // Scratchpad
    8 => {
      let ptr = builder.ins().stack_addr(I64, meta.scratchpad, ofset);
      let alignment = get_max_alignment(64, ofset);

      break_simd_waterfall(alignment, typedata, count, assumedwdt)
        .into_iter()
        .map(|(offset, mem, memflags)| builder.ins().load(mem, memflags, ptr, offset.cast_signed()))
        .collect::<Box<[_]>>()
    }
    9 => {
      let baseptr = builder.use_var(meta.largepad);

      let ptr = builder.ins().iadd_imm(baseptr, ofset as i64);
      let alignment = get_max_alignment(alignment.unwrap_or(8), ofset);

      break_simd_waterfall(alignment, typedata, count, assumedwdt)
        .into_iter()
        .map(|(offset, mem, memflags)| builder.ins().load(mem, memflags, ptr, offset.cast_signed()))
        .collect::<Box<[_]>>()
    }
    // The pointer is to be read from r2
    10 => {
      let r2 = resolve_reg(builder, meta, 1);

      let ptr = builder.use_var(r2);
      let alignment = alignment.unwrap_or(1);

      break_simd_waterfall(alignment, typedata, count, assumedwdt)
        .into_iter()
        .map(|(offset, mem, memflags)| builder.ins().load(mem, memflags, ptr, offset.cast_signed()))
        .collect::<Box<[_]>>()
    }
    _ => unreachable!(),
  }
}

pub fn resolve_location_src_store(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  typ: TypeOrWidth,

  // Location-src
  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,
) -> StoreResolver {
  resolve_location_src_store_assumedwdt(builder, meta, typ, locsrc, alignment, offset, count, None)
}

pub fn resolve_location_src_store_assumedwdt(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  typ: TypeOrWidth,

  // Location-src
  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,

  assumedwdt: Option<u32>,
) -> StoreResolver {
  let typedata = typ.clif_mapping();
  let ofset = offset * typedata.width as i32;

  match locsrc {
    0..=7 => {
      let mapout = regmapper(locsrc, ofset, typedata, count, assumedwdt);

      StoreResolver::Regs {
        mapout,
        typedata,
        values: vec![],
      }
    }
    // Scratchpad
    8 => {
      let ptr = builder.ins().stack_addr(I64, meta.scratchpad, ofset);
      let alignment = get_max_alignment(64, ofset);

      let waterfall =
        break_simd_waterfall(alignment, typedata, count, assumedwdt).into_boxed_slice();

      StoreResolver::Pointer {
        baseptr: ptr,
        storeseq: waterfall,
        regtouches: Box::new([]),
      }
    }
    9 => {
      let baseptr = builder.use_var(meta.largepad);

      let ptr = builder.ins().iadd_imm(baseptr, ofset as i64);
      let alignment = get_max_alignment(alignment.unwrap_or(8), ofset);

      let waterfall =
        break_simd_waterfall(alignment, typedata, count, assumedwdt).into_boxed_slice();

      StoreResolver::Pointer {
        baseptr: ptr,
        storeseq: waterfall,
        regtouches: Box::new([]),
      }
    }
    // The pointer is to be read from r2
    10 => {
      let r2 = resolve_reg(builder, meta, 1);

      let ptr = builder.use_var(r2);
      let alignment = alignment.unwrap_or(1);

      let data = break_simd_waterfall(alignment, typedata, count, assumedwdt).into_boxed_slice();

      StoreResolver::Pointer {
        baseptr: ptr,
        regtouches: Box::new([]),
        storeseq: data,
      }
    }
    _ => unreachable!(),
  }
}

// pub struct StoreResolver {
//   pub baseptr: Value,
//   pub core_width: u32,
//   pub offset_bits: i64,
//   ,
//   ,
// }

pub enum StoreResolver {
  Pointer {
    baseptr: Value,
    storeseq: Box<[(u32, Type, MemFlags)]>,
    regtouches: Box<[Variable]>,
  },
  Regs {
    mapout: RegMapOut,

    typedata: ClifTypeMapping,

    // Values
    //
    // As it is used, it stores the values
    values: Vec<Value>,
  },
}

impl StoreResolver {
  pub fn waterfall_typerating(&self) -> Box<[Type]> {
    match self {
      Self::Pointer { storeseq, .. } => storeseq.iter().map(|&(_, x, _)| x).collect::<Box<[_]>>(),
      Self::Regs { mapout, .. } => mapout
        .waterfall
        .iter()
        .map(|&(_, x, _)| x)
        .collect::<Box<[_]>>(),
    }
  }

  pub fn total(&self) -> usize {
    match self {
      Self::Pointer { storeseq, .. } => storeseq.len(),
      _ => 0,
    }
  }

  pub fn store(&mut self, builder: &mut FunctionBuilder, idx: usize, val: Value) {
    match self {
      Self::Pointer {
        baseptr, storeseq, ..
      } => {
        let &(offset, _, memflags) = &storeseq[idx];
        builder.ins().store(memflags, val, *baseptr, offset as i32);
      }
      Self::Regs { values, .. } => {
        values.push(val);
      }
    }
  }

  pub fn synchronize(self, builder: &mut FunctionBuilder, meta: &mut CompilerMeta) {
    match &self {
      Self::Pointer {
        baseptr,
        regtouches,
        ..
      } => {
        for (idx, &register) in regtouches.iter().enumerate() {
          let offset = (idx as i32) * 8;

          let val = builder
            .ins()
            .load(I64, MemFlags::trusted(), *baseptr, offset);
          builder.def_var(register, val);
        }
      }
      Self::Regs {
        mapout: out,
        typedata,
        values,
      } => {
        let mut regs = out
          .regstouched
          .clone()
          .map(|x| {
            let v = resolve_reg(builder, meta, x);
            let r = builder.use_var(v);

            (
              // builder.ins().bitcast(
              //   typedata.xreg,
              //   MemFlags::new().with_endianness(Endianness::Little),
              //   r,
              // ),
              r, v,
            )
          })
          .collect::<Box<[_]>>();

        out
          .map
          .iter()
          .zip(values.iter().map(|x| *x))
          .for_each(|(x, valuetoset)| {
            let singlelane = x.lanes.len() == 1;

            for (idx, &lane) in x.lanes.iter().enumerate() {
              let value_to_set = if singlelane {
                valuetoset
              } else {
                abstract_extractlane(builder, meta, valuetoset, idx as u8)
              };

              let (reg, _) = &mut regs[lane.regidx as usize];

              let typ = builder.func.dfg.value_type(*reg);

              // No reglane dance if its already I64
              if typ.bytes() == typedata.x1.bytes() {
                *reg = value_to_set;
              } else {
                let output = reglane_insert(builder, *reg, value_to_set, lane.laneid as u8);

                *reg = output;
              }
            }
          });

        for (latest, reg) in regs {
          let t = builder.func.dfg.value_type(latest);

          let mut latest = latest;

          if t != I64 {
            latest = builder.ins().bitcast(
              I64,
              MemFlags::new().with_endianness(Endianness::Little),
              latest,
            );
          }

          builder.def_var(reg, latest);
        }
      }
    }

    std::mem::forget(self);
  }
}

impl Drop for StoreResolver {
  fn drop(&mut self) {
    panic!("synchronize must be used to ensure to synchronization");
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
          x1i: I64,
          xreg: I64,
          signed: *typ == 4,
          float: false,
        },
        1 | 5 => ClifTypeMapping {
          width: 4,
          x1: I32,
          x1i: I32,
          xreg: I32X2,
          signed: *typ == 5,
          float: false,
        },
        2 | 6 => ClifTypeMapping {
          width: 2,
          x1: I16,
          x1i: I16,
          xreg: I16X4,
          signed: *typ == 6,
          float: false,
        },
        3 | 7 => ClifTypeMapping {
          width: 1,
          x1: I8,
          x1i: I8,
          xreg: I8X8,
          signed: *typ == 7,
          float: false,
        },
        8 => ClifTypeMapping {
          width: 8,
          x1: F64,
          x1i: I64,
          xreg: F64,
          signed: false,
          float: true,
        },
        9 => ClifTypeMapping {
          width: 4,
          x1: F32,
          x1i: I32,
          xreg: F32X2,
          signed: false,
          float: true,
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
  pub x1i: Type,
  pub xreg: Type,
  pub signed: bool,
  pub float: bool,
}

impl ClifTypeMapping {
  pub fn width(&self) -> u32 {
    self.width as _
  }

  pub fn simd_width_type(&self, simdbytes: u8) -> Type {
    let width = self.width();

    let coretype = if self.float {
      match width {
        8 => F64,
        4 => F32,
        _ => return INVALID,
      }
    } else {
      match width {
        8 => I64,
        4 => I32,
        2 => I16,
        1 => I8,
        _ => return INVALID,
      }
    };

    if simdbytes as u32 == coretype.bytes() {
      return coretype;
    }

    coretype
      .by(simdbytes as u32 / coretype.bytes())
      .unwrap_or(INVALID)
  }
}
