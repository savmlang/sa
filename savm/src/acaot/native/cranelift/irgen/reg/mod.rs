use cranelift::prelude::{
  types::{F32, F32X2, F64, I16, I16X2, I16X4, I32, I32X2, I64, I8, I8X2, I8X4, I8X8},
  FunctionBuilder, InstBuilder, Type, Value, Variable,
};

use crate::acaot::native::cranelift::CompilerMeta;

mod countsplitter;
pub(crate) mod regmap;
mod resolve;
mod stackload;
pub(crate) mod vector;

pub use countsplitter::*;
pub use resolve::*;
pub use stackload::*;

pub fn get(builder: &mut FunctionBuilder, reg: &mut Option<Variable>) -> Variable {
  let curr = builder.current_block().unwrap();

  *reg.get_or_insert_with(|| builder.declare_var(I64))
}

pub fn resolve_reg(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, r: u8) -> Variable {
  match r {
    0 => get(builder, &mut meta.r1),
    1 => get(builder, &mut meta.r2),
    2 => get(builder, &mut meta.r3),
    3 => get(builder, &mut meta.r4),
    4 => get(builder, &mut meta.r5),
    5 => get(builder, &mut meta.r6),
    6 => get(builder, &mut meta.r7),
    7 => get(builder, &mut meta.r8),
    _ => unreachable!(),
  }
}
