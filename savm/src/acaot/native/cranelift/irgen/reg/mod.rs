use cranelift::{
  codegen::{
    cursor::{Cursor, FuncCursor},
    ir::{Block, MemFlagsData},
  },
  prelude::{
    FunctionBuilder, InstBuilder, Type, Value, Variable,
    types::{F32, F32X2, F64, I8, I8X8, I16, I16X4, I32, I32X2, I64},
  },
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

pub fn get(
  builder: &mut FunctionBuilder,
  vmctx: Value,
  prologue: Block,
  r: u8,
  reg: &mut Option<Variable>,
) -> Variable {
  *reg.get_or_insert_with(|| {
    let Some(last_inst) = builder.func.layout.last_inst(prologue) else {
      unreachable!()
    };

    let mut cursor = FuncCursor::new(&mut builder.func);

    cursor.goto_inst(last_inst);
    let val = cursor
      .ins()
      .load(I64, MemFlagsData::trusted(), vmctx, r as i32 * 8);

    let variable = builder.declare_var(I64);
    builder.def_var(variable, val);

    variable
  })
}

pub fn resolve_reg(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, r: u8) -> Variable {
  match r {
    0 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r1),
    1 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r2),
    2 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r3),
    3 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r4),
    4 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r5),
    5 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r6),
    6 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r7),
    7 => get(builder, meta.vmtaskstate, meta.prologue, r, &mut meta.r8),
    _ => unreachable!(),
  }
}
