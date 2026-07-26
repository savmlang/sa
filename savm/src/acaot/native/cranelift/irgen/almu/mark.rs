use cranelift::{
  codegen::ir::{BlockArg, InstBuilderBase, condcodes::IntCC, immediates::Imm64, types::I64},
  frontend::FunctionBuilder,
  prelude::InstBuilder,
};

use crate::acaot::native::cranelift::CompilerMeta;

pub fn mark_optflow(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, marker: u64) {
  let markerid = builder
    .ins()
    .build_imm_const(I64, Imm64::new(marker as i64), false);

  let hotness = builder.use_var(meta.hotness);
  let hotness_new = builder.ins().iadd_imm_u(hotness, 1);
  builder.def_var(meta.hotness, hotness_new);

  // Jump if HIT
  {
    // instead of modulo - we do a >=256
    let hit = builder
      .ins()
      .icmp_imm_u(IntCC::UnsignedGreaterThanOrEqual, hotness_new, 256);

    let continue_block = builder.create_block();

    builder.ins().brif(
      hit,
      meta.suspend_epilogue,
      &[BlockArg::Value(markerid)],
      continue_block,
      &[],
    );

    builder.switch_to_block(continue_block);
  }
}
