use std::mem::forget;

use cranelift::prelude::{FunctionBuilder, InstBuilder, Value, types::I64};

use crate::acaot::native::cranelift::{CompilerMeta, irgen::reg::resolve_reg};

pub fn resolve_loc_to_ptr(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,

  // Location-src
  locsrc: u8,
  offset: i32,
) -> LocToPtr {
  match locsrc {
    // Scratchpad
    8 => {
      let stadr = builder
        .ins()
        .stack_addr(meta.isa.pointer_type(), meta.scratchpad, offset);

      return LocToPtr {
        ptr: stadr,
        reg_touched: false,
      };
    }
    9 => {
      let stadr = builder.use_var(meta.largepad);

      let stadr = builder.ins().iadd_imm_u(stadr, offset as i64);

      return LocToPtr {
        ptr: stadr,
        reg_touched: false,
      };
    }
    10 => {
      let r2 = resolve_reg(builder, meta, 1);

      let ptr = builder.use_var(r2);
      let ptr = builder.ins().iadd_imm_u(ptr, offset as i64);

      return LocToPtr {
        ptr,
        reg_touched: false,
      };
    }
    11 => {
      let r3 = resolve_reg(builder, meta, 2);

      let ptr = builder.use_var(r3);
      let ptr = builder.ins().iadd_imm_u(ptr, offset as i64);

      return LocToPtr {
        ptr,
        reg_touched: false,
      };
    }
    _ => {}
  }

  [
    resolve_reg(builder, meta, 0),
    resolve_reg(builder, meta, 1),
    resolve_reg(builder, meta, 2),
    resolve_reg(builder, meta, 3),
    resolve_reg(builder, meta, 4),
    resolve_reg(builder, meta, 5),
    resolve_reg(builder, meta, 6),
    resolve_reg(builder, meta, 7),
  ]
  .into_iter()
  .enumerate()
  .for_each(|(idx, var)| {
    let var = builder.use_var(var);
    builder
      .ins()
      .stack_store(meta.isa.pointer_type(), var, meta.regspill, idx as i32 * 8);
  });

  let ptr = builder.ins().stack_addr(
    meta.isa.pointer_type(),
    meta.regspill,
    locsrc as i32 * 8 + offset,
  );
  LocToPtr {
    reg_touched: true,
    ptr,
  }
}

pub struct LocToPtr {
  reg_touched: bool,
  pub ptr: Value,
}

impl LocToPtr {
  pub fn sync(self, builder: &mut FunctionBuilder, meta: &mut CompilerMeta) {
    if self.reg_touched {
      (0..8).into_iter().for_each(|reg| {
        let val = builder
          .ins()
          .stack_load(meta.isa.pointer_type(), I64, meta.regspill, 8 * reg);

        let reg = resolve_reg(builder, meta, reg as u8);
        builder.def_var(reg, val);
      });
    }

    self.no_sync();
  }

  pub fn no_sync(self) {
    forget(self);
  }
}

impl Drop for LocToPtr {
  fn drop(&mut self) {
    panic!("Must synchronize");
  }
}
