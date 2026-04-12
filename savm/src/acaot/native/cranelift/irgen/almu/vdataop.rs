use cranelift::prelude::{FunctionBuilder, InstBuilder};

use crate::{
  acaot::{
    native::cranelift::{
      CompilerMeta,
      irgen::reg::{TypeOrWidth, resolve_location_src_load, resolve_location_src_store},
    },
    pickle::def::PickleInstruction,
  },
  readws,
};

macro_rules! handle_vdata_op {
  ($pickle:ident, $builder:ident, $meta:ident) => {{
    let f1 = $pickle.u1;
    let f2 = $pickle.u2;

    let flags = u16::from_ne_bytes([f1, f2]);

    let dtype = (flags >> 12) as u8;
    let typ = TypeOrWidth::Type(dtype);

    let count = readws!($meta, start = 0, stop = 4, u32);

    let ofset1 = readws!($meta, start = 4, stop = 8, i32);
    let ofset2 = readws!($meta, start = 12, stop = 16, i32);

    let src1 = {
      let src = (flags >> 8) as u8 & 0x0F;

      resolve_location_src_load($builder, $meta, typ, src as u8, None, ofset1, count)
    };

    let target = {
      let src = (flags as u8) >> 4;

      resolve_location_src_store($builder, $meta, typ, src as u8, None, ofset2, count)
    };

    (flags, typ, src1, target)
  }};
}

pub fn hwnd_vabs(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, typ, src, mut target) = handle_vdata_op!(pickle, builder, meta);

  let clif = typ.clif_mapping();

  src.into_iter().enumerate().for_each(|(idx, src)| {
    let val = if clif.float {
      builder.ins().fabs(src)
    } else if clif.signed {
      builder.ins().iabs(src)
    } else {
      src
    };
    target.store(builder, idx, val);
  });

  target.synchronize(builder, meta);
}

pub fn hwnd_vneg(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (_, typ, src, mut target) = handle_vdata_op!(pickle, builder, meta);

  let clif = typ.clif_mapping();

  src.into_iter().enumerate().for_each(|(idx, src)| {
    let val = if clif.float {
      builder.ins().fneg(src)
    } else if clif.signed {
      builder.ins().ineg(src)
    } else {
      src
    };
    target.store(builder, idx, val);
  });

  target.synchronize(builder, meta);
}
