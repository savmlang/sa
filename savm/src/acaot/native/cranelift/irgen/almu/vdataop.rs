use cranelift::{
  codegen::ir::Value,
  prelude::{FunctionBuilder, InstBuilder},
};

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{
      StoreResolver, TypeOrWidth, resolve_location_src_load, resolve_location_src_store,
    },
  },
  pickle::{
    def::PickleInstruction,
    reader::vfop::{VDATAOP, parse_vdataop},
  },
};

fn handle_vdata_op(
  pickle: &PickleInstruction,
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
) -> (TypeOrWidth, Box<[Value]>, StoreResolver) {
  let VDATAOP {
    datatype,
    count,
    src1,
    of_src1,
    tgt,
    of_tgt,
  } = parse_vdataop(pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Type(datatype);

  let src1 = { resolve_location_src_load(builder, meta, typ, src1 as u8, None, of_src1, count) };

  let target = { resolve_location_src_store(builder, meta, typ, tgt as u8, None, of_tgt, count) };

  (typ, src1, target)
}

pub fn hwnd_vabs(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let (typ, src, mut target) = handle_vdata_op(&pickle, builder, meta);

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
  let (typ, src, mut target) = handle_vdata_op(&pickle, builder, meta);

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
