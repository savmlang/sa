use cranelift::{
  codegen::ir::condcodes::FloatCC,
  prelude::{FunctionBuilder, InstBuilder},
};

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{
      TypeOrWidth, resolve_location_src_load, resolve_location_src_load_assumedwdt,
      resolve_location_src_store, resolve_location_src_store_assumedwdt,
    },
  },
  pickle::{
    def::PickleInstruction,
    reader::{
      cast::{CAST, VFCAST, parse_cast, parse_vfcast},
      vfop::{FOP_CEIL, FOP_FLOOR, FOP_ROUND, FOP_SQRT, FOP_TRUNC, VFOP, parse_vfop},
      vminimax::{VCNT, VMINIMAX, parse_vcnt, parse_vminimax},
      vsh::{VSH, parse_vsh},
    },
  },
};

pub fn hwnd_vsh(builder: &mut FunctionBuilder, meta: &mut CompilerMeta, pickle: PickleInstruction) {
  let VSH {
    op,
    flags_src1,
    flags_src2,
    flags_target,
    count,
    of_src1,
    of_src2,
    of_target,
    typ,
  } = parse_vsh(&pickle, &meta.ws);

  let typ = TypeOrWidth::Type(typ);

  let src1 = resolve_location_src_load(builder, meta, typ, flags_src1, None, of_src1 as _, count);
  let src2 = resolve_location_src_load(builder, meta, typ, flags_src2, None, of_src2 as _, count);
  let mut target = resolve_location_src_store(
    builder,
    meta,
    typ,
    flags_target,
    None,
    of_target as _,
    count,
  );

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .map(|a| (a.0, a.1.0, a.1.1))
    .for_each(|(idx, src1, src2)| {
      let val = if op == 0 {
        builder.ins().ishl(src1, src2)
      } else {
        if typ.clif_mapping().signed {
          builder.ins().sshr(src1, src2)
        } else {
          builder.ins().ushr(src1, src2)
        }
      };

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}

pub fn hwnd_vminimax(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let VMINIMAX {
    op,
    flags_src1,
    flags_src2,
    flags_target,
    count,
    of_src1,
    of_src2,
    of_target,
    typ,
    alignment_src1,
    alignment_src2,
    alignment_target,
  } = parse_vminimax(&pickle, &meta.ws);

  let typ = TypeOrWidth::Type(typ);

  let src1 = resolve_location_src_load(
    builder,
    meta,
    typ,
    flags_src1,
    alignment_src1,
    of_src1 as _,
    count,
  );
  let src2 = resolve_location_src_load(
    builder,
    meta,
    typ,
    flags_src2,
    alignment_src2,
    of_src2 as _,
    count,
  );
  let mut target = resolve_location_src_store(
    builder,
    meta,
    typ,
    flags_target,
    alignment_target,
    of_target as _,
    count,
  );

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .map(|a| (a.0, a.1.0, a.1.1))
    .for_each(|(idx, src1, src2)| {
      let clif = typ.clif_mapping();
      let val = if clif.float {
        let x_is_nan = builder.ins().fcmp(FloatCC::OrderedNotEqual, src1, src1);
        let y_is_nan = builder.ins().fcmp(FloatCC::OrderedNotEqual, src2, src2);

        let normal = if op == 0 {
          builder.ins().fmin(src1, src2)
        } else {
          builder.ins().fmax(src1, src2)
        };

        let select_x_if_y_nan = builder.ins().select(y_is_nan, src1, normal);
        builder.ins().select(x_is_nan, src2, select_x_if_y_nan)
      } else if clif.signed {
        if op == 0 {
          builder.ins().smin(src1, src2)
        } else {
          builder.ins().smax(src1, src2)
        }
      } else {
        if op == 0 {
          builder.ins().umin(src1, src2)
        } else {
          builder.ins().umax(src1, src2)
        }
      };

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}

pub fn hwnd_vcnt(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let VCNT {
    op,
    flags_src,
    flags_target,
    of_src,
    of_target,
    typ,
    alignment_src,
    alignment_target,
    count,
  } = parse_vcnt(&pickle, &meta.ws);

  let typ = TypeOrWidth::Width(typ);

  // Check if POPCNT
  if op == 0 {
    let src = resolve_location_src_load(
      builder,
      meta,
      typ,
      flags_src,
      alignment_src,
      of_src as _,
      count,
    );
    let mut target = resolve_location_src_store(
      builder,
      meta,
      typ,
      flags_target,
      alignment_target,
      of_target as _,
      count,
    );

    src.into_iter().enumerate().for_each(|(idx, src1)| {
      let val = builder.ins().popcnt(src1);

      target.store(builder, idx, val);
    });

    target.synchronize(builder, meta);
  } else {
    for idx in 0..count {
      let &[src] = resolve_location_src_load(
        builder,
        meta,
        typ,
        flags_src,
        None,
        of_src as i32 + idx as i32,
        1,
      )
      .as_ref() else {
        unreachable!()
      };
      let mut target = resolve_location_src_store(
        builder,
        meta,
        typ,
        flags_target,
        None,
        of_target as i32 + idx as i32,
        1,
      );

      let val = match op {
        1 => builder.ins().clz(src),
        2 => builder.ins().cls(src),
        3 => builder.ins().ctz(src),
        _ => src,
      };

      target.store(builder, 0, val);

      target.synchronize(builder, meta);
    }
  }
}

pub fn hwnd_cast(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let CAST {
    offset_src,
    offset_target,
    src,
    target,
    type_initial,
    type_final,
  } = parse_cast(&pickle, &meta.ws);

  let src_typ = TypeOrWidth::Type(type_initial);
  let &[src_val] =
    resolve_location_src_load(builder, meta, src_typ, src, None, offset_src, 1).as_ref()
  else {
    unreachable!()
  };

  let target_typ = TypeOrWidth::Type(type_final);
  let mut target =
    resolve_location_src_store(builder, meta, target_typ, target, None, offset_target, 1);

  let target_clif = target_typ.clif_mapping();
  let src_clif = src_typ.clif_mapping();

  let out;
  // Both are INTs
  if target_clif.x1.is_int() && src_clif.x1.is_int() {
    if src_clif.width == target_clif.width {
      out = src_val;
    } else if src_clif.width > target_clif.width {
      out = builder.ins().ireduce(target_clif.x1, src_val);
    } else {
      if src_clif.signed {
        out = builder.ins().sextend(target_clif.x1, src_val);
      } else {
        out = builder.ins().uextend(target_clif.x1, src_val);
      }
    }
  }
  // float -> float
  else if target_clif.float && src_clif.float {
    if src_clif.width > target_clif.width {
      out = builder.ins().fdemote(target_clif.x1, src_val);
    } else {
      out = builder.ins().fpromote(target_clif.x1, src_val);
    }
  }
  // int -> float
  else if target_clif.float {
    if src_clif.signed {
      out = builder.ins().fcvt_from_sint(target_clif.x1, src_val);
    } else {
      out = builder.ins().fcvt_from_uint(target_clif.x1, src_val);
    }
  }
  // float -> int
  else {
    if target_clif.signed {
      out = builder.ins().fcvt_to_sint_sat(target_clif.x1, src_val);
    } else {
      out = builder.ins().fcvt_to_uint_sat(target_clif.x1, src_val);
    }
  };

  target.store(builder, 0, out);
  target.synchronize(builder, meta);
}

pub fn hwnd_vfcast(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let VFCAST {
    offset_src,
    offset_target,
    count,
    src,
    target,
    type_initial,
    type_final,
  } = parse_vfcast(&pickle, &meta.ws);

  let src_typ = TypeOrWidth::Type(type_initial);
  let target_typ = TypeOrWidth::Type(type_final);

  let src_clif = src_typ.clif_mapping();
  let target_clif = target_typ.clif_mapping();

  let assumed_width = src_clif.width().max(target_clif.width());

  let src = resolve_location_src_load_assumedwdt(
    builder,
    meta,
    src_typ,
    src,
    None,
    offset_src,
    count,
    Some(assumed_width),
  );

  let mut target = resolve_location_src_store_assumedwdt(
    builder,
    meta,
    target_typ,
    target,
    None,
    offset_target,
    count,
    Some(assumed_width),
  );

  let waterfall = target.waterfall_typerating();
  src.into_iter().enumerate().for_each(|(idx, sval)| {
    let tgt_lane = waterfall[idx];

    // int->float
    let val = if !src_clif.float {
      if src_clif.signed {
        builder.ins().fcvt_from_sint(tgt_lane, sval)
      } else {
        builder.ins().fcvt_from_uint(tgt_lane, sval)
      }
    }
    // float -> int
    else {
      if target_clif.signed {
        builder.ins().fcvt_to_sint_sat(tgt_lane, sval)
      } else {
        builder.ins().fcvt_to_uint_sat(tgt_lane, sval)
      }
    };

    target.store(builder, idx, val);
  });

  target.synchronize(builder, meta);
}

pub fn hwnd_vfop(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let VFOP {
    src,
    target,
    subop,
    offset_src,
    offset_target,
    count,
    typetag,
  } = parse_vfop(&pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Type(typetag);

  let src = resolve_location_src_load(builder, meta, typ, src, None, offset_src, count);
  let mut tgt = resolve_location_src_store(builder, meta, typ, target, None, offset_target, count);

  src.into_iter().enumerate().for_each(|(idx, src)| {
    let ins = builder.ins();

    let val = match subop {
      FOP_CEIL => ins.ceil(src),
      FOP_FLOOR => ins.floor(src),
      FOP_ROUND => ins.nearest(src),
      FOP_SQRT => ins.sqrt(src),
      FOP_TRUNC => ins.trunc(src),
      _ => unreachable!(),
    };

    tgt.store(builder, idx, val);
  });

  tgt.synchronize(builder, meta);
}
