use cranelift::{
  codegen::ir::{
    Endianness, MemFlagsData,
    types::{F32, F64},
  },
  prelude::{FunctionBuilder, InstBuilder},
};

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{TypeOrWidth, resolve_location_src_load, resolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::vcmp::{CMPOp, VCMP, parse_vcmp},
  },
};

pub fn hwnd_vcmp(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let VCMP {
    datawdt,
    cmpop,
    count,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_vcmp(&pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Width(datawdt);

  let src1 = { resolve_location_src_load(builder, meta, typ, src1, None, of_src1, count) };

  let src2 = { resolve_location_src_load(builder, meta, typ, src2, None, of_src2, count) };

  let mut target = { resolve_location_src_store(builder, meta, typ, tgt, None, of_tgt, count) };

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .for_each(|(idx, (src1, src2))| {
      let val = match &cmpop {
        CMPOp::IntOp(i) => builder.ins().icmp(i.to_clir(), src1, src2),
        CMPOp::FloatOp(f) => {
          let fptype = match typ.clif_mapping().width {
            // float
            4 => F32,
            8 => F64,
            _ => unreachable!(),
          };
          let src1 = builder.ins().bitcast(
            fptype,
            MemFlagsData::new().with_endianness(Endianness::Little),
            src1,
          );
          let src2 = builder.ins().bitcast(
            fptype,
            MemFlagsData::new().with_endianness(Endianness::Little),
            src2,
          );
          builder.ins().fcmp(f.to_clir(), src1, src2)
        }
      };

      let newtype = builder.func.dfg.value_type(src1);

      let val = builder.ins().sextend(newtype, val);

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}
