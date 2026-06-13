use cranelift::prelude::{FunctionBuilder, InstBuilder};

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{TypeOrWidth, resolve_location_src_load, resolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::vbit::{
      VBIT, VBIT_BAND, VBIT_BAND_NOT, VBIT_BITREV, VBIT_BITSWAP, VBIT_BOR, VBIT_BOR_NOT, VBIT_BXOR,
      VBIT_BXOR_NOT, VBIT_NOT, VROT, parse_vbit, parse_vrot,
    },
  },
};

pub fn hwnd_vbit(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let VBIT {
    count,
    op,
    width,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_vbit(&pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Width(width);

  let src1 = { resolve_location_src_load(builder, meta, typ, src1, None, of_src1, count) };
  let src2 = { resolve_location_src_load(builder, meta, typ, src2, None, of_src2, count) };

  let mut target = { resolve_location_src_store(builder, meta, typ, tgt, None, of_tgt, count) };

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .map(|x| (x.0, x.1.0, x.1.1))
    .for_each(|(idx, src1, src2)| {
      let ins = builder.ins();
      let output = match op {
        VBIT_BAND => ins.band(src1, src2),
        VBIT_BOR => ins.bor(src1, src2),
        VBIT_BXOR => ins.bxor(src1, src2),
        VBIT_NOT => ins.bnot(src1),
        VBIT_BOR_NOT => ins.bor_not(src1, src2),
        VBIT_BAND_NOT => ins.band_not(src1, src2),
        VBIT_BXOR_NOT => ins.bxor_not(src1, src2),
        VBIT_BITREV => ins.bitrev(src1),
        VBIT_BITSWAP => ins.bswap(src1),
        e => unimplemented!("Unknown op : {e}"),
      };

      target.store(builder, idx, output);
    });

  target.synchronize(builder, meta);
}

pub fn hwnd_vrot(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let VROT {
    count,
    op,
    typetag,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_vrot(&pickle, meta.ws.as_ref());

  let typ = TypeOrWidth::Type(typetag);

  let src1 = { resolve_location_src_load(builder, meta, typ, src1, None, of_src1, count) };
  let src2 = { resolve_location_src_load(builder, meta, typ, src2, None, of_src2, count) };

  let mut target = { resolve_location_src_store(builder, meta, typ, tgt, None, of_tgt, count) };

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .map(|x| (x.0, x.1.0, x.1.1))
    .for_each(|(idx, src1, src2)| {
      let ins = builder.ins();
      let output = match op {
        0 => ins.rotl(src1, src2),
        1 => ins.rotr(src1, src2),
        e => unimplemented!("Unknown op : {e}"),
      };

      target.store(builder, idx, output);
    });

  target.synchronize(builder, meta);
}
