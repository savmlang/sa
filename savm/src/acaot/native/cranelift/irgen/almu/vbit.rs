use cranelift::prelude::{FloatCC, FunctionBuilder, InstBuilder, IntCC, types::I64};

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

pub fn hwnd_vbit(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let count = pickle.u3;

  let op = count >> 4;

  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let width = (flags >> 14) as u8;
  let typ = TypeOrWidth::Width(width);

  let count = {
    let countdata = readws!(meta, start = 0, stop = 4, u32);

    countdata
  };

  let src1 = {
    let src = (flags as u8) & 0x0F;
    let of = readws!(meta, start = 4, stop = 8, i32);

    resolve_location_src_load(builder, meta, typ, src, None, of, count)
  };
  let src2 = {
    let src = (flags as u8) >> 4 & 0x0F;
    let of = readws!(meta, start = 8, stop = 12, i32);

    resolve_location_src_load(builder, meta, typ, src, None, of, count)
  };

  let mut target = {
    let src = (flags >> 12) as u8 & 0x0F;
    let of = readws!(meta, start = 12, stop = 16, i32);

    resolve_location_src_store(builder, meta, typ, src, None, of, count)
  };

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .map(|x| (x.0, x.1.0, x.1.1))
    .for_each(|(idx, src1, src2)| {
      let ins = builder.ins();
      let output = match op {
        0 => ins.band(src1, src2),
        1 => ins.bor(src1, src2),
        2 => ins.bxor(src1, src2),
        3 => ins.bnot(src1),
        4 => ins.bor_not(src1, src2),
        5 => ins.band_not(src1, src2),
        6 => ins.bxor_not(src1, src2),
        7 => ins.bitrev(src1),
        8 => ins.bswap(src1),
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
  let rot = pickle.u3;

  let op = rot & 0x01;

  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let typ = (flags >> 12) as u8;
  let typ = TypeOrWidth::Type(typ);

  let count = {
    let countdata = readws!(meta, start = 0, stop = 4, u32);

    countdata
  };

  let src1 = {
    let src = (flags as u8) & 0x0F;
    let of = readws!(meta, start = 4, stop = 8, i32);

    resolve_location_src_load(builder, meta, typ, src, None, of, count)
  };
  let src2 = {
    let src = (flags as u8) >> 4 & 0x0F;
    let of = readws!(meta, start = 8, stop = 12, i32);

    resolve_location_src_load(builder, meta, typ, src, None, of, count)
  };

  let mut target = {
    let src = (flags >> 12) as u8 & 0x0F;
    let of = readws!(meta, start = 12, stop = 16, i32);

    resolve_location_src_store(builder, meta, typ, src, None, of, count)
  };

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
