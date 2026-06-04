use crate::{acaot::pickle::def::PickleInstruction, wspickle};

pub struct VFP {
  pub instdef: u8,
  pub count: u32,

  pub datatype: u8,

  pub src1: u8,
  pub src2: u8,
  pub tgt: u8,
  pub of_src1: i32,
  pub of_src2: i32,
  pub of_tgt: i32,
}

pub fn parse_vfp(pickle: &PickleInstruction, meta: &[u8]) -> VFP {
  let f1 = pickle.u1;
  let f2 = pickle.u2;

  let flags = u16::from_ne_bytes([f1, f2]);

  let fptype = ((flags >> 12) & 0x01) as u8;

  let datatype = match fptype {
    0 => 8,
    1 => 9,
    _ => unreachable!(),
  };

  let instdef = ((flags >> 14) & 0x01) as u8;

  let count = wspickle!(meta, start = 0, stop = 4, u32);

  let of_src1 = wspickle!(meta, start = 4, stop = 8, i32);
  let of_src2 = wspickle!(meta, start = 8, stop = 12, i32);
  let of_tgt = wspickle!(meta, start = 12, stop = 16, i32);

  let src1 = {
    let src = (flags >> 8 as u8) & 0x0F;

    src as u8
  };

  let src2 = {
    let src = (flags as u8) >> 4;

    src as u8
  };

  let tgt = {
    let src = (flags as u8) & 0x0F;

    src as u8
  };

  VFP {
    instdef,
    count,
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  }
}

pub struct VFMA {
  pub datatype: u8,
  pub count: u32,

  pub src1: u8,
  pub of_src1: i32,

  pub src2: u8,
  pub of_src2: i32,

  pub src3: u8,
  pub of_src3: i32,

  pub tgt: u8,
  pub of_tgt: i32,
}

pub fn parse_vfma(pickle: &PickleInstruction, meta: &[u8]) -> VFMA {
  let f1 = pickle.u1;
  let f2 = pickle.u2;

  let flags = u16::from_ne_bytes([f1, f2]);

  let fptype = ((flags >> 12) & 0x01) as u8;

  let datatype = match fptype {
    0 => 8,
    1 => 9,
    _ => unreachable!(),
  };

  let count = wspickle!(meta, start = 0, stop = 4, u32);
  let of_src1 = wspickle!(meta, start = 4, stop = 8, i32);
  let of_src2 = wspickle!(meta, start = 8, stop = 12, i32);
  let of_src3 = wspickle!(meta, start = 12, stop = 16, i32);
  let of_tgt = wspickle!(meta, start = 16, stop = 20, i32);

  let src1 = {
    let src = (flags >> 12) & 0x0F;

    src as u8
  };

  let src2 = {
    let src = (flags >> 8) & 0x0F;

    src as u8
  };

  let src3 = {
    let src = (flags as u8) >> 4;

    src as u8
  };

  let tgt = {
    let src = (flags as u8) & 0x0F;

    src as u8
  };

  VFMA {
    datatype,
    count,
    src1,
    of_src1,
    src2,
    of_src2,
    src3,
    of_src3,
    tgt,
    of_tgt,
  }
}
