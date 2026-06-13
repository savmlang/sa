use crate::{acaot::pickle::def::PickleInstruction, constdef, wspickle};

pub struct VBIT {
  pub count: u32,
  pub op: u8,
  pub width: u8,

  pub src1: u8,
  pub of_src1: i32,

  pub src2: u8,
  pub of_src2: i32,

  pub tgt: u8,
  pub of_tgt: i32,
}

constdef! {
  u8

  {
    VBIT_BAND 0,
    VBIT_BOR 1,
    VBIT_BXOR 2,
    VBIT_NOT 3,
    VBIT_BOR_NOT 4,
    VBIT_BAND_NOT 5,
    VBIT_BXOR_NOT 6,
    VBIT_BITREV 7,
    VBIT_BITSWAP 8
  }
}

pub fn parse_vbit(pickle: &PickleInstruction, meta: &[u8]) -> VBIT {
  let count = pickle.u3;

  let op = count >> 4;

  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let width = (flags >> 14) as u8;

  let count = {
    let countdata = wspickle!(meta, start = 0, stop = 4, u32);

    countdata
  };

  let (src1, of_src1) = {
    let src = (flags as u8) & 0x0F;
    let of = wspickle!(meta, start = 4, stop = 8, i32);
    (src, of)
  };
  let (src2, of_src2) = {
    let src = (flags as u8) >> 4 & 0x0F;
    let of = wspickle!(meta, start = 8, stop = 12, i32);

    (src, of)
  };

  let (tgt, of_tgt) = {
    let src = (flags >> 12) as u8 & 0x0F;
    let of = wspickle!(meta, start = 12, stop = 16, i32);

    (src, of)
  };

  VBIT {
    count,
    op,
    width,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  }
}

pub struct VROT {
  pub count: u32,
  pub op: u8,
  pub typetag: u8,

  pub src1: u8,
  pub of_src1: i32,

  pub src2: u8,
  pub of_src2: i32,

  pub tgt: u8,
  pub of_tgt: i32,
}

constdef! {
  u8

  {
    VROT_L 0,
    VROT_R 1
  }
}

pub fn parse_vrot(pickle: &PickleInstruction, meta: &[u8]) -> VROT {
  let rot = pickle.u3;

  let op = rot & 0x01;

  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let typetag = (flags >> 12) as u8;

  let count = {
    let countdata = wspickle!(meta, start = 0, stop = 4, u32);

    countdata
  };

  let (src1, of_src1) = {
    let src = (flags as u8) & 0x0F;
    let of = wspickle!(meta, start = 4, stop = 8, i32);

    (src, of)
  };
  let (src2, of_src2) = {
    let src = (flags as u8) >> 4 & 0x0F;
    let of = wspickle!(meta, start = 8, stop = 12, i32);

    (src, of)
  };

  let (tgt, of_tgt) = {
    let src = (flags >> 12) as u8 & 0x0F;
    let of = wspickle!(meta, start = 12, stop = 16, i32);

    (src, of)
  };

  VROT {
    count,
    op,
    typetag,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  }
}
