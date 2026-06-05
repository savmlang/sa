use crate::{acaot::pickle::def::PickleInstruction, constdef, wspickle};

pub struct VDATAOP {
  pub datatype: u8,
  pub count: u32,

  pub src1: u8,
  pub of_src1: i32,

  pub tgt: u8,
  pub of_tgt: i32,
}

pub fn parse_vdataop(pickle: &PickleInstruction, meta: &[u8]) -> VDATAOP {
  let f1 = pickle.u1;
  let f2 = pickle.u2;
  let flags = u16::from_ne_bytes([f1, f2]);

  let datatype = (flags >> 12) as u8;
  let count = wspickle!(meta, start = 0, stop = 4, u32);
  let of_src1 = wspickle!(meta, start = 4, stop = 8, i32);
  let of_tgt = wspickle!(meta, start = 12, stop = 16, i32);

  let src1 = {
    let src = (flags >> 8) as u8 & 0x0F;

    src
  };

  let tgt = {
    let src = (flags as u8) >> 4;

    src
  };

  VDATAOP {
    datatype,
    count,
    src1,
    of_src1,
    tgt,
    of_tgt,
  }
}

pub struct VFOP {
  pub src: u8,
  pub target: u8,

  pub subop: u8,

  pub offset_src: i32,
  pub offset_target: i32,

  pub count: u32,

  pub typetag: u8,
}

constdef! {
  u8
  {
    FOP_CEIL 0,
    FOP_FLOOR 1,
    FOP_TRUNC 2,
    FOP_ROUND 3,
    FOP_SQRT 4
  }
}

pub fn parse_vfop(pickle: &PickleInstruction, ws: &[u8]) -> VFOP {
  let flags = u16::from_le_bytes([pickle.u1, pickle.u2]);

  let count = wspickle!(ws, start = 0, stop = 4, u32);

  let offset_src = wspickle!(ws, start = 4, stop = 8, i32);
  let offset_target = wspickle!(ws, start = 8, stop = 12, i32);

  let subop = (flags as u8) & 0x7;

  let target = (flags as u8) >> 4;
  let src = (flags >> 8) as u8 & 0xF;

  let float_type = match ((flags >> 3) as u8) & 0x1 {
    0 => 8,
    1 => 9,
    _ => unimplemented!(),
  };

  VFOP {
    src,
    target,
    offset_src,
    offset_target,
    count,
    typetag: float_type,
    subop,
  }
}
