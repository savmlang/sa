use crate::{acaot::pickle::def::PickleInstruction, constdef, wspickle};

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
