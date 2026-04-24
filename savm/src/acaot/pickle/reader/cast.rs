use crate::{acaot::pickle::def::PickleInstruction, wspickle};

pub struct CAST {
  pub offset_src: i32,
  pub offset_target: i32,

  pub src: u8,
  pub target: u8,

  pub type_initial: u8,
  pub type_final: u8,
}

pub struct VFCAST {
  pub offset_src: i32,
  pub offset_target: i32,

  pub count: u32,

  pub src: u8,
  pub target: u8,

  pub type_initial: u8,
  pub type_final: u8,
}

#[inline(always)]
pub fn parse_cast(pickle: &PickleInstruction, ws: &[u8]) -> CAST {
  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let offset_src = wspickle!(ws, start = 0, stop = 4, i32);
  let offset_target = wspickle!(ws, start = 4, stop = 8, i32);

  let src = (flags as u8) >> 4;

  let target = (flags as u8) & 0x0F;

  let type_initial = (flags >> 12) as u8;
  let type_final = ((flags >> 8) as u8) & 0x0F;

  CAST {
    offset_src,
    offset_target,
    src,
    target,
    type_initial,
    type_final,
  }
}

#[inline(always)]
pub fn parse_vfcast(pickle: &PickleInstruction, ws: &[u8]) -> VFCAST {
  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let count = wspickle!(ws, start = 0, stop = 4, u32);

  let offset_src = wspickle!(ws, start = 4, stop = 8, i32);
  let offset_target = wspickle!(ws, start = 8, stop = 12, i32);

  let src = (flags as u8) >> 4;

  let target = (flags as u8) & 0x0F;

  let type_int = (flags >> 8) as u8 & 0x03;
  let type_float = match (flags >> 9) as u8 & 0x01 {
    0 => 8,
    1 => 9,
    _ => unreachable!(),
  };

  let (type_initial, type_final) = match (flags >> 9) as u8 & 0x01 {
    0 => (type_float, type_int),
    1 => (type_int, type_float),
    _ => unreachable!(),
  };

  VFCAST {
    offset_src,
    offset_target,
    count,
    src,
    target,
    type_initial,
    type_final,
  }
}
