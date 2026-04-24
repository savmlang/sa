use crate::{acaot::pickle::def::PickleInstruction, wspickle};

pub struct VSH {
  pub op: u8,
  pub flags_src1: u8,
  pub flags_src2: u8,
  pub flags_target: u8,
  pub count: u32,
  pub of_src1: i8,
  pub of_src2: i8,
  pub of_target: i8,
  pub typ: u8,
}

#[inline(always)]
pub fn parse_vsh(pickle: &PickleInstruction, ws: &[u8]) -> VSH {
  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let typ = (flags >> 13) as u8;
  let op = (flags >> 12) as u8 & 0x01;

  let count = wspickle!(ws, start = 0, stop = 4, u32);

  let flags_src1 = (flags as u8) & 0x0F;
  let flags_src2 = (flags as u8) >> 4 & 0x0F;
  let flags_target = (flags >> 12) as u8 & 0x0F;

  let of_src1 = wspickle!(ws, start = 4, stop = 5, i8);
  let of_src2 = wspickle!(ws, start = 5, stop = 6, i8);
  let of_target = wspickle!(ws, start = 6, stop = 7, i8);

  VSH {
    op,
    flags_src1,
    flags_src2,
    flags_target,
    count,
    of_src1,
    of_src2,
    of_target,
    typ,
  }
}
