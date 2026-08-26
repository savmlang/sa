use crate::{acaot::pickle::def::PickleInstruction, wspickle};

pub struct VMINIMAX {
  pub op: u8,
  pub flags_src1: u8,
  pub flags_src2: u8,
  pub flags_target: u8,
  pub count: u32,
  pub of_src1: i8,
  pub of_src2: i8,
  pub of_target: i8,
  pub typ: u8,
  pub alignment_src1: Option<u8>,
  pub alignment_src2: Option<u8>,
  pub alignment_target: Option<u8>,
}

#[inline(always)]
pub fn parse_vminimax(pickle: &PickleInstruction, ws: &[u8]) -> VMINIMAX {
  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
  let op = pickle.u3 & 0x01;

  let typ = (flags >> 12) as u8;

  let align_target = pickle.u3 >> 1 as u8 & 0x03;
  let align_src2 = pickle.u3 >> 3 as u8 & 0x03;
  let align_src1 = pickle.u3 >> 5 as u8 & 0x03;

  let align = |align: u8| match align {
    1 => Some(16),
    2 => Some(32),
    3 => Some(64),
    _ => None,
  };

  let count = {
    let countdata = wspickle!(ws, start = 0, stop = 4, u32);

    countdata
  };

  let flags_src1 = (flags >> 8) as u8 & 0x0F;
  let flags_src2 = (flags >> 4) as u8 & 0x0F;
  let flags_target = flags as u8 & 0x0F;

  let of_src1 = wspickle!(ws, start = 4, stop = 5, i8);
  let of_src2 = wspickle!(ws, start = 5, stop = 6, i8);
  let of_target = wspickle!(ws, start = 6, stop = 7, i8);

  VMINIMAX {
    op,
    flags_src1,
    flags_src2,
    flags_target,
    count,
    of_src1,
    of_src2,
    of_target,
    typ,
    alignment_src1: align(align_src1),
    alignment_src2: align(align_src2),
    alignment_target: align(align_target),
  }
}

pub struct VCNT {
  pub op: u8,
  pub flags_src: u8,
  pub flags_target: u8,
  pub count: u32,
  pub of_src: i8,
  pub of_target: i8,
  pub typ: u8,
  pub alignment_src: Option<u8>,
  pub alignment_target: Option<u8>,
}

#[inline(always)]
pub fn parse_vcnt(pickle: &PickleInstruction, ws: &[u8]) -> VCNT {
  let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let op = (flags as u8) & 0x0F;
  let typ = (flags >> 12) as u8 & 0x03;
  let count = {
    let countdata = wspickle!(ws, start = 0, stop = 4, u32);

    countdata
  };

  let align_target = pickle.u3 as u8 & 0x03;
  let align_src = pickle.u3 >> 2 as u8 & 0x03;

  let align = |align: u8| match align {
    1 => Some(16),
    2 => Some(32),
    3 => Some(64),
    _ => None,
  };

  let flags_src = (flags >> 8) as u8 & 0x0F;
  let flags_target = (flags >> 4) as u8 & 0x0F;

  let of_src = wspickle!(ws, start = 4, stop = 5, i8);
  let of_target = wspickle!(ws, start = 5, stop = 6, i8);

  VCNT {
    op,
    flags_src,
    flags_target,
    count,
    of_src,
    of_target,
    typ,
    alignment_src: align(align_src),
    alignment_target: align(align_target),
  }
}
