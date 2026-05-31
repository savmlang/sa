use crate::{acaot::pickle::def::PickleInstruction, wspickle};

pub struct DIVLIKE {
  pub datatype: u8,
  pub src1: u8,
  pub of_src1: i32,
  pub src2: u8,
  pub of_src2: i32,
  pub tgt: u8,
  pub of_tgt: i32,
}

pub fn parse_divlike(pickle: &PickleInstruction, ws: &[u8]) -> DIVLIKE {
  let args = u16::from_ne_bytes([pickle.u1, pickle.u2]);

  let typ = (args >> 12) as u8;

  let of_src1 = wspickle!(ws, start = 0, stop = 4, i32);
  let of_src2 = wspickle!(ws, start = 4, stop = 8, i32);
  let of_tgt = wspickle!(ws, start = 8, stop = 12, i32);

  let src1 = {
    let src = (args >> 8) & 0x0F;

    src as u8
  };

  let src2 = {
    let src = (args as u8) >> 4;

    src as u8
  };

  let tgt = {
    let src = (args as u8) & 0x0F;

    src as u8
  };

  DIVLIKE {
    datatype: typ,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  }
}
