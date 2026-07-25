use crate::{acaot::pickle::def::PickleInstruction, wspickle};

pub struct SPAWN {
  pub section: u64,
  pub return_hwnd: bool,

  pub out_loc: u8,
}

pub fn parse_spawn(pickle: &PickleInstruction, ws: &[u8]) -> SPAWN {
  let section = wspickle!(ws, start = 0, stop = 8, u64);

  let hwnd = (pickle.u1 & 0x1) > 0;
  let taskout = (pickle.u1 >> 1) & 0xF;

  SPAWN {
    section,
    out_loc: taskout,
    return_hwnd: hwnd,
  }
}
