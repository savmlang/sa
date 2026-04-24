use std::{
  mem::offset_of,
  ptr::{self, null_mut},
};

use sart::{ctr::VMTaskState, structures::QuadPackedData};

use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  wspickle,
};

#[derive(Clone, Copy)]
pub struct VCOPY {
  pub src: u8,
  pub target: u8,

  pub count: Count,

  pub src_offset: i32,
  pub target_offset: i32,

  pub overlapping: bool,
  pub src_align: u8,
  pub target_align: u8,
}

#[derive(Clone, Copy)]
pub enum Count {
  Runtime,
  Abs(u32),
}

impl Count {
  pub fn is_runtime(self) -> bool {
    matches!(self, Self::Runtime)
  }

  pub fn get(self, ts: *mut VMTaskState) -> u32 {
    match self {
      Self::Runtime => unsafe { ptr::read(ts.add(offset_of!(VMTaskState, r1)) as *mut u32) },
      Self::Abs(count) => count,
    }
  }
}

fn alignment(flags: u8) -> u8 {
  match flags {
    0 => 1,
    1 => 16,
    2 => 32,
    3 => 64,
    _ => unreachable!(),
  }
}

pub fn parse_vcopy(pickle: &PickleInstruction, ws: &[u8]) -> VCOPY {
  let memflags = pickle.u1;
  let srcflags = pickle.u2;

  let memory_flags = memflags & 0x7F;

  let target_align = alignment(memory_flags & 0x03);
  let src_align = alignment((memory_flags >> 2) & 0x03);
  let overlapping = ((memory_flags & 0x10) > 0);

  let countbit = memflags & 0x80;

  let src = srcflags >> 4;
  let target = srcflags & 0x0F;

  let count = if countbit > 0 {
    Count::Runtime
  } else {
    Count::Abs(wspickle!(ws, start = 0, stop = 4, u32))
  };

  let src_offset = wspickle!(ws, start = 4, stop = 8, i32);
  let target_offset = wspickle!(ws, start = 8, stop = 12, i32);

  VCOPY {
    src,
    target,
    count,
    src_offset,
    target_offset,
    src_align,
    target_align,
    overlapping,
  }
}

pub(crate) extern "C" fn jitcall_vcopy_noalias(src: *mut u8, target: *mut u8, count: u32) {
  unsafe { ptr::copy_nonoverlapping(src, target, count as _) };
}

pub(crate) extern "C" fn jitcall_vcopy_overlapping(src: *mut u8, target: *mut u8, count: u32) {
  unsafe { ptr::copy(src, target, count as _) };
}

pub enum SCRATCH {
  Allocate { size_reg: u8, align_reg: u8 },
  DropClassic,
  DropAligned,
}

pub fn parse_scratch(pickle: &PickleInstruction, ws: &[u8]) -> SCRATCH {
  let op_class = pickle.u1;

  let payload = u16::from_ne_bytes([pickle.u2, pickle.u3]);

  match op_class {
    0b00 => SCRATCH::Allocate {
      size_reg: payload as u8 >> 4,
      align_reg: payload as u8 & 0x0F,
    },
    0b01 => SCRATCH::DropClassic,
    0b10 => SCRATCH::DropAligned,
    _ => unreachable!(),
  }
}

extern "C" fn scratch_ffi(
  op: u8,
  ws: *mut WorkingSet,
  arg1: *mut QuadPackedData,
  arg2: usize,
) -> *mut QuadPackedData {
  unsafe {
    match op {
      // Alloc
      0 => {
        let size = arg1.addr();
        let align = arg2;

        return WorkingSet::allocate(&mut *ws, size, align);
      }
      1 => {
        WorkingSet::free(&mut *ws, arg1);
      }
      2 => {
        WorkingSet::salloc_free(&mut *ws, arg1);
      }
      _ => unimplemented!(),
    }

    null_mut()
  }
}
