#![allow(unused_unsafe)]

use std::{
  collections::HashMap,
  hint::cold_path,
  mem::{transmute_copy, zeroed},
  ptr::read_unaligned,
  sync::Arc,
};

mod almu;
pub use almu::*;

mod threading;
pub use threading::*;

use sart::{
  ctr::{AggressiveMatrixExtension, VMTaskState},
  salloc,
  structures::QuadPackedData,
};

use crate::acaot::pickle::def::{
  PICKLE_DISPATCH_TABLE, PICKLE_OPCODE_JIF, PICKLE_OPCODE_JMP, PICKLE_OPCODE_MARK,
  PICKLE_OPCODE_VADD, PICKLE_OPCODE_VCMP, PickleInstruction,
};

pub const SIZE_128KB: usize = 128 * 1024 / size_of::<QuadPackedData>();

pub struct WorkingSet {
  pub arr: &'static [u8],
  pub largepad: *mut QuadPackedData, // SIZE_128KB allocated
  pub largepad_cursor: usize,
  pub relocmap: Arc<HashMap<u64, usize, ahash::RandomState>>,

  // AME
  pub ame: *mut AggressiveMatrixExtension,
  pub ame_free: bool,

  // Branch Predictor
  pub jmp: (u64, usize),
}

// the largepad is a LIFO-ONLY Queue
// Anything lese is UB
impl WorkingSet {
  pub fn getame(&mut self) -> *mut AggressiveMatrixExtension {
    let allocame = || unsafe {
      salloc::aligned_malloc(
        size_of::<AggressiveMatrixExtension>(),
        align_of::<AggressiveMatrixExtension>(),
      ) as *mut AggressiveMatrixExtension
    };

    if self.ame_free {
      self.ame_free = false;

      if self.ame.is_null() {
        self.ame = allocame();
      }

      return self.ame;
    }

    allocame()
  }

  pub fn freeame(&mut self, ame: *mut AggressiveMatrixExtension) {
    if self.ame == ame {
      self.ame_free = true;
      return;
    }

    unsafe { salloc::aligned_free(ame as _) };
  }

  pub fn allocate(&mut self, size: usize, align: usize) -> *mut QuadPackedData {
    // FAST PATH: Specific Alignment (No Header)
    // We skip the req_size calculation entirely here.
    if align != 0 {
      return unsafe {
        sart::salloc::aligned_malloc(size * size_of::<QuadPackedData>(), align) as _
      };
    }

    // Standard
    // They are happy with any alignment
    let req_size = size as usize + 1; // 1x 64-bit for header reasons

    // Cursor runout
    let Some(new_cursor) = self.largepad_cursor.checked_add(req_size) else {
      return self.sallocate_fallback(req_size);
    };

    // Overflow!
    if new_cursor > SIZE_128KB {
      return self.sallocate_fallback(req_size);
    }

    // Standard, add a header
    unsafe {
      let newptr = self.largepad.add(self.largepad_cursor);
      *newptr = QuadPackedData { u64: req_size as _ };

      self.largepad_cursor = new_cursor;

      return newptr.add(1);
    }
  }

  fn sallocate_fallback(&self, req_size: usize) -> *mut QuadPackedData {
    unsafe {
      let out = sart::salloc::aligned_malloc(
        req_size * size_of::<QuadPackedData>(),
        align_of::<QuadPackedData>(), // Natural alignment for 64-bit
      ) as *mut QuadPackedData;

      if out.is_null() {
        return std::ptr::null_mut();
      }
      (*out).u64 = 0;

      out.add(1)
    }
  }

  pub fn salloc_free(&self, ptr: *mut QuadPackedData) {
    unsafe {
      sart::salloc::aligned_free(ptr as _);
    }
  }

  pub fn free(&mut self, ptr: *mut QuadPackedData) {
    // Look at header
    unsafe {
      let header = ptr.wrapping_sub(1);

      let length_of_ptr = (*header).u64;

      if length_of_ptr == 0 {
        return self.salloc_free(ptr.wrapping_sub(1));
      }

      self.largepad_cursor = self.largepad_cursor.wrapping_sub(length_of_ptr as usize);
    }
  }
}

#[macro_export]
macro_rules! resolve {
  ($task:ident => $x:ident) => {
    unsafe {
      match $x {
        0 => (*$task).r1,
        1 => (*$task).r2,
        2 => (*$task).r3,
        3 => (*$task).r4,
        4 => (*$task).r5,
        5 => (*$task).r6,
        6 => (*$task).r7,
        7 => (*$task).r8,
        _ => unimplemented!(),
      }
    }
  };
}

#[macro_export]
macro_rules! resolve_ptr {
  ($task:ident => $x:ident) => {
    unsafe {
      match $x {
        0 => std::ptr::addr_of_mut!((*$task).r1),
        1 => std::ptr::addr_of_mut!((*$task).r2),
        2 => std::ptr::addr_of_mut!((*$task).r3),
        3 => std::ptr::addr_of_mut!((*$task).r4),
        4 => std::ptr::addr_of_mut!((*$task).r5),
        5 => std::ptr::addr_of_mut!((*$task).r6),
        6 => std::ptr::addr_of_mut!((*$task).r7),
        7 => std::ptr::addr_of_mut!((*$task).r8),
        _ => unimplemented!(),
      }
    }
  };
}

#[macro_export]
macro_rules! resolve_location_src {
  ($task:ident => $x:ident $($e:ident)?) => {
    unsafe { match $x {
      0 => std::ptr::addr_of_mut!((*$task).r1),
      1 => std::ptr::addr_of_mut!((*$task).r2),
      2 => std::ptr::addr_of_mut!((*$task).r3),
      3 => std::ptr::addr_of_mut!((*$task).r4),
      4 => std::ptr::addr_of_mut!((*$task).r5),
      5 => std::ptr::addr_of_mut!((*$task).r6),
      6 => std::ptr::addr_of_mut!((*$task).r7),
      7 => std::ptr::addr_of_mut!((*$task).r8),
      8 => (*$task).scratchpad,
      9 => (*$task).largepad,
      #[allow(unused_unsafe)]
      10 => unsafe { (*$task).r2.selfref },
      $(
        _con => $e,
      )?
      #[allow(unreachable_patterns)]
      _ => unimplemented!(),
    }}
  };
}

pub type ResolveFn =
  fn(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) -> ();

#[inline(always)]
pub fn call_hint(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  unsafe {
    let instruction = pickle.u1;

    let total_wsput = pickle.u2 as usize;
    let bytes = pickle.u3 as usize;

    let pic = { (*taskstate).curline_or_resume.usi };

    // Fetch WS_PUTs and decode
    (*ws).arr = {
      std::slice::from_raw_parts(
        ((*taskstate).engine_or_pt.pt as *const PickleInstruction).add(pic + 1) as *const u8,
        bytes,
      )
    };

    // Increment counter by that exact amount
    // total_wsput
    //
    // +1 to go past the last WS_PUT
    (*taskstate).curline_or_resume.usi = pic + total_wsput + 1;

    // Call next instruction
    {
      let pkl = &*((*taskstate).engine_or_pt.pt as *const PickleInstruction)
        .add((*taskstate).curline_or_resume.usi);

      debug_assert!(pkl.opcode == instruction);

      // TODO: Replace with `become` once its in nightly-functional
      match instruction {
        // These calls are infact inlined
        PICKLE_OPCODE_MARK => call_mark(pkl, ws, taskstate),
        PICKLE_OPCODE_JMP => call_jmp(pkl, ws, taskstate),
        PICKLE_OPCODE_JIF => call_jif(pkl, ws, taskstate),
        PICKLE_OPCODE_VCMP => call_vcmp(pkl, ws, taskstate),
        PICKLE_OPCODE_VADD => call_vadd(pkl, ws, taskstate),
        _ => return PICKLE_DISPATCH_TABLE.get_unchecked(instruction as usize)(pkl, ws, taskstate),
      }
    }
  }
}

#[inline(always)]
pub fn call_mark(_pickle: &PickleInstruction, _ws: *mut WorkingSet, _taskstate: *mut VMTaskState) {}

#[inline(always)]
pub fn call_ws_put(
  _pickle: &PickleInstruction,
  _ws: *mut WorkingSet,
  _taskstate: *mut VMTaskState,
) {
  panic!("WS_PUT is not to be called");
  // let offset = pickle.u1 as usize;

  // unsafe {
  //   *unsafe { (*ws).arr }.get_unchecked_mut(offset * 2) = pickle.u2;
  //   *unsafe { (*ws).arr }.get_unchecked_mut(offset * 2 + 1) = pickle.u3;
  // }
}

#[inline(always)]
pub fn call_mov(pickle: &PickleInstruction, _ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let source = pickle.u1;
  let target = pickle.u2;

  if source == target {
    cold_path();

    match source {
      12 => unsafe {
        (*taskstate).r1.selfref = (*taskstate).largepad;
      },
      13 => {
        // Get pointer to global state
        todo!("RW Global State isn't yet implemented")
      }
      _ => panic!("source == target but special ids don't match"),
    }
  } else {
    unsafe {
      let rsrc = resolve!(taskstate => source);
      let ptarget = resolve_ptr!(taskstate => target);

      *ptarget = rsrc
    };
  }
}

#[inline(always)]
pub fn call_reg(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let reg = pickle.u1;

  let mut filled = [0u8; 8];
  unsafe { filled[0..8].copy_from_slice(&(&(*ws).arr)[0..8]) };
  let data = u64::from_ne_bytes(filled);

  unsafe { *resolve_ptr!(taskstate => reg) = QuadPackedData { u64: data } };
}

#[inline(always)]
pub fn call_jmp(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let mut filled = [0u8; 8];
  filled[0..6].copy_from_slice(unsafe { &(&(*ws).arr)[0..6] });
  filled[6..8].copy_from_slice(&[pickle.u1, pickle.u2]);
  let data = u64::from_ne_bytes(filled);

  unsafe {
    if (*ws).jmp.0 == data {
      (*taskstate).curline_or_resume.usi = (*ws).jmp.1;
      return;
    }

    let cr = *(*ws).relocmap.get(&data).unwrap_unchecked();

    (*ws).jmp = (data, cr);
    (*taskstate).curline_or_resume.usi = cr;
  }
}

macro_rules! jif_comparison {
  (
    $ts:ident, $rsrc:ident, $off:ident, $wid:ident

    widths {
      $(
        { $d:expr } => $di:ty
      ),*
    }
  ) => {
    unsafe {
      let src = resolve_location_src!($ts => $rsrc) as *mut u8;

      match $wid {
        $(
          $d => {
            std::ptr::read_unaligned((src as *mut $di).offset($off as _)) != 0
          }
        ),*
        _ => panic!("Invalid width")
      }
    }
  };
}

#[inline(always)]
pub fn call_jif(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let intent = pickle.u1;
  let relocation_src = pickle.u2;
  let width = pickle.u3;

  let offset = i32::from_ne_bytes(unsafe { (&(*ws).arr)[0..4].try_into().unwrap_unchecked() });
  let marker = u64::from_ne_bytes(unsafe { (&(*ws).arr)[4..12].try_into().unwrap_unchecked() });

  let not_zero = jif_comparison!(
    taskstate, relocation_src, offset, width

    widths {
      { 0 } => u64,
      { 1 } => u32,
      { 2 } => u16,
      { 3 } => u8
    }
  );

  unsafe {
    if (intent == 0 && !not_zero) || (intent != 0 && not_zero) {
      if (*ws).jmp.0 == marker {
        (*taskstate).curline_or_resume.usi = (*ws).jmp.1;
        return;
      }

      let cr = *(*ws).relocmap.get(&marker).unwrap_unchecked();

      (*ws).jmp = (marker, cr);
      (*taskstate).curline_or_resume.usi = cr;
    }
  }
}

#[macro_export]
macro_rules! arrcastint {
  ($ws:ident, start = $start:expr, stop = $stop:expr, $i:ty) => {{
    #[allow(unused_unsafe)]
    <$i>::from_ne_bytes(unsafe { (&(*$ws).arr)[$start..$stop].try_into().unwrap_unchecked() })
  }};
}

#[inline(always)]
pub fn call_vcmp(pickle: &PickleInstruction, ws: *mut WorkingSet, taskstate: *mut VMTaskState) {
  let op = pickle.u1;
  let width = pickle.u2;

  let srcflags = arrcastint!(ws, start = 0, stop = 2, u16);

  let _src1 = (srcflags >> 12) as u8 & 0xF;
  let _src2 = ((srcflags >> 8) & 0xF) as u8;
  let _target = ((srcflags >> 4) & 0xF) as u8;

  let count = arrcastint!(ws, start = 2, stop = 6, u32);

  let offset1 = arrcastint!(ws, start = 6, stop = 10, i32);
  let offset2 = arrcastint!(ws, start = 10, stop = 14, i32);
  let offset3 = arrcastint!(ws, start = 14, stop = 18, i32);

  let src1 = { resolve_location_src!(taskstate => _src1) };
  let src2 = { resolve_location_src!(taskstate => _src2) };
  let target = { resolve_location_src!(taskstate => _target) };

  // We're assuming vectored, as there's no issues, haha
  // Also, its easier to downref to u32, u16...
  let successval = if count > 1 { !0u64 } else { 1u64 };

  // integral
  let innercmp: unsafe fn(
    u8,
    *const QuadPackedData,
    *const QuadPackedData,
    *mut QuadPackedData,
    u64,
    i32,
    i32,
    i32,
  ) = if op <= 9 {
    let is_signed = [2, 4, 6, 8].iter().any(|o| op == *o);

    match (is_signed, width) {
      (true, 0) => vcmp_inner::<i64>,
      (true, 1) => vcmp_inner::<i32>,
      (true, 2) => vcmp_inner::<i16>,
      (true, 3) => vcmp_inner::<i8>,
      (false, 0) => vcmp_inner::<u64>,
      (false, 1) => vcmp_inner::<u32>,
      (false, 2) => vcmp_inner::<u16>,
      (false, 3) => vcmp_inner::<u8>,
      _ => panic!(),
    }
  } else {
    match width {
      1 => vcmp_f_inner::<f32, i32>,
      0 => vcmp_f_inner::<f64, i64>,
      _ => panic!(),
    }
  };

  unsafe {
    for additive in 0..count {
      innercmp(
        op,
        src1.add(additive as _),
        src2.add(additive as _),
        target.add(additive as _),
        successval,
        offset1,
        offset2,
        offset3,
      );
    }
  }
}

// A helper for the inner loop logic
unsafe fn vcmp_inner<T>(
  op: u8,
  s1: *const QuadPackedData,
  s2: *const QuadPackedData,
  t: *mut QuadPackedData,
  success: u64,
  offset1: i32,
  offset2: i32,
  offset3: i32,
) where
  T: Copy + PartialEq + PartialOrd + 'static,
{
  unsafe {
    let v1 = read_unaligned((s1 as *mut T).offset(offset1 as _));
    let v2 = read_unaligned((s2 as *mut T).offset(offset2 as _));

    // We are treating signed and unsigned as same ONLY
    // because we prune sign earlier
    let cond = match op {
      0 => v1 == v2,
      1 => v1 != v2,
      2 | 3 => v1 < v2,
      4 | 5 => v1 <= v2,
      6 | 7 => v1 > v2,
      8 | 9 => v1 >= v2,
      _ => false,
    };

    let val = if cond {
      transmute_copy(&success)
    } else {
      zeroed()
    };
    (t as *mut T).offset(offset3 as _).write_unaligned(val);
  }
}

trait Float {
  fn nan(&self) -> bool;
}

impl Float for f32 {
  fn nan(&self) -> bool {
    self.is_nan()
  }
}

impl Float for f64 {
  fn nan(&self) -> bool {
    self.is_nan()
  }
}

unsafe fn vcmp_f_inner<T, E>(
  op: u8,
  s1: *const QuadPackedData,
  s2: *const QuadPackedData,
  t: *mut QuadPackedData,
  success: u64,
  offset1: i32,
  offset2: i32,
  offset3: i32,
) where
  T: Copy + Float + PartialEq + PartialOrd + 'static,
{
  unsafe {
    assert!(size_of::<T>() == size_of::<E>());
    assert!(align_of::<T>() == align_of::<E>());

    let v1 = read_unaligned((s1 as *mut T).offset(offset1 as _));
    let v2 = read_unaligned((s2 as *mut T).offset(offset2 as _));

    let un = v1.nan() || v2.nan();
    let eq = v1 == v2;
    let lt = v1 < v2;
    let gt = v1 > v2;

    // We are treating signed and unsigned as same ONLY
    // because we prune sign earlier
    let cond = match op {
      10 => eq || lt || gt,
      11 => un,
      12 => eq,
      13 => un || lt || gt,
      14 => lt || gt,
      15 => un || eq,
      16 => lt,
      17 => lt || eq,
      18 => gt,
      19 => gt || eq,
      20 => un || lt,
      21 => un || lt || eq,
      22 => un || gt,
      23 => un || gt || eq,
      _ => false,
    };

    let val = if cond {
      transmute_copy(&success)
    } else {
      zeroed()
    };
    ((t as *mut T).offset(offset3 as _) as *mut E).write_unaligned(val);
  }
}
