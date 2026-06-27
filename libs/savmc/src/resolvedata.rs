use core::slice;
use std::{
  ffi::c_void,
  io::{Error, Read},
};

use crate::{SAVMC_ISlice_Impl, SAVMC_Maybe};

type IResolveData = SAVMC_IResolveData;
type IResolveState = SAVMC_IResolveState;

#[repr(C)]
pub struct SAVMC_IResolveData {
  pub state: SAVMC_IResolveState,

  pub read_ptr: extern "C" fn(*mut IResolveState, *mut u8, usize) -> SAVMC_Maybe<usize>,
  pub free: extern "C" fn(*mut IResolveState),
}

impl IResolveData {
  pub(crate) fn usedata(bytecode: SAVMC_ISlice_Impl<u8>) -> Self {
    Self {
      state: IResolveState {
        fd: IValData8 {
          ptr: bytecode.data as _,
        },
        vw: IValData8 {
          u64: bytecode.len as u64,
        },
        internal1: IValData8 { u64: 0 },
        internal2: IValData8 { u64: 0 },
        internal3: IValData8 { u64: 0 },
      },
      read_ptr,
      free,
    }
  }
}

extern "C" fn read_ptr(
  state: *mut IResolveState,
  buf_ptr: *mut u8,
  len: usize,
) -> SAVMC_Maybe<usize> {
  unsafe {
    let slice = slice::from_raw_parts_mut(buf_ptr, len);

    let data = (*state).fd.ptr as *const u8;
    let len = (*state).vw.u64 as usize;
    let Some(mut data_f) = slice::from_raw_parts(data, len).get((*state).internal1.u64 as usize..)
    else {
      return SAVMC_Maybe::None;
    };

    match data_f.read(slice) {
      Err(_) => SAVMC_Maybe::None,
      Ok(agr) => {
        (*state).internal1.u64 += agr as u64;
        SAVMC_Maybe::Some(agr)
      }
    }
  }
}
extern "C" fn free(_: *mut IResolveState) {}

#[repr(C)]
/// This is a model struct to happily utilize the full
/// 64-bytes, a structure with 64B alignment MAY be passed
///
/// Use it anyhow you want to manage the state
pub struct SAVMC_IResolveState {
  pub fd: IValData8,
  pub vw: IValData8,

  pub internal1: IValData8,
  pub internal2: IValData8,
  pub internal3: IValData8,
}

#[repr(C)]
pub union IValData8 {
  pub i64: i64,
  pub u64: u64,
  pub ptr: *mut c_void,
}

impl Read for IResolveData {
  fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
    match (self.read_ptr)(&mut self.state, buf.as_mut_ptr(), buf.len()) {
      SAVMC_Maybe::Some(e) => Ok(e),
      _ => Err(Error::last_os_error()),
    }
  }
}

impl Drop for IResolveData {
  fn drop(&mut self) {
    (self.free)(&mut self.state);
  }
}

#[repr(C)]
/// A Streamed data structure
///
/// It may stream both `borrowed` or `owned` datastructure.
/// There is no definitve way to know if its `borrowed` or not.
/// Refer to `individual usage` for the same.
pub struct SAVMC_IStream<T> {
  pub state: *mut c_void,
  pub size_hint: SAVMC_IStream_ISizeHint,
  pub read_ptr: extern "C" fn(*mut c_void) -> SAVMC_Maybe<T>,
  pub drop_ptr: extern "C" fn(*mut c_void),
}

#[repr(C)]
/// Describes a SizeHint to help with allocations
pub struct SAVMC_IStream_ISizeHint {
  pub min: usize,
  pub max: SAVMC_Maybe<usize>,
}

extern "C" fn drop_noop(_: *mut c_void) {}
extern "C" fn readptr<T, E: Iterator<Item = T>>(state: *mut c_void) -> SAVMC_Maybe<T> {
  unsafe {
    match (*(state as *mut E)).next() {
      Some(x) => SAVMC_Maybe::Some(x),
      None => SAVMC_Maybe::None,
    }
  }
}

type IStream<T> = SAVMC_IStream<T>;

impl<T> IStream<T> {
  pub fn create_borrowed<E: Iterator<Item = T>>(stream: &mut E) -> Self {
    let (min_hint, max_hint) = stream.size_hint();
    Self {
      state: stream as *mut _ as _,
      size_hint: SAVMC_IStream_ISizeHint {
        min: min_hint,
        max: max_hint.map_or(SAVMC_Maybe::None, |x| SAVMC_Maybe::Some(x)),
      },
      drop_ptr: drop_noop,
      read_ptr: readptr::<T, E>,
    }
  }
}

impl<T> Iterator for IStream<T> {
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> {
    match (self.read_ptr)(self.state) {
      SAVMC_Maybe::Some(x) => Some(x),
      _ => None,
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    (
      self.size_hint.min,
      match self.size_hint.max {
        SAVMC_Maybe::Some(x) => Some(x),
        _ => None,
      },
    )
  }
}

impl<T> Drop for IStream<T> {
  fn drop(&mut self) {
    (self.drop_ptr)(self.state);
  }
}
