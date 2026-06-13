use std::{
  ffi::c_void,
  io::{Error, Read, Seek, SeekFrom},
};

use crate::SAVMC_Maybe;

#[repr(C)]
pub enum ISeekFrom {
  Start(u64),
  End(i64),
  Current(i64),
}

#[repr(C)]
pub struct IResolveData {
  pub state: IResolveState,

  pub read_ptr: extern "C" fn(*mut IResolveState, *mut u8, usize) -> SAVMC_Maybe<usize>,
  pub seek_ptr: extern "C" fn(*mut IResolveState, ISeekFrom) -> SAVMC_Maybe<u64>,
  pub free: extern "C" fn(*mut IResolveState),
}

#[repr(C)]
/// This is a model struct to happily utilize the full
/// 64-bytes we can use thx to 64B alignment :)
///
/// Use it anyhow you want to manage the state
pub struct IResolveState {
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

impl Seek for IResolveData {
  fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
    let pos = match pos {
      SeekFrom::Current(x) => ISeekFrom::Current(x),
      SeekFrom::Start(x) => ISeekFrom::Start(x),
      SeekFrom::End(x) => ISeekFrom::End(x),
    };

    match (self.seek_ptr)(&mut self.state, pos) {
      SAVMC_Maybe::Some(x) => Ok(x),
      _ => Err(Error::last_os_error()),
    }
  }
}

impl Drop for IResolveData {
  fn drop(&mut self) {
    (self.free)(&mut self.state);
  }
}
