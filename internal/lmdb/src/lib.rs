use crate::ffi::{
  mdb_env_close, mdb_env_create, mdb_env_open, mdb_env_set_mapsize, mdb_env_set_maxdbs, mdb_mode_t,
  MDB_env,
};
use std::{
  borrow::Borrow,
  ffi::{c_uint, CStr},
  num::NonZeroI32,
  ptr::null_mut,
};

pub mod ffi;

pub struct Environment {
  pub _raw: *mut MDB_env,
}

unsafe impl Send for Environment {}
unsafe impl Sync for Environment {}

impl Environment {
  pub fn open<T: Borrow<CStr>>(
    size: usize,
    dbs: u32,
    openoptions: OpenOptions<T>,
  ) -> Result<Self, NonZeroI32> {
    unsafe {
      let mut env = null_mut();
      fallible(mdb_env_create(&mut env))?;

      let env = Environment { _raw: env };
      fallible(mdb_env_set_maxdbs(env._raw, dbs))?;
      fallible(mdb_env_set_mapsize(env._raw, size))?;

      let OpenOptions { path, flags, mode } = openoptions;
      fallible(mdb_env_open(env._raw, path.borrow().as_ptr(), flags, mode))?;

      Ok(env)
    }
  }
}

pub struct OpenOptions<T: Borrow<CStr>> {
  pub path: T,
  pub flags: c_uint,
  pub mode: mdb_mode_t,
}

impl Drop for Environment {
  fn drop(&mut self) {
    unsafe {
      mdb_env_close(self._raw);
    }
  }
}

#[inline]
fn fallible(code: i32) -> Result<(), NonZeroI32> {
  if code != 0 {
    return unsafe { Err(NonZeroI32::new_unchecked(code)) };
  }
  Ok(())
}
