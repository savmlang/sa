use std::{ffi::CString, marker::PhantomData, mem::transmute_copy, ops::Deref, os::raw::c_void};

use libc::{RTLD_LAZY, RTLD_LOCAL, dlclose, dlopen, dlsym};

pub mod cpu;
pub mod proc;

pub struct OSLibrary {
  hwnd: *mut c_void,
}

impl OSLibrary {
  pub fn load(path: &str) -> Option<Self> {
    unsafe {
      let d = CString::new(path).expect("Unable to convert to CStr");

      let hwnd = dlopen(d.as_ptr(), RTLD_LAZY | RTLD_LOCAL);

      drop(d);

      if hwnd.is_null() {
        return None;
      }

      Some(Self { hwnd })
    }
  }

  pub fn resolve<'a, T: Sized>(&'a self, symb: *const u8) -> Option<MGuard<'a, T>> {
    unsafe {
      assert!(size_of::<T>() == size_of::<usize>());
      assert!(align_of::<T>() == align_of::<usize>());

      let addr = dlsym(self.hwnd, symb as _) as *const c_void;

      if addr.is_null() {
        return None;
      }

      Some(MGuard {
        _data: transmute_copy(&addr),
        _guard: PhantomData,
      })
    }
  }
}

pub struct MGuard<'a, T> {
  _data: T,
  _guard: PhantomData<&'a OSLibrary>,
}

impl<'a, T> MGuard<'a, T> {
  pub unsafe fn unguard(self) -> T {
    self._data
  }
}

impl<'a, T> Deref for MGuard<'a, T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self._data
  }
}

impl Drop for OSLibrary {
  fn drop(&mut self) {
    unsafe {
      dlclose(self.hwnd);
    }
  }
}
