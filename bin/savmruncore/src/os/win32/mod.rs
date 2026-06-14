use std::{marker::PhantomData, mem::transmute_copy, ops::Deref};

use windows::{
  Win32::{
    Foundation::{FreeLibrary, HMODULE},
    System::LibraryLoader::{GetProcAddress, LoadLibraryW},
  },
  core::{HSTRING, PCSTR, PCWSTR},
};

pub mod cpu;
pub mod proc;

pub struct OSLibrary {
  hmod: HMODULE,
}

impl OSLibrary {
  pub fn load(path: &str) -> Option<Self> {
    unsafe {
      let hst = HSTRING::from(path);
      let pwstr = PCWSTR::from_raw(hst.as_ptr());

      let library = LoadLibraryW(pwstr).ok()?;

      Some(Self { hmod: library })
    }
  }

  pub fn resolve<'a, T: Sized>(&'a self, symb: *const u8) -> Option<MGuard<'a, T>> {
    unsafe {
      assert!(size_of::<T>() == size_of::<usize>());
      assert!(align_of::<T>() == align_of::<usize>());

      let hwnd = GetProcAddress(self.hmod, PCSTR::from_raw(symb))?;

      Some(MGuard {
        _data: transmute_copy(&hwnd),
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
      _ = FreeLibrary(self.hmod);
    }
  }
}
