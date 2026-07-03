use core::slice;
use sajit::Executable;
use sart::{code::SwappableCodeStore, salloc};
use std::sync::atomic::{AtomicPtr, Ordering};

pub type CodeStore = SwappableCodeStore<*const Executable>;
pub type Base = *mut CodeStore;

#[derive(Debug)]
pub struct SwappableCodeSpace {
  region: *mut *mut CodeStore,
  len: usize,
}

unsafe impl Send for SwappableCodeSpace {}
unsafe impl Sync for SwappableCodeSpace {}

impl SwappableCodeSpace {
  pub fn create(len: usize) -> Option<Self> {
    if len == 0 {
      return None;
    }

    let alloc = unsafe { salloc::aligned_zalloc(size_of::<Base>() * len, align_of::<Base>()) };

    if alloc.is_null() {
      return None;
    }

    Some(Self {
      len,
      region: alloc as _,
    })
  }

  pub fn get<'a>(&'a self, idx: u64) -> Option<&'a CodeStore> {
    let idx: usize = usize::try_from(idx).ok()?;

    if idx >= self.len {
      return None;
    }

    unsafe {
      let region = self.region.add(idx);

      let swappablestore = AtomicPtr::from_ptr(region).load(Ordering::Relaxed);

      if swappablestore.is_null() {
        return None;
      }

      Some(&*swappablestore)
    }
  }

  /// Adding another item while one is present is undefined behaviour
  pub unsafe fn set<'a>(&'a self, idx: u64, store: CodeStore) -> Option<()> {
    let idx: usize = usize::try_from(idx).ok()?;

    if idx >= self.len {
      return None;
    }

    let raw = Box::into_raw(Box::new(store));

    unsafe {
      let region = self.region.add(idx);

      AtomicPtr::from_ptr(region).store(raw, Ordering::Relaxed);

      Some(())
    }
  }
}

impl Drop for SwappableCodeSpace {
  fn drop(&mut self) {
    unsafe {
      for &elm in slice::from_raw_parts(self.region, self.len) {
        if !elm.is_null() {
          _ = Box::from_raw(elm);
        }
      }

      salloc::aligned_free(self.region as _);
    }
  }
}
