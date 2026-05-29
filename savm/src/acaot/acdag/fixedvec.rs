use core::slice;
use std::{
  alloc::{Layout, alloc, dealloc, handle_alloc_error},
  ops::{Deref, DerefMut},
  ptr,
};

pub struct FixedVec<T: Sized> {
  data: *mut T,
  layout: Layout,
  len: usize,
  cap: usize,
}

impl<T: Sized> FixedVec<T> {
  pub fn new(cap: usize) -> Self {
    let layout = Layout::array::<T>(cap).expect("Layout overflow");

    let data = unsafe { alloc(layout) } as *mut T;

    if data.is_null() {
      handle_alloc_error(layout);
    }

    Self {
      data,
      layout,
      cap,
      len: 0,
    }
  }

  pub fn factory<F: FnMut(usize) -> T>(mut factory: F, cap: usize) -> Self {
    let mut out: FixedVec<T> = FixedVec::new(cap);
    for i in 0..out.cap {
      _ = out.push(factory(i));
    }

    out
  }

  pub fn push(&mut self, item: T) -> Result<(), T> {
    if self.len == self.cap {
      return Err(item);
    }

    unsafe {
      ptr::write(self.data.add(self.len), item);
    }

    self.len += 1;

    Ok(())
  }

  pub fn pop(&mut self) -> Option<T> {
    if self.len == 0 {
      return None;
    }

    let out = unsafe { ptr::read(self.data.add(self.len - 1)) };

    self.len -= 1;

    Some(out)
  }
}

impl<T: Sized> Deref for FixedVec<T> {
  type Target = [T];

  fn deref(&self) -> &Self::Target {
    unsafe { slice::from_raw_parts(self.data, self.len) }
  }
}

impl<T: Sized> DerefMut for FixedVec<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    unsafe { slice::from_raw_parts_mut(self.data, self.len) }
  }
}

impl<T: Sized> Drop for FixedVec<T> {
  fn drop(&mut self) {
    for i in 0..self.len {
      unsafe {
        ptr::drop_in_place(self.data.add(i));
      }
    }

    unsafe { dealloc(self.data as _, self.layout) };
  }
}
