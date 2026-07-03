use super::IRamFile;
use libc::{
  MAP_FAILED, MAP_PRIVATE, O_RDONLY, POSIX_MADV_RANDOM, PROT_READ, close, fstat, mmap, munmap,
  open, posix_madvise, stat,
};
use std::{
  ffi::{CString, c_int, c_void},
  mem::MaybeUninit,
  ptr::null_mut,
};

struct FD(c_int);
impl Drop for FD {
  fn drop(&mut self) {
    unsafe {
      close(self.0);
    }
  }
}

struct MMAP(*mut c_void, usize);
impl Drop for MMAP {
  fn drop(&mut self) {
    unsafe {
      if !self.0.is_null() && self.1 > 0 {
        munmap(self.0, self.1 as _);
      }
    }
  }
}

pub struct RamFile {
  // Follow the inverse order of declaration here!
  memmap: MMAP,
  _fd: FD,
}

impl IRamFile for RamFile {
  fn open(path: &str) -> Result<Self, i32>
  where
    Self: Sized,
  {
    unsafe {
      let strn = CString::new(path).ok().ok_or(0)?;

      let fd = open(strn.as_ptr(), O_RDONLY);
      if fd < 0 {
        return Err(-1);
      }
      let fd = FD(fd);

      let mut stat = MaybeUninit::<stat>::uninit();
      if fstat(fd.0, stat.as_mut_ptr()) < 0 {
        return Err(-2);
      }
      let stat = stat.assume_init();
      let size = stat.st_size as usize;

      let mmapd = mmap(null_mut(), size as _, PROT_READ, MAP_PRIVATE, fd.0, 0);
      if mmapd == MAP_FAILED {
        return Err(-3);
      }

      posix_madvise(mmapd, size as _, POSIX_MADV_RANDOM);

      let memmap = MMAP(mmapd, size);

      Ok(Self { _fd: fd, memmap })
    }
  }

  fn as_slice<'a>(&'a self) -> &'a [u8] {
    if self.memmap.1 == 0 {
      return &[];
    }

    unsafe {
      core::slice::from_raw_parts(self.memmap.0 as *mut u8 as *const u8, self.memmap.1 as _)
    }
  }
}
