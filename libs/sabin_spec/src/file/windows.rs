use std::{ffi::c_void, iter::once};

use windows::{
  Win32::{
    Foundation::{CloseHandle, GENERIC_READ, HANDLE},
    Storage::FileSystem::{
      CreateFileW, FILE_ATTRIBUTE_NORMAL, FILE_FLAG_RANDOM_ACCESS, FILE_SHARE_READ, GetFileSizeEx,
      OPEN_EXISTING,
    },
    System::Memory::{
      CreateFileMappingW, FILE_MAP_READ, MEMORY_MAPPED_VIEW_ADDRESS, MapViewOfFile, PAGE_READONLY,
      UnmapViewOfFile,
    },
  },
  core::PCWSTR,
};

use crate::file::IRamFile;

#[repr(transparent)]
struct AutoHwnd(pub HANDLE);
impl Drop for AutoHwnd {
  fn drop(&mut self) {
    unsafe {
      _ = CloseHandle(self.0);
    }
  }
}

#[repr(transparent)]
struct FileView(pub *mut c_void);
impl Drop for FileView {
  fn drop(&mut self) {
    unsafe {
      _ = UnmapViewOfFile(MEMORY_MAPPED_VIEW_ADDRESS { Value: self.0 });
    }
  }
}

pub struct RamFile {
  hviewaddr: FileView,
  _hfilemappingobject: AutoHwnd,
  _hfile: AutoHwnd,

  size: u64,
}

impl IRamFile for RamFile {
  fn open(path: &str) -> Result<Self, i32>
  where
    Self: Sized,
  {
    unsafe {
      let utf16_size = path.len() + 1;

      let container;
      let lpfilename = if utf16_size <= 128 {
        let mut slice = [0u16; 128];
        path
          .encode_utf16()
          .chain(once(0))
          .enumerate()
          .for_each(|(idx, data)| {
            *slice.get_unchecked_mut(idx) = data;
          });

        container = UTF16::Stack(slice);

        let UTF16::Stack(slice) = &container else {
          unreachable!()
        };
        slice.as_ptr()
      } else {
        let out = path.encode_utf16().chain(once(0)).collect::<Box<[_]>>();

        container = UTF16::Vect(out);

        let UTF16::Vect(slice) = &container else {
          unreachable!()
        };
        slice.as_ptr()
      };

      let _hfile = CreateFileW(
        PCWSTR::from_raw(lpfilename),
        GENERIC_READ.0,
        FILE_SHARE_READ,
        None,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS,
        None,
      )
      .map(AutoHwnd)
      .map_err(|e| e.code().0)?;

      let mut size = 0;
      GetFileSizeEx(_hfile.0, &mut size).map_err(|e| e.code().0)?;

      let _hfilemappingobject = CreateFileMappingW(_hfile.0, None, PAGE_READONLY, 0, 0, None)
        .map(AutoHwnd)
        .map_err(|e| e.code().0)?;

      let hviewaddr = FileView(MapViewOfFile(_hfilemappingobject.0, FILE_MAP_READ, 0, 0, 0).Value);

      drop(container);

      Ok(Self {
        _hfile,
        _hfilemappingobject,
        hviewaddr,

        size: size.cast_unsigned(),
      })
    }
  }

  fn as_slice<'a>(&'a self) -> &'a [u8] {
    unsafe {
      if self.hviewaddr.0.is_null() || self.size == 0 {
        return &[];
      }

      let viewaddr = self.hviewaddr.0 as *mut u8 as *const u8;

      core::slice::from_raw_parts(viewaddr, self.size as _)
    }
  }
}

enum UTF16 {
  Stack([u16; 128]),
  Vect(Box<[u16]>),
}
