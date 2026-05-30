use std::io::{ErrorKind, IoSlice, Write};

pub trait VectoredWrite: Write {
  fn vectored_write(&mut self, mut bufs: &mut [IoSlice<'_>]) -> Option<()> {
    // Guarantee that bufs is empty if it contains no data,
    // to avoid calling write_vectored if there is no data to be written.
    IoSlice::advance_slices(&mut bufs, 0);
    while !bufs.is_empty() {
      match self.write_vectored(bufs) {
        Ok(0) => {
          return None;
        }
        Ok(n) => IoSlice::advance_slices(&mut bufs, n),
        Err(ref e) if e.kind() == ErrorKind::Interrupted => {}
        Err(_) => return None,
      }
    }
    Some(())
  }
}

impl<T: Write> VectoredWrite for T {}
