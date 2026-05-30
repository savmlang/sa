use libc::{ESRCH, kill};
use std::io::Error;

pub fn is_alive(procid: u32) -> bool {
  unsafe {
    if kill(procid as _, 0) == 0 {
      return true;
    }
  }

  let errno = Error::last_os_error().raw_os_error();

  errno.map(|err| err != ESRCH).unwrap_or(true)
}
