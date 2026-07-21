use std::{num::NonZero, thread::available_parallelism};

#[cfg(target_os = "linux")]
pub(crate) mod linux;
#[cfg(windows)]
pub(crate) mod win32;

/// Get the total number of processors
///
/// This returns the LOGICAL processors
pub fn total_processors() -> Result<usize, impl std::error::Error> {
  available_parallelism().map(NonZero::get)
}

/// Sets the core id specified by the INDEX
/// as the THREAD's AFFINITY
pub unsafe fn set_core(coreid: usize) -> Option<()> {
  unsafe {
    #[cfg(windows)]
    return win32::set_core(coreid);

    #[cfg(target_os = "linux")]
    return linux::set_core(coreid);

    // macOS : Since macOS does not have direct core set system - we do nothing
  }

  #[cfg(not(any(windows, target_os = "linux")))]
  Some(())
}
