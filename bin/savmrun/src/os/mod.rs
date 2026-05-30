#[cfg(unix)]
mod unix;

#[cfg(unix)]
pub use unix::*;

#[cfg(windows)]
mod win32;

#[cfg(windows)]
pub use win32::*;
