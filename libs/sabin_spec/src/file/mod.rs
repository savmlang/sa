#[cfg(windows)]
pub mod windows;
#[cfg(windows)]
pub use windows::RamFile;

#[cfg(unix)]
pub mod unix;
#[cfg(unix)]
pub use unix::RamFile;

pub trait IRamFile {
  /// On failure, this results in the OS ERROR code
  fn open(path: &str) -> Result<Self, i32>
  where
    Self: Sized;

  fn as_slice<'a>(&'a self) -> &'a [u8];
}
