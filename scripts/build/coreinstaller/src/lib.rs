use std::{env, io::Cursor, path::Path};
use zip::ZipArchive;

const COMPRESSED_DATA: &[u8] = include_bytes!(env!("TARGET_PKG_ZIP"));

pub fn extract<T: AsRef<Path>>(directory: T) {
  let mut archive = ZipArchive::new(Cursor::new(COMPRESSED_DATA)).unwrap();

  archive.extract(directory).unwrap();
}
