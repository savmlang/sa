use postcard::{from_bytes, to_allocvec};
use rusqlite::{Connection, params};
use sart::structures::ffi::LibraryResolverStructure;
use std::{fs, path::PathBuf};

pub fn emit(path: &mut PathBuf, conn: &Connection) {
  path.push("libresolver");

  fs::read_dir(&path)
    .unwrap()
    .map(|x| x.unwrap())
    .for_each(|x| {
      let libid = x.file_name();
      let libid = libid.to_string_lossy().parse::<u64>().unwrap();

      let meta = fs::read(x.path()).unwrap();

      let rsv = from_bytes::<LibraryResolverStructure>(&meta).unwrap();

      for (fid, data) in rsv.into_iter() {
        let symbol_name = &data.symbol as &[u8];

        let callsig = to_allocvec(&data.sig).unwrap();

        conn
          .execute(
            "INSERT INTO LibFnMap (library_id, function_id, symbol_name, callsig) VALUES (?1, ?2, ?3, ?4)",
            params![libid as i64, fid as i64, symbol_name, &callsig as &[u8]],
          )
          .unwrap();
      }
    });

  path.pop();
}
