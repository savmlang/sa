use std::{
  fs::{self, File},
  io,
  os::windows::fs::MetadataExt,
  path::PathBuf,
};

use rusqlite::{Connection, MAIN_DB, blob::ZeroBlob, params};

pub fn emit(path: &mut PathBuf, conn: &Connection) {
  path.push("library");

  fs::read_dir(&path)
    .unwrap()
    .map(|x| x.unwrap())
    .for_each(|x| {
      let id = x.file_name();
      let libid = id.to_str().unwrap().parse::<u64>().unwrap();

      fs::read_dir(x.path())
        .unwrap()
        .map(|y| y.unwrap())
        .for_each(|y| {
          let plt = y.file_name();
          let plt = plt.to_str().unwrap();

          let pth = y.path();

          let size = fs::metadata(&pth).unwrap().file_size();

          const ONE_GIB: u64 = 1024 * 1024 * 1024;
          if size > ONE_GIB {
            panic!("Library {libid} is larger than 1GiB ({ONE_GIB} bytes)");
          }

          let mut file = File::open(&pth).unwrap();

          conn
            .execute(
              "INSERT INTO DllStore (library_id, platform, dylibcontent) VALUES (?1, ?2, ?3)",
              params![libid as i64, plt, ZeroBlob(size as _)],
            )
            .unwrap();

          let rid = conn.last_insert_rowid();
          let mut blob = conn
            .blob_open(MAIN_DB, "DllStore", "dylibcontent", rid, false)
            .unwrap();

          io::copy(&mut file, &mut blob).unwrap();
        });
    });

  path.pop();
}
