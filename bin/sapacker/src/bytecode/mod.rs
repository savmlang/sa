use std::{
  fs::{self, File},
  io::Read,
  path::PathBuf,
};

use rusqlite::{Connection, params};

const ASSETCLASS_BINCODE: u64 = 0;

pub fn emit(path: &mut PathBuf, conn: &Connection) {
  path.push("bytecode");

  let mut sect: u16 = 0;

  let mut buffer = Vec::with_capacity(1024 * 64); // Pre-allocate 64KB
  fs::read_dir(&path)
    .unwrap()
    .map(|x| x.unwrap())
    .for_each(|x| {
      sect += 1;
      buffer.clear(); // Keep the capacity, reset the length

      let secid = x.file_name();
      let sectid = secid.to_str().unwrap().parse::<u64>().unwrap();

      println!("Read {sectid}");

      let mut file = File::open(x.path()).unwrap();

      file.read_to_end(&mut buffer).unwrap();

      conn
        .execute(
          "INSERT INTO Bincode (sectionid, assetclass, bindata) VALUES (?1, ?2, ?3)",
          params![sectid as i64, ASSETCLASS_BINCODE as i64, &buffer as &[u8]],
        )
        .unwrap();

      if sect == 1000 {
        conn
          .execute_batch(
            "COMMIT;
                  BEGIN TRANSACTION;",
          )
          .unwrap();
        sect = 0;
      }
    });

  path.pop();
}
