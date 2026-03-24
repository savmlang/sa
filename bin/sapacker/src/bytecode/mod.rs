use std::{
  fs::{self, File},
  io::Read,
  path::PathBuf,
};

use rusqlite::{Connection, params};

const ASSETCLASS_BINCODE: u64 = 0;
const LIBRARY_EARGERLOAD: u64 = 1;
const LIBRARY_EXPLICITLOAD: u64 = 2;

pub fn emit(path: &mut PathBuf, conn: &Connection) {
  for (pth, asst) in [
    ("bytecode", ASSETCLASS_BINCODE),
    ("eagerload", LIBRARY_EARGERLOAD),
    ("explicit", LIBRARY_EXPLICITLOAD),
  ] {
    path.push(pth);

    let mut buffer = Vec::with_capacity(1024 * 64); // Pre-allocate 64KB
    fs::read_dir(&path)
      .unwrap()
      .map(|x| x.unwrap())
      .for_each(|x| {
        buffer.clear(); // Keep the capacity, reset the length

        let secid = x.file_name();
        let sectid = secid.to_str().unwrap().parse::<u64>().unwrap();

        let mut file = File::open(x.path()).unwrap();

        file.read_to_end(&mut buffer).unwrap();

        conn
          .execute(
            "INSERT INTO Bincode (sectionid, assetclass, bindata) VALUES (?1, ?2, ?3)",
            params![sectid as i64, asst as i64, &buffer as &[u8]],
          )
          .unwrap();
      });

    path.pop();
  }
}
