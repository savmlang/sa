use std::{
  fs::{self, File},
  io::Read,
  path::PathBuf,
};

use rusqlite::{Connection, params};

pub fn emit(path: &mut PathBuf, conn: &Connection) {
  path.push("metadata");

  let mut buffer = Vec::with_capacity(1024 * 64); // Pre-allocate 64KB
  fs::read_dir(&path)
    .unwrap()
    .map(|x| x.unwrap())
    .for_each(|x| {
      buffer.clear(); // Keep the capacity, reset the length

      let secid = x.file_name();

      let mut file = File::open(x.path()).unwrap();

      file.read_to_end(&mut buffer).unwrap();

      let sectid = secid.to_str().unwrap().parse::<u64>().unwrap();

      conn
        .execute(
          "INSERT INTO Metadata (identifier, valuedata) VALUES (?1, ?2)",
          params![sectid as i64, &buffer as &[u8]],
        )
        .unwrap();
    });

  path.pop();
}
