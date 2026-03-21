use std::{env::current_dir, fs, path::PathBuf};

use rusqlite::{Connection, OpenFlags};

pub fn setup_db(distdir: &str) -> (PathBuf, Connection) {
  let mut cwd = current_dir().unwrap();
  cwd.push(distdir);
  cwd.push("binary.sabin");

  _ = fs::remove_file(&distdir);

  let conn = Connection::open_with_flags(
    &cwd,
    OpenFlags::SQLITE_OPEN_CREATE
      | OpenFlags::SQLITE_OPEN_READ_WRITE
      | OpenFlags::SQLITE_OPEN_NO_MUTEX,
  )
  .expect("Unable to create database");

  cwd.pop();

  conn
    .execute_batch(include_str!("./prelude/data.sql"))
    .unwrap();

  (cwd, conn)
}
