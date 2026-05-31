use std::{
  env::{current_dir, temp_dir},
  fs,
  path::{self, PathBuf},
  process::id,
};

use rusqlite::{Connection, OpenFlags};

use crate::os::proc::is_alive;

pub static SQL_CACHE_CONNFLAG: &'static str = include_str!("./prelude/cacheload.sql");
pub static SQL_CACHE_PRELUDE: &'static str = include_str!("./prelude/cache.sql");

pub static SABIN_PRELUDE: &'static str = include_str!("./prelude/sabin_prelude.sql");

pub const SQL_CACHE_VERSION: i32 = 2;

pub fn savm_tmp(tmp_to_cwd: bool) -> PathBuf {
  let mut tmp = if tmp_to_cwd {
    temp_dir()
  } else {
    current_dir().expect("Unable to fetch CWD")
  };

  tmp.push("savmcache");

  // --- The Janitor Sweep ---
  if let Ok(entries) = fs::read_dir(&tmp) {
    for entry in entries.flatten() {
      if let Ok(name) = entry.file_name().into_string() {
        if let Ok(old_pid) = name.parse::<u32>() {
          // If the process is dead, nuke the folder.
          if !is_alive(old_pid) {
            let _ = fs::remove_dir_all(entry.path());
          }
        }
      }
    }
  }
  // -------------------------

  tmp.push(std::process::id().to_string());

  clear_savm_tmp(&tmp);
  fs::create_dir_all(&tmp).expect("Unable to create SaVM Cache Directory");

  tmp
}

pub fn clear_savm_tmp(path: &PathBuf) {
  _ = fs::remove_dir_all(path);
}

pub fn load_sabin(path: &str) -> Connection {
  let conn = Connection::open_with_flags(
    path,
    OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_NO_MUTEX,
  )
  .expect("ERROR : Unable to load binary file");

  conn
    .execute_batch(SABIN_PRELUDE)
    .expect("Unable to apply optimizations");

  conn
}

#[cfg(feature = "nocache")]
pub fn vm_cache(_: &str, _: &mut PathBuf, hash: &[u8]) -> Connection {
  let db = Connection::open_in_memory().expect("Unable to open a memory sqlite");

  db.execute_batch(SQL_CACHE_PRELUDE).expect("Prelude failed");
  db.execute(
    "INSERT INTO Metadata (prim, valhash) VALUES (?1, ?2)",
    rusqlite::params![1i64, hash],
  )
  .expect("Intertion failed!");
  db.execute_batch(SQL_CACHE_CONNFLAG)
    .expect("SQL Load failed");

  db
}

#[cfg(not(feature = "nocache"))]
pub fn vm_cache(path: &str, tmp: &mut PathBuf, hash: &[u8]) -> Connection {
  // Copy if exists
  if let Some(db) = maybeload(path, tmp, hash) {
    return db;
  }

  tmp.push("savmcache.sdb");

  let c = Connection::open_with_flags(
    tmp.to_str().expect("Unable to load"),
    OpenFlags::SQLITE_OPEN_NO_MUTEX
      | OpenFlags::SQLITE_OPEN_READ_WRITE
      | OpenFlags::SQLITE_OPEN_CREATE,
  )
  .expect("Unable to start connection");

  tmp.pop();

  c.execute_batch(SQL_CACHE_PRELUDE).expect("Prelude failed");
  c.execute(
    "INSERT INTO Metadata (prim, valhash) VALUES (?1, ?2)",
    rusqlite::params![1i64, hash],
  )
  .expect("Intertion failed!");
  c.execute_batch(SQL_CACHE_CONNFLAG)
    .expect("SQL Load failed");

  c
}

#[cfg(not(feature = "nocache"))]
pub fn maybeload(path: &str, tmp: &mut PathBuf, hash: &[u8]) -> Option<Connection> {
  if fs::exists(path).unwrap_or(false) {
    tmp.push("savmcache.sdb");
    fs::copy(path, &tmp).ok()?;
    tmp.pop();

    let c = Connection::open_with_flags(
      tmp.to_str()?,
      OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_NO_MUTEX,
    )
    .ok()?;

    let hashmatch = c
      .query_one("SELECT * FROM Metadata WHERE prim=1", [], |x| {
        Ok(x.get_ref("valhash")?.as_blob()?.eq(hash))
      })
      .ok()?
      && c
        .query_one("PRAGMA user_version", [], |x| {
          Ok(x.get::<_, i32>(0)? == SQL_CACHE_VERSION)
        })
        .ok()?;

    // The good day event
    if hashmatch {
      c.execute_batch(SQL_CACHE_CONNFLAG).ok()?;
      return Some(c);
    }
  }

  None
}

#[cfg(feature = "nocache")]
pub fn savm_backup(_: &str, _: &str) {}

#[cfg(not(feature = "nocache"))]
pub fn savm_backup(cache: &str, master: &str) {
  let tmp = path::absolute(format!("{master}/../master.{}", id()))
    .expect("Severe error")
    .into_os_string();

  let Ok(_) = fs::copy(cache, &tmp) else {
    return;
  };

  // POSIX Guaranteee
  #[cfg(unix)]
  let _ = fs::rename(&tmp, master);

  #[cfg(windows)]
  unsafe {
    use windows::{
      Win32::Storage::FileSystem::{
        REPLACEFILE_IGNORE_ACL_ERRORS, REPLACEFILE_IGNORE_MERGE_ERRORS, ReplaceFileW,
      },
      core::HSTRING,
    };

    let master_w = HSTRING::from(master);
    let tmp_w = HSTRING::from(&tmp);

    let _ = ReplaceFileW(
      &master_w,
      &tmp_w,
      None,
      REPLACEFILE_IGNORE_MERGE_ERRORS | REPLACEFILE_IGNORE_ACL_ERRORS,
      None,
      None,
    );
  }

  let _ = fs::remove_file(tmp);
}
