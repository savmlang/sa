use parking_lot::Mutex;
use savm::{acaot::pickle::def::PickleInstruction, sart::structures::ffi::CallSig};
use std::{
  env::consts::{DLL_PREFIX, DLL_SUFFIX},
  io::Cursor,
  mem::transmute,
  path::PathBuf,
  ptr,
  sync::Arc,
};

use dashmap::DashMap;
use rusqlite::{Connection, Row};
use savm::{
  BytecodeResolver, CacheData, CacheLevel, ResolvedData, SymbolMapTable, SymbolMapTableInfo,
};

use crate::os::OSLibrary;

#[derive(Clone, Copy)]
pub struct UnSafePtrRef<T>(pub *const T);
unsafe impl<T> Send for UnSafePtrRef<T> {}
unsafe impl<T> Sync for UnSafePtrRef<T> {}

pub struct ApplicationManager {
  // These Mutexes are infact is a Zero-Cost wrapper
  // because we disabled sqlite MUTEX
  pub bytecodedata: Mutex<Connection>,

  pub cache: Mutex<Option<Connection>>,

  // Data fetched from metadata
  pub last_section: u64,
  pub pgo: [Box<[u64]>; 2],

  // Loaded dylibs
  pub dylibroot: Mutex<PathBuf>,

  pub libbookeeping: DashMap<(u64, u64), ResolvedLibFn, ahash::RandomState>,
  pub dylibs: DylibMapBox,
}

pub struct ResolvedLibFn {
  pub fnptr: UnSafePtrRef<()>,
  pub cdecl: CallSig,
}

impl ApplicationManager {
  fn resolve_data<E, F: FnOnce(&ResolvedLibFn) -> E>(&self, libid: u64, funcid: u64, cb: F) -> E {
    let oslib = self.dylibs.0.get(&libid).expect("Resolved dylib");

    let dnet = self
      .bytecodedata
      .lock()
      .query_one(
        "SELECT symbol_name,callsig from LibFnmap WHERE library_id=? AND function_id=?",
        [libid.cast_signed(), funcid.cast_signed()],
        |x| {
          let bytes = x.get_ref("symbol_name")?.as_bytes()?;

          let post = x.get_ref("callsig")?.as_bytes()?;
          let cdecl: CallSig =
            postcard::from_bytes(post).expect("CRITICAL ERROR : Unable to parse CallSig");

          // 512 bytes (limit as desbried in our spec + 1 NUL byte)
          let mut buf = [0u8; 513];
          unsafe {
            ptr::copy_nonoverlapping(bytes.as_ptr(), buf.as_mut_ptr(), bytes.len().min(512));
          }

          let resolved = oslib
            .resolve::<*const ()>(buf.as_ptr())
            .map(|x| unsafe { x.unguard() })
            .expect("Unable to resolve symbol");

          Ok(ResolvedLibFn {
            fnptr: UnSafePtrRef(resolved),
            cdecl,
          })
        },
      )
      .expect("Unable to resolve");

    let out = cb(&dnet);
    self.libbookeeping.insert((libid, funcid), dnet);

    out
  }

  fn import_dylib<E, F: FnOnce(&ResolvedLibFn) -> E>(&self, libid: u64, funcid: u64, cb: F) -> E {
    if let Some(_) = self.dylibs.0.get(&libid) {
      // TODO: Replace with `become` once stable
      return self.resolve_data(libid, funcid, cb);
    };

    // ## LOCK
    // Prevent others from loading
    // any library while we hold the lock
    //
    // This indeed makes it slower - but much more efficient
    let mut lck = self.dylibroot.lock();

    lck.push(format!("{}{}{}", DLL_PREFIX, libid, DLL_SUFFIX));

    let oslib = OSLibrary::load(lck.to_str().expect("PATH ERROR: Non UTF8 data"))
      .expect("CRITICAL: OS Loader could not resolve dll");

    _ = self.dylibs.0.insert(libid, oslib);

    drop(lck);

    // TODO: Replace with `become` once stable
    return self.resolve_data(libid, funcid, cb);
  }
}

impl BytecodeResolver for ApplicationManager {
  fn last_section_id(&self) -> u64 {
    self.last_section
  }

  fn heuristic_pgo(&self) -> [&[u64]; 2] {
    [self.pgo[0].as_ref(), self.pgo[1].as_ref()]
  }

  fn resolve_data(&self, section: u64) -> SymbolMapTable<Box<dyn ResolvedData>> {
    let conn = self.bytecodedata.lock();
    let (assetid, bindata): (i64, Box<[u8]>) = conn
      .query_one(
        "SELECT assetclass,bindata FROM Bincode WHERE sectionid=?",
        [section as i64],
        |x| Ok((x.get("assetclass")?, x.get("bindata")?)),
      )
      .expect("Cannot query sectionid");

    // CRITICAL
    // Failing to drop early can lead to a
    drop(conn);

    let assetid = assetid.cast_unsigned();

    match assetid {
      0 => SymbolMapTable::MixedSizedBytecode {
        bytecode: Box::new(Cursor::new(bindata)),
      },
      // bits 0..64 = lib id
      // bits 64.. = func id
      1 => {
        let libid = u64::from_le_bytes(
          bindata[0..8]
            .try_into()
            .expect("The slice defines [0..8] so it can't error"),
        );
        let funcid = u64::from_le_bytes(
          bindata[8..16]
            .try_into()
            .expect("The slice defines [0..8] so it can't error"),
        );

        match self.libbookeeping.get(&(libid, funcid)) {
          Some(d) => {
            let v = d.value();

            return SymbolMapTable::NativePointer {
              fnptr: unsafe { transmute(v.fnptr.0) },
              cdecl: d.cdecl.clone(),
            };
          }
          None => {
            let (v, cdecl) = self.import_dylib(libid, funcid, |n| (n.fnptr.0, n.cdecl.clone()));

            return SymbolMapTable::NativePointer {
              fnptr: unsafe { transmute(v) },
              cdecl,
            };
          }
        };
      }
      e => panic!("Unknown assettype {e}"),
    }
  }

  fn learn_data(&self, section: u64) -> savm::SymbolMapTableInfo {
    let conn = self.bytecodedata.lock();
    let (assetid, _bindata): (i64, Box<[u8]>) = conn
      .query_one(
        "SELECT * FROM Bincode WHERE sectionid=?",
        [section as i64],
        |x| Ok((x.get("assetclass")?, x.get("bindata")?)),
      )
      .expect("Cannot query sectionid");

    // CRITICAL
    // Failing to drop early can lead to a
    drop(conn);

    let assetid = assetid.cast_unsigned();

    match assetid {
      0 => SymbolMapTableInfo::MixedSizedBytecode,
      1 => SymbolMapTableInfo::NativePointer,
      e => panic!("Unknown assettype {e}"),
    }
  }

  fn update_cache(&self, section: u64, cache: CacheData) {
    let mut conn = self.cache.lock();
    let Some(conn) = conn.as_mut() else {
      return;
    };

    let cmd = "INSERT INTO Cache (sectionid, optlevel, metamap, machinecode)
      VALUES (?1, ?2, ?3, ?4)
      ON CONFLICT(sectionid, optlevel) 
      DO UPDATE SET
        metamap = excluded.metamap,
        machinecode = excluded.machinecode;";

    let Ok(mut transaction) = conn.prepare(cmd) else {
      return;
    };

    let (optlevel, metamap, machinecode) = match &cache {
      CacheData::None => return,
      CacheData::Pickle { out, jumps } => {
        let out_as_bytes: &[u8] =
          unsafe { std::slice::from_raw_parts(out.as_ptr() as *const u8, out.len() * 4) };
        let metadata = postcard::to_allocvec(jumps.as_ref()).expect("Unable to parse");

        (0i64, metadata, out_as_bytes)
      }
      _ => todo!(),
    };

    _ = transaction.execute(rusqlite::params![
      section.cast_signed(),
      optlevel,
      &metamap as &[u8],
      machinecode
    ]);

    drop(cache);
  }

  fn get_best_cache(&self, section: u64) -> CacheData {
    let mut conn_guard = self.cache.lock();
    let Some(conn) = conn_guard.as_mut() else {
      return CacheData::None;
    };

    let query = "SELECT optlevel, metamap, machinecode
      FROM Cache 
      WHERE sectionid = ? 
      ORDER BY optlevel DESC LIMIT 1";

    conn
      .query_row(query, [section as i64], |row| {
        let optlevel: i64 = row.get(0)?;

        let Some(level) = CacheLevel::from_int(optlevel) else {
          return Ok(CacheData::None);
        };

        Self::process_cache(level, row)
      })
      .unwrap_or(CacheData::None)
  }

  fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData {
    let optlevel: i64 = level.to_int() as i64;

    let mut conn = self.cache.lock();
    let Some(conn) = conn.as_mut() else {
      return CacheData::None;
    };

    conn
      .query_one(
        "SELECT metamap, machinecode FROM Cache WHERE sectionid=? AND optlevel=?",
        [section.cast_signed(), optlevel],
        |row| Self::process_cache(level, row),
      )
      .ok()
      .unwrap_or(CacheData::None)
  }
}

impl ApplicationManager {
  fn process_cache(level: CacheLevel, row: &Row) -> Result<CacheData, rusqlite::Error> {
    match level {
      CacheLevel::Pickle => {
        let out = row
          .get_ref("machinecode")?
          .as_blob()?
          .as_chunks::<4>()
          .0
          .into_iter()
          .map(|x| *x)
          .map(|x| PickleInstruction {
            opcode: x[0],
            u1: x[1],
            u2: x[2],
            u3: x[3],
          })
          .collect::<Arc<[PickleInstruction]>>();

        Ok(CacheData::Pickle {
          out,
          jumps: Arc::new(
            postcard::from_bytes(row.get_ref("metamap")?.as_bytes()?)
              .map_err(|_| rusqlite::Error::BlobSizeError)?,
          ),
        })
      }
      // Use the general Machine Code
      _mc => {
        todo!()
      }
    }
  }
}

impl Drop for ApplicationManager {
  // Backup the caches db (nicely)
  fn drop(&mut self) {}
}

pub struct DylibMapBox(pub DashMap<u64, OSLibrary, ahash::RandomState>);

unsafe impl Send for DylibMapBox {}
unsafe impl Sync for DylibMapBox {}
