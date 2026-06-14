pub(crate) use savmruncore::ports::VectoredWrite;

use blake3::Hasher;
use parking_lot::Mutex;
use rusqlite::{Connection, MAIN_DB};
use savm::VM;
use std::{
  any::Any,
  env::{
    args,
    consts::{DLL_EXTENSION, DLL_PREFIX},
  },
  fs::File,
  io::IoSlice,
};

use crate::{
  resolver::{ApplicationManager, DylibMapBox},
  sql::{SQL_CACHE_VERSION, clear_savm_tmp, load_sabin, savm_backup, savm_tmp, vm_cache},
};

pub(crate) use savmruncore::os;
pub(crate) mod sql;

use savmruncore::os::cpu::get_cpuname;

mod resolver;

#[global_allocator]
static SALLOC: savmruncore::sallocator::SaAllocator = savmruncore::sallocator::SaAllocator;

fn main() {
  savmruncore::osprelude();

  let mut argv = args();
  _ = argv.next();

  let filename = argv.next();
  let vcache = argv.next();

  let fname: &str = filename.as_deref().unwrap_or("./dist/binary.sabin");
  let sacache = vcache.as_deref().unwrap_or("./dist/cache.bak");

  // Calculate Master Hash!
  let masterhash = {
    let mut corehash = Hasher::new();

    let cpuname = get_cpuname();

    corehash.vectored_write(&mut [
      IoSlice::new(&cpuname),
      IoSlice::new(env!("CARGO_PKG_VERSION").as_bytes()),
      IoSlice::new(env!("BUILD_TARGET").as_bytes() as _),
    ]);

    corehash
      .update_reader(File::open(fname).expect("Unable to read sabin file!"))
      .expect("Could not feed sabin file");

    let hash = corehash.finalize();

    *hash.as_bytes()
  };

  let sabin = load_sabin(fname);

  let dylib_expanded = fetch_bool(&sabin, 3);
  let tmp_to_cwd = fetch_bool(&sabin, 4);

  let mut tmp = savm_tmp(tmp_to_cwd);

  let mut dylibroot = tmp.clone();

  // Change Directory
  // (if preexpanded)
  if dylib_expanded {
    dylibroot = std::path::absolute(fname).expect("Unable to absolute resolve!");
    dylibroot.pop();
  }

  dylibroot.push("libs");

  if !dylib_expanded {
    let mut stmt: rusqlite::Statement<'_> = sabin
      .prepare("SELECT rowid, library_id FROM DllStore WHERE platform=?")
      .expect("Unable to prep statment");

    stmt
      .query_map([env!("BUILD_TARGET")], |x| {
        Ok((
          x.get::<_, i64>(0)?,
          x.get::<_, i64>("library_id")?.cast_unsigned(),
        ))
      })
      .expect("Unknown err")
      .map(|x| x.expect("Unable to load rowid"))
      .for_each(|(row_id, libid)| {
        let mut d = sabin
          .blob_open(MAIN_DB, "DllStore", "dylibcontent", row_id, true)
          .expect("Unable to load blob");

        dylibroot.push(format!("{}{}{}", DLL_PREFIX, libid, DLL_EXTENSION));

        let mut file = File::create(&dylibroot).expect("Unable to open a dylib file");
        std::io::copy(&mut d, &mut file).expect("Unable to write IO");

        dylibroot.pop();
      });
  }

  let cache = vm_cache(sacache, &mut tmp, &masterhash);

  // Prune memory eagerly
  drop(filename);
  drop(argv);

  let val: i32 = sabin
    .query_one("PRAGMA user_version", [], |x| x.get(0usize))
    .unwrap();
  assert!(val == SQL_CACHE_VERSION);

  let mgr = {
    let last_section = u64::from_le_bytes(
      sabin
        .query_one("SELECT * FROM Metadata WHERE identifier=0", [], |x| {
          x.get("valuedata")
        })
        .expect("Unable to load metadata.identifier=0"),
    );

    let top_priority_raw = fetch_vect(&sabin, 1);
    let priority_raw = fetch_vect(&sabin, 2);
    let _global_data = sabin
      .query_one("SELECT * FROM Metadata WHERE identifier=3", [], |x| {
        x.get::<_, Box<[u8]>>("valuedata")
      })
      .expect("Unable to get GlobalData");

    ApplicationManager {
      bytecodedata: Mutex::new(sabin),
      cache: Mutex::new(Some(cache)),
      dylibs: DylibMapBox(Default::default()),
      dylibroot: Mutex::new(dylibroot),
      last_section,
      libbookeeping: Default::default(),
      pgo: [top_priority_raw, priority_raw],
    }
  };

  let vm = VM::new(mgr);

  vm.call_section(0);

  let r = vm.resolve.clone();

  drop(vm);

  let mt = r.as_ref() as &dyn Any;
  let m = mt.downcast_ref::<ApplicationManager>().unwrap();

  let mut lck = m.cache.lock();

  {
    let val = lck.take().expect("Cannot be possible to NOT be null");

    val.close().expect("Unable to close SQLite DB");

    tmp.push("savmcache.sdb");
    savm_backup(tmp.to_str().expect("Unable to express as &str"), sacache);
    tmp.pop();
  }

  clear_savm_tmp(&tmp);

  drop(lck);
  drop(tmp);
}

fn fetch_bool(sabin: &Connection, pkey: i64) -> bool {
  sabin
    .query_one("SELECT * FROM Metadata WHERE identifier=?", [pkey], |x| {
      Ok(
        x.get_ref("valuedata")?
          .as_blob()?
          .get(0)
          .map(|x| *x != 0)
          .unwrap_or_default(),
      )
    })
    .unwrap_or_default()
}

fn fetch_vect(sabin: &Connection, pkey: i64) -> Box<[u64]> {
  sabin
    .query_one("SELECT * FROM Metadata WHERE identifier=?", [pkey], |x| {
      let rf = x.get_ref::<_>("valuedata")?.as_blob()?;

      let chunks = rf.as_chunks::<8>();

      assert!(chunks.1.len() == 0);

      Ok(
        chunks
          .0
          .into_iter()
          .map(|ch| u64::from_le_bytes(*ch))
          .collect::<Box<_>>(),
      )
    })
    .expect("Unable to load metadata")
}
