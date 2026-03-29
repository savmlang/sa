use savm::{
  BytecodeResolver, CacheData, CacheLevel, ResolvedData, SymbolMapTable, SymbolMapTableInfo, VM,
  sync::VMSTAT,
};
use serde::{Deserialize, Serialize};
use std::{
  fs::{self, File},
  mem::zeroed,
};

#[derive(Debug, Serialize, Deserialize)]
struct ExpectedOutput {
  r1: u64,
  r2: u64,
  r3: u64,
  r4: u64,
  r5: u64,
  r6: u64,
  r7: u64,
  r8: u64,
}

struct Resolver(pub usize);

impl BytecodeResolver for Resolver {
  fn learn_data(&self, _: u64) -> SymbolMapTableInfo {
    SymbolMapTableInfo::MixedSizedBytecode
  }
  fn get_cache(&self, _: u64, _: CacheLevel) -> CacheData {
    CacheData::None
  }
  fn get_best_cache(&self, _: u64) -> CacheData {
    CacheData::None
  }
  fn update_cache(&self, _section: u64, _cache: CacheData) {}
  fn heuristic_pgo(&self) -> [&[u64]; 2] {
    [&[], &[]]
  }
  fn last_section_id(&self) -> u64 {
    (self.0 - 1) as _
  }
  fn resolve_data(&self, section: u64) -> SymbolMapTable<Box<dyn ResolvedData>> {
    SymbolMapTable::MixedSizedBytecode {
      bytecode: Box::new(File::open(format!("./dist/{section}")).unwrap()),
    }
  }
}

fn main() {
  let total = fs::read_dir("./dist").unwrap().count();
  let vm = VM::new(Resolver(total));

  for sectionid in 0..total {
    println!("[TESTING] #{sectionid}");

    let out: ExpectedOutput =
      serde_json::from_reader(File::open(format!("./expected/{sectionid}.json")).unwrap()).unwrap();

    vm.call_section(sectionid as _);

    VMSTAT.with(|x| unsafe {
      let mt = &mut *x.get();

      let ts = &mt.ts[0];

      let actual = [
        ts.r1.u64, ts.r2.u64, ts.r3.u64, ts.r4.u64, ts.r5.u64, ts.r6.u64, ts.r7.u64, ts.r8.u64,
      ];
      let expected = [
        out.r1, out.r2, out.r3, out.r4, out.r5, out.r6, out.r7, out.r8,
      ];

      assert_eq!(actual, expected);

      for i in 0..8 {
        assert_eq!(
          actual[i],
          expected[i],
          "Logic Error in Section {} at Register r{}",
          sectionid,
          i + 1
        );
      }

      mt.ts = zeroed();
    });

    println!("[PASS]    #{sectionid}");
    println!();
  }
}
