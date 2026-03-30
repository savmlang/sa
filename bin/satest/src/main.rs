use savm::{
  BytecodeResolver, CacheData, CacheLevel, ResolvedData, SymbolMapTable, SymbolMapTableInfo, VM,
  sync::VMSTAT,
};
use serde::{Deserialize, Serialize};
use std::{
  fs::{self, File},
  mem::zeroed,
  time::{Duration, Instant},
};

#[derive(Debug, Serialize, Deserialize)]
struct ExpectedOutput {
  #[serde(default)]
  r1: u64,
  #[serde(default)]
  r2: u64,
  #[serde(default)]
  r3: u64,
  #[serde(default)]
  r4: u64,
  #[serde(default)]
  r5: u64,
  #[serde(default)]
  r6: u64,
  #[serde(default)]
  r7: u64,
  #[serde(default)]
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

    let out = fs::read(format!("./expected/{sectionid}.toml")).unwrap();
    let out: ExpectedOutput = toml::from_slice(&out).unwrap();

    let mut durs = vec![];

    for _ in 0..100 {
      let t0 = Instant::now();
      vm.call_section(sectionid as _);
      let tf = t0.elapsed();

      durs.push(tf);

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
    }

    durs.sort();

    let d = (|| {
      let len = durs.len();
      if len == 0 {
        return Duration::new(0, 0);
      }

      if len % 2 == 1 {
        // 2a. If odd, take the middle element
        durs[len / 2]
      } else {
        // 2b. If even, take the average of the two middle elements
        let mid1 = durs[len / 2 - 1];
        let mid2 = durs[len / 2];
        (mid1 + mid2) / 2
      }
    })();

    println!("[PASS]    #{sectionid} in {d:?} (Intl-Mode)");
    println!();
  }
}
