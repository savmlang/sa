use savm::{
  BytecodeResolver, CacheData, CacheLevel, JIT_CACHE, ResolvedData, SymbolMapTable,
  SymbolMapTableInfo, VM, sync::VMSTAT,
};
use serde::{Deserialize, Serialize};
use std::{
  fs::{self, File},
  mem::zeroed,
  thread::sleep,
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

  sleep(Duration::from_secs(30));

  for sectionid in 0..total {
    println!("[TESTING] #{sectionid}");

    let out = fs::read(format!("./expected/{sectionid}.toml")).unwrap();
    let out: ExpectedOutput = toml::from_slice(&out).unwrap();

    let mut durs_intl = vec![];
    let mut durs_clif = vec![];

    for _ in 0..100 {
      let t0 = Instant::now();
      vm.dispatch_chocolate::<false>(sectionid as _);
      let tf = t0.elapsed();

      durs_intl.push(tf);

      assertchecks(&out, sectionid);

      if let Some(true) = JIT_CACHE
        .get()
        .and_then(|x| Some(x.0.contains_key(&(sectionid as u64))))
      {
        let t0 = Instant::now();
        vm.dispatch_jit(sectionid as _);
        let tf = t0.elapsed();

        durs_clif.push(tf);
      }

      assertchecks(&out, sectionid);
    }

    durs_intl.sort();
    durs_clif.sort();

    let d = |durs: &[Duration]| {
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
    };

    let interpreter = d(&durs_intl);
    let cranelift = d(&durs_clif);
    println!("[PASS]    #{sectionid} in {interpreter:?} (Chocolate - Interpreter)");

    if cranelift.is_zero() {
      println!("[PASS]    #{sectionid} could not compile for (Crafter JIT - Compiler)");
    } else {
      println!("[PASS]    #{sectionid} in {cranelift:?} (Crafter JIT - Compiler)");
    }
    println!();
  }
}

fn assertchecks<T: std::fmt::Display>(out: &ExpectedOutput, sectionid: T) {
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
