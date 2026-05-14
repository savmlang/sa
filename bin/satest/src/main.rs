use console::Style;
use savm::{
  BytecodeResolver, CacheData, CacheLevel, ResolvedData, SymbolMapTable, SymbolMapTableInfo, VM,
};
use serde::{Deserialize, Serialize};
use std::{
  borrow::Cow,
  env::var,
  fmt::Display,
  fs::{self, File},
  process::exit,
};

use crate::bench::interpreter_benchmark;
mod bench;
#[cfg(feature = "native")]
pub(crate) mod jitmem;
mod testbuild;
mod testsuite;

pub(crate) fn err<T: Display>(err: T) -> ! {
  println!("{}", Style::new().red().apply_to(err));
  exit(-1);
}

pub(crate) struct TestHarness {
  pub bench: bool,
  pub jit: bool,
  pub iter: u64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ExpectedOutput {
  #[serde(default)]
  pub r1: u64,
  #[serde(default)]
  pub r2: u64,
  #[serde(default)]
  pub r3: u64,
  #[serde(default)]
  pub r4: u64,
  #[serde(default)]
  pub r5: u64,
  #[serde(default)]
  pub r6: u64,
  #[serde(default)]
  pub r7: u64,
  #[serde(default)]
  pub r8: u64,
}

#[derive(Debug, Clone)]
pub(crate) struct Resolver {
  pub total: usize,
  pub root: Box<str>,
}

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
  fn update_cache(&self, _section: u64, _cache: CacheData) {
    err("This shouldn't happen");
  }
  fn heuristic_pgo(&self) -> [&[u64]; 2] {
    [&[], &[]]
  }
  fn last_section_id(&self) -> u64 {
    (self.total - 1) as _
  }
  fn resolve_data(&self, section: u64) -> SymbolMapTable<Box<dyn ResolvedData>> {
    SymbolMapTable::MixedSizedBytecode {
      bytecode: Box::new(File::open(format!("./{}/dist/{section}", self.root)).unwrap()),
    }
  }
}

fn main() {
  let tests = var("SATEST_TEST_DIR")
    .map(|x| Cow::Owned(x))
    .unwrap_or(Cow::Borrowed("./tests"));

  let mut harness = TestHarness {
    bench: false,
    jit: true,
    iter: 100,
  };

  if let Ok(asmbuild) = File::open(format!("./{tests}/tests.build")) {
    println!(
      "{:>12} tests.build",
      Style::new().yellow().bold().apply_to("Forging")
    );
    testbuild::asmbuild(asmbuild, &mut harness, &tests);

    println!(
      "{:>12} tests.build\n",
      Style::new().green().bold().apply_to("Forged")
    );
  }

  let resolver = Resolver {
    total: fs::read_dir(format!("./{tests}/dist"))
      .expect("Couldn't read directory")
      .count(),
    root: Box::from(tests.as_ref()),
  };

  let savm = unsafe { VM::new_unsafe::<_, false>(resolver) };

  #[cfg(feature = "native")]
  let mut jitdata = jitmem::default();

  let mut sectionids = vec![];

  // FileTests
  {
    println!(
      "{:>12} filetests",
      Style::new().yellow().bold().apply_to("Starting")
    );

    let mut fail = 0u64;

    for entry in fs::read_dir(format!("./{tests}/expected")).unwrap() {
      let entry = entry.expect("Unable to unwrap dir entry.");

      let sectionid = entry
        .file_name()
        .to_str()
        .unwrap()
        .split_once(".")
        .unwrap()
        .0
        .parse::<u64>()
        .unwrap();
      let out: ExpectedOutput =
        toml::from_slice(&fs::read(entry.path()).expect("Unable to read file"))
          .expect("Unable to parse toml entry");

      sectionids.push(sectionid);

      println!(
        "{:>12} Starting TestID #{sectionid}",
        Style::new().yellow().apply_to("Test")
      );

      let mut failtest = false;

      testsuite::test_vm_interpreter(&savm, &out, sectionid, &mut failtest);

      #[cfg(feature = "native")]
      if harness.jit {
        testsuite::test_jits(&savm, &mut jitdata, &out, sectionid, &mut failtest);
      }

      if failtest {
        fail += 1;
        println!(
          "{:>12} TestID #{sectionid}",
          Style::new().green().apply_to("Pass")
        );
      } else {
        println!(
          "{:>12} TestID #{sectionid}",
          Style::new().green().apply_to("Pass")
        );
      }
    }

    if fail != 0 {
      println!(
        "{:>12} {fail} test(s) have failed. Cannot continue to benchmarks.",
        Style::new().red().bold().apply_to("FAILED")
      );

      err("\nTests Failed. Aborting");
    }

    println!(
      "{:>12} All Tests Passed",
      Style::new().green().bold().apply_to("Successful")
    );
  }

  if harness.bench {
    println!();

    println!(
      "{:>12} benchmarks ({} iterations)",
      Style::new().yellow().bold().apply_to("Starting"),
      harness.iter
    );

    println!(
      "{:>12} These may take a while to finish.",
      Style::new().yellow().apply_to("Note")
    );

    for sectionid in sectionids {
      println!(
        "{:>12} TestID #{sectionid}",
        Style::new().bold().yellow().apply_to("Begin")
      );

      interpreter_benchmark(&savm, sectionid, harness.iter);

      #[cfg(feature = "native")]
      if harness.jit {
        use crate::bench::jit_benchmark;

        jit_benchmark(&savm, &mut jitdata, sectionid, harness.iter);
      }
    }

    println!(
      "{:>12} Benchmarks complete.",
      Style::new().green().bold().apply_to("Complete")
    );
  }
}
