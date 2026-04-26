use std::mem::forget;
use std::time::{Duration, Instant};

use savm::acaot::{native::compiler_infra, pickle::PickleWorker};
use savm::management::jitmem::JITMemoryManager;
use savm::{BytecodeResolver, VM};

struct Modules {}

impl BytecodeResolver for Modules {
  fn get_best_cache(&self, _: u64) -> savm::CacheData {
    todo!()
  }
  fn get_cache(&self, _: u64, _: savm::CacheLevel) -> savm::CacheData {
    todo!()
  }
  fn heuristic_pgo(&self) -> [&[u64]; 2] {
    todo!()
  }
  fn last_section_id(&self) -> u64 {
    todo!()
  }
  fn learn_data(&self, _: u64) -> savm::SymbolMapTableInfo {
    todo!()
  }
  fn resolve_data(&self, _: u64) -> savm::SymbolMapTable<Box<dyn savm::ResolvedData>> {
    todo!()
  }
  fn update_cache(&self, _: u64, _: savm::CacheData) {
    todo!()
  }
}

fn main() {
  let f = std::fs::File::open("./2").unwrap();

  let mut worker = PickleWorker {
    bytecode: f,
    out: vec![],
    jump: Default::default(),
  };

  worker.pass1();

  let mut cranelift = compiler_infra()[0].get();
  let code = cranelift.compile(&worker.out, &worker.jump);

  let vm = unsafe { VM::new_unsafe::<Modules, false>(Modules {}) };

  let mut jmem = JITMemoryManager::new();
  let ptr = match code {
    savm::CacheData::CraneliftAbs8 { binary, .. } => jmem.write_quick(&binary, &[]),
    _ => panic!(),
  };

  let mut durs = vec![];

  for _ in 0..100 {
    let t0 = Instant::now();

    vm.exec_jit(ptr);

    let tf = t0.elapsed();

    durs.push(tf);
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

  println!("[PASS] {d:?}");
  println!();

  forget(jmem);
}
