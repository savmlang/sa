use std::mem::zeroed;
#[cfg(feature = "native")]
use std::{sync::Arc, time::Instant};

use crate::ExpectedOutput;
#[cfg(feature = "native")]
use crate::jitmem::JITMemData;
use console::Style;
#[cfg(feature = "native")]
use savm::{CacheData, CacheLevel, management::jitmem::calculate_relocation_abs};
use savm::{VM, sync::VMSTAT};

pub fn test_vm_interpreter(vm: &VM, out: &ExpectedOutput, sectionid: u64, fail: &mut bool) {
  println!(
    "{:>14} Start TestID #{sectionid} (Chocolate - Interpreter)",
    Style::new().yellow().apply_to("Test")
  );

  vm.dispatch_chocolate::<false>(sectionid);

  assertchecks(out, fail);

  if !*fail {
    println!(
      "{:>14} {} TestID #{sectionid} (Chocolate - Interpreter)",
      Style::new().blue().apply_to("Test"),
      Style::new().green().apply_to("Success")
    );
  }
}

#[cfg(feature = "native")]
pub fn test_jits(
  vm: &VM,
  jitdata: &mut JITMemData,
  out: &ExpectedOutput,
  sectionid: u64,
  fail: &mut bool,
) {
  use crate::err;
  use savm::{
    SymbolMapTable,
    acaot::{native::testing_compiler_infra, pickle::PickleWorker},
  };

  let mut worker = PickleWorker {
    bytecode: match vm.resolve.resolve_data(sectionid) {
      SymbolMapTable::MixedSizedBytecode { bytecode } => bytecode,
      _ => err("NativePtr is not supported"),
    },
    out: vec![],
    libcalls: Default::default(),
    jump: Default::default(),
  };

  worker.pass1();

  let outarc = Arc::from(worker.out.into_boxed_slice());
  let jumps = Arc::new(worker.jump);
  let libcalls = Arc::new(worker.libcalls);

  let out2 = outarc.clone();
  let jumps2 = jumps.clone();
  vm.resolve.as_ref().update_cache(
    sectionid,
    CacheData::Pickle {
      out: outarc,
      jumps,
      libcalls,
    },
  );

  for (name, builder) in testing_compiler_infra() {
    println!(
      "\n{:>14} Start TestID #{sectionid} ({name})",
      Style::new().yellow().apply_to("Test"),
    );

    let t0 = Instant::now();
    let mut compiler = builder.get();
    let compiled = compiler.compile(&out2, &jumps2);

    let tf = t0.elapsed();
    let exec = match compiled {
      savm::CacheData::JITCache {
        level,
        binary,
        reloc,
      } => match level {
        CacheLevel::CraneliftCrafter | CacheLevel::LLVMCinder | CacheLevel::LLVMCrater => {
          let reloc = calculate_relocation_abs(&reloc);

          let exec = jitdata.mem().write_quick(&binary, &reloc);
          jitdata.ptrstore.insert((sectionid, *name), (exec as _, tf));

          exec
        }
        _ => err("Unsupported CacheLevel"),
      },
      _ => err("Unsupported Compiler Output"),
    };

    vm.exec_jit(exec);

    assertchecks(out, fail);

    if !*fail {
      println!(
        "{:>14} {} TestID #{sectionid} ({name})",
        Style::new().blue().apply_to("Test"),
        Style::new().green().apply_to("Success")
      );
    }
  }
}

fn assertchecks(out: &ExpectedOutput, fail: &mut bool) {
  VMSTAT.with(|x| unsafe {
    let mt = &mut *x.get();

    let ts = &mt.ts[0];

    let actual = [
      ts.r1.u64, ts.r2.u64, ts.r3.u64, ts.r4.u64, ts.r5.u64, ts.r6.u64, ts.r7.u64, ts.r8.u64,
    ];
    let expected = [
      out.r1, out.r2, out.r3, out.r4, out.r5, out.r6, out.r7, out.r8,
    ];

    for i in 0..8 {
      if actual[i] != expected[i] {
        *fail = true;
        println!(
          "{:>16} Assertion Failed at r{}. Expected: {:#x}, Found: {:#x}",
          Style::new().red().bold().apply_to("FAIL"),
          i + 1,
          expected[i],
          actual[i]
        );
      }
    }

    mt.ts = zeroed();
  });
}
