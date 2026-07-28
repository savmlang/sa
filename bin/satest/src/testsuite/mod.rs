use std::mem::zeroed;
#[cfg(feature = "native")]
use std::{sync::Arc, time::Instant};

use crate::ExpectedOutput;
#[cfg(feature = "native")]
use crate::jitmem::{JITMemData, run::run_jit};
use console::Style;
use savm::{BytecodeResolver, VM, sart::ctr::FLAGS::FLAG_FIRST, sync::VMSTAT};
#[cfg(feature = "native")]
use savm::{
  CacheData, CacheLevel,
  acaot::{native::NativeCompiler, pickle::def::PickleInstruction},
  kvwrap::SaVMJumpWrapRef,
};

pub fn clean() {
  VMSTAT.with(|x| {
    let mt = unsafe { &mut *x.get() };

    for (idx, ts) in mt.ts.iter_mut().enumerate() {
      // Only preserve scratchpad
      let scratchpad = ts.scratchpad;

      *ts = unsafe { zeroed() };
      if idx == 0 {
        ts.flags |= FLAG_FIRST;
      }

      ts.scratchpad = scratchpad;
    }
  });
}

pub fn test_vm_interpreter<T: BytecodeResolver + Send + Sync + 'static>(
  vm: &VM<T>,
  out: &ExpectedOutput,
  sectionid: u64,
  fail: &mut bool,
) {
  println!(
    "{:>14} Start TestID #{sectionid} (Chocolate - Interpreter)",
    Style::new().yellow().apply_to("Test")
  );

  clean();
  vm.dispatch_chocolate::<false>(sectionid);

  let localfail = assertchecks(out, fail);

  if !localfail {
    println!(
      "{:>14} {} TestID #{sectionid} (Chocolate - Interpreter)",
      Style::new().blue().apply_to("Test"),
      Style::new().green().apply_to("Success")
    );
  }
}

#[cfg(feature = "native")]
pub fn test_jits<T: BytecodeResolver + Send + Sync + 'static>(
  vm: &VM<T>,
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

  let outarc: Arc<[PickleInstruction]> = Arc::from(worker.out.into_boxed_slice());
  let jumps: Arc<[savm::PickleJumpData]> = Arc::from(worker.jump);
  let libcalls = Arc::from(worker.libcalls);

  let out2 = outarc.clone();
  let jumps2 = jumps.clone();
  vm.resolve.as_ref().update_cache(
    sectionid,
    CacheData::Pickle {
      out: outarc,
      jumps,
      libcalls: Some(libcalls),
    },
  );

  for (name, builder) in testing_compiler_infra::<true>() {
    println!(
      "\n{:>14} Start TestID #{sectionid} ({name})",
      Style::new().yellow().apply_to("Test"),
    );

    let t0 = Instant::now();
    let mut compiler: Box<dyn NativeCompiler<true>> = builder.get();
    let compiled = compiler.compile(&out2, SaVMJumpWrapRef(&jumps2));

    let tf = t0.elapsed();
    let (exec, _) = match compiled {
      savm::CacheData::JITCache {
        level,
        binary,
        reloc: _reloc,
      } => match level {
        #[cfg(feature = "cranelift")]
        CacheLevel::CraneliftCrafter => {
          use savm::management::jitmem::calculate_relocation_abs;

          let reloc = calculate_relocation_abs(&_reloc);

          jitdata.mem().write_quick(&binary, &reloc)
        }
        #[cfg(feature = "llvm")]
        CacheLevel::LLVMCinder | CacheLevel::LLVMCrater => jitdata
          .mem()
          .write_llvm(&binary, |_x| {
            panic!("Resolver asked for {}!", unsafe { &*_x });
          })
          .expect("Unable to get rest"),
        _ => err("Unsupported CacheLevel"),
      },
      _ => err("Unsupported Compiler Output"),
    };

    jitdata.ptrstore.insert((sectionid, *name), (exec as _, tf));

    clean();
    run_jit(vm, exec);

    let localfailure = assertchecks(out, fail);

    if !localfailure {
      println!(
        "{:>14} {} TestID #{sectionid} ({name})",
        Style::new().blue().apply_to("Test"),
        Style::new().green().apply_to("Success")
      );
    }
  }
}

fn assertchecks(out: &ExpectedOutput, fail: &mut bool) -> bool {
  let mut localfailure = false;
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
        localfailure = true;
        println!(
          "{:>16} Assertion Failed at r{}. Expected: {:#x}, Found: {:#x}",
          Style::new().red().bold().apply_to("FAIL"),
          i + 1,
          expected[i],
          actual[i]
        );
      }
    }
  });

  localfailure
}
