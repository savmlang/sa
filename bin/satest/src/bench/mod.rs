use crate::testsuite::clean;
#[cfg(feature = "native")]
use crate::{
  Resolver,
  jitmem::{JITMemData, JITMems, run::run_jit},
};
use console::Style;
use savm::{BytecodeResolver, VM};
#[cfg(feature = "native")]
use savm::{
  Executable,
  acaot::{
    native::{NativeCompilerBuilder, testing_compiler_infra, testing_epitier_compilers},
    pickle::def::PickleInstruction,
  },
};
use statrs::statistics::{Data, Distribution, OrderStatistics};
use std::time::Instant;

fn bench_report(median: &str, p75: &str, p99: &str, sd: &str, compile: Option<&str>) {
  print!(
    "{:>14} {} {median:>12};",
    Style::new().blue().apply_to("Bench"),
    Style::new().yellow().apply_to("Median")
  );
  print!("{:>8} {p75:>12};", Style::new().yellow().apply_to("p75"));
  print!("{:>8} {p99:>12};", Style::new().yellow().apply_to("p99"));
  print!("{:>8} {sd:>12}", Style::new().white().apply_to("SD"));

  if let Some(compile) = compile {
    println!(" ({} {compile})", Style::new().red().apply_to("Compile"));
  } else {
    println!();
  }
  println!();
}

pub fn interpreter_benchmark<T: BytecodeResolver + Send + Sync + 'static>(
  vm: &VM<T>,
  sectionid: u64,
  rounds: u64,
) {
  let mut store = Vec::with_capacity(rounds as usize);
  for _ in 0..rounds {
    clean();

    let t0 = Instant::now();
    vm.dispatch_chocolate::<false>(sectionid);
    let tf = t0.elapsed();
    store.push(tf.as_secs_f64());
  }

  let mut statdata = Data::new(store.as_mut_slice());

  let p99 = format_duration(statdata.percentile(99));
  let p75 = format_duration(statdata.percentile(75));
  let median = format_duration(statdata.median());
  let sd = format_duration(statdata.std_dev().unwrap_or(f64::NEG_INFINITY));

  println!(
    "{:>14} Tier : Chocolate - Interpreter",
    Style::new().green().apply_to("Bench")
  );
  bench_report(&median, &p75, &p99, &sd, None);
}

#[cfg(feature = "native")]
pub fn jit_benchmark<T: BytecodeResolver + Send + Sync + 'static>(
  vm: &VM<T>,
  jitdata: &mut JITMems,
  sectionid: u64,
  rounds: u64,
) {
  let mut store = Vec::with_capacity(rounds as usize);

  let mut runtest = |jit: &mut JITMemData,
                     compilers: &[(&'static str, &'static dyn NativeCompilerBuilder<true>)],
                     outarc: &[PickleInstruction]| {
    for &(name, _) in compilers {
      let (exec, compile) = jit.ptrstore.get(&(sectionid, name)).unwrap();

      for _ in 0..rounds {
        clean();

        let t0 = Instant::now();
        run_jit(vm, &*outarc, *exec as *const Executable, name);
        let tf = t0.elapsed();
        store.push(tf.as_secs_f64());
      }

      let mut statdata = Data::new(store.as_mut_slice());

      let p99 = format_duration(statdata.percentile(99));
      let p75 = format_duration(statdata.percentile(75));
      let median = format_duration(statdata.median());
      let sd = format_duration(statdata.std_dev().unwrap_or(f64::NEG_INFINITY));
      let compile = format_duration(compile.as_secs_f64());

      println!(
        "{:>14} Tier : {name}",
        Style::new().green().apply_to("Bench")
      );
      bench_report(&median, &p75, &p99, &sd, Some(&compile));
    }
  };

  let outarc = jitdata.picklestore.get(&sectionid).unwrap();
  runtest(
    &mut jitdata.general,
    testing_compiler_infra::<true, Resolver>(),
    &*outarc,
  );

  for (idx, &compiler) in testing_epitier_compilers::<true>().iter().enumerate() {
    runtest(&mut jitdata.epitier[idx], &[compiler], &*outarc);
  }
}

fn format_duration(seconds: f64) -> String {
  if seconds >= 1.0 {
    format!("{:.3} s", seconds)
  } else if seconds >= 0.001 {
    format!("{:.3} ms", seconds * 1e3)
  } else if seconds >= 0.000001 {
    format!("{:.3} µs", seconds * 1e6)
  } else {
    format!("{:.3} ns", seconds * 1e9)
  }
}
