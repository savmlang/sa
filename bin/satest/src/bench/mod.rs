use console::Style;
use savm::VM;
#[cfg(feature = "native")]
use savm::acaot::native::testing_compiler_infra;
use statrs::statistics::{Data, Distribution, OrderStatistics};
use std::time::Instant;

#[cfg(feature = "native")]
use crate::jitmem::JITMemData;

pub fn interpreter_benchmark(vm: &VM, sectionid: u64, rounds: u64) {
  let mut store = Vec::with_capacity(rounds as usize);
  for _ in 0..rounds {
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
  println!("{:>16} {median}", Style::new().yellow().apply_to("Median"));
  println!("{:>16} {p75}", Style::new().yellow().apply_to("p75"));
  println!("{:>16} {p99}", Style::new().yellow().apply_to("p99"));
  println!("{:>16} {sd}", Style::new().white().apply_to("SD"));
}

#[cfg(feature = "native")]
pub fn jit_benchmark(vm: &VM, jit: &mut JITMemData, sectionid: u64, rounds: u64) {
  let mut store = Vec::with_capacity(rounds as usize);

  for (name, _) in testing_compiler_infra() {
    let (exec, compile) = jit.ptrstore.get(&(sectionid, *name)).unwrap();

    for _ in 0..rounds {
      let t0 = Instant::now();
      vm.exec_jit(*exec as _);
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
    println!("{:>16} {median}", Style::new().yellow().apply_to("Median"));
    println!("{:>16} {p75}", Style::new().yellow().apply_to("p75"));
    println!("{:>16} {p99}", Style::new().yellow().apply_to("p99"));
    println!("{:>16} {sd}", Style::new().white().apply_to("SD"));
    println!("{:>16} {compile}", Style::new().red().apply_to("Compile"));
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
