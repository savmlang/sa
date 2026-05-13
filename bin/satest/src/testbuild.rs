use std::{
  borrow::Cow,
  env::{current_dir, set_current_dir},
  fs::File,
  io::{BufRead, BufReader},
  path::PathBuf,
};

use console::Style;
use sasm::PathInfo;

use crate::{TestHarness, err};

pub fn asmbuild(asm: File, tst: &mut TestHarness, cwd: &str) {
  let old = CwdGuard(current_dir().expect("Cannot get current dir"));
  set_current_dir(cwd).expect("Unable to set chdir");

  let reader = BufReader::new(asm);

  reader
    .lines()
    .filter_map(Result::ok)
    .filter(|x| !x.trim().starts_with(";"))
    .filter(|x| !x.trim().is_empty())
    .for_each(|unit| {
      let mut splits = unit.trim().split_whitespace();

      match splits.next().expect("Compileunit cannot be empty") {
        ":compile" => {
          let bin = splits.next().expect("bindir is mandatory");
          let dist = splits.next().expect("distdir is mandatory");

          println!(
            "{:>12} {bin}->{dist}",
            Style::new().yellow().bold().apply_to("Compiling")
          );

          sasm::sasm(PathInfo {
            bindir: Cow::Borrowed(bin),
            distdir: Cow::Borrowed(dist),
          });
        }
        ":bench" => {
          tst.bench = true;
          println!(
            "{:>12} BENCH=true",
            Style::new().blue().bold().apply_to("Configure")
          );
        }
        ":jit" => {
          #[cfg(feature = "native")]
          let cnt = "JIT=true";
          #[cfg(not(feature = "native"))]
          let cnt = "JIT=true (unsupported)";

          println!(
            "{:>12} {cnt}",
            Style::new().blue().bold().apply_to("Configure")
          );
          tst.jit = true;
        }
        ":iter" => {
          let num = splits
            .next()
            .expect("After iter, the number is required")
            .parse::<u64>()
            .expect("Iterations must be a unsigned INTEGER");
          let unit = match splits.next().expect("Unit is required!") {
            "B" | "b" => 10u64.wrapping_pow(9),
            "M" | "m" => 10u64.wrapping_pow(6),
            "K" | "k" => 10u64.wrapping_pow(3),
            "n" => 1u64,
            e => {
              err(format!("Unknown unit : {e}"));
            }
          };

          tst.iter = num.saturating_mul(unit).clamp(100, 10u64.pow(7));

          println!(
            "{:>12} Benchmark Iterations={} (from {}*{})",
            Style::new().blue().bold().apply_to("Configure"),
            tst.iter,
            num,
            unit
          );
        }
        d => {
          err(format!("Illegal compileunit : {d}"));
        }
      }
    });

  drop(old);
}

struct CwdGuard(PathBuf);

impl Drop for CwdGuard {
  fn drop(&mut self) {
    _ = set_current_dir(&self.0);
  }
}
