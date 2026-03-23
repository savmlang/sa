#![allow(unused)]

use std::{
  borrow::Cow,
  cmp::Ordering,
  collections::HashMap,
  fs,
  sync::{Arc, OnceLock, atomic::AtomicU64},
  thread::{self, sleep},
  time::{Duration, Instant},
};

use console::Style;
use indicatif::{ProgressBar, ProgressStyle};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::assembler::{OutValue, assemble, macros::MacroJIT};

mod assembler;

pub static GLOB_MACROS: OnceLock<HashMap<&'static str, Cow<'static, MacroJIT<'static>>>> =
  OnceLock::new();

pub static GLOB_VALUES: OnceLock<HashMap<&'static str, OutValue>> = OnceLock::new();

fn main() {
  let t0 = Instant::now();
  let mut files = fs::read_dir("./bin")
    .unwrap()
    .map(|x| x.unwrap())
    .collect::<Vec<_>>();

  _ = fs::create_dir_all("./dist");

  let mut has_macros = false;
  // Send macros to the last element
  files.sort_by(|a, b| {
    let a = a.file_name();
    let b = b.file_name();
    let is_a_macros = a == "defs.sasm";
    let is_b_macros = b == "defs.sasm";

    if is_a_macros {
      has_macros = true;
      Ordering::Greater
    } else if is_b_macros {
      has_macros = true;
      Ordering::Less
    } else {
      Ordering::Equal
    }
  });

  if has_macros {
    let macrosfile = unsafe { files.pop().unwrap_unchecked() };

    let static_str = Box::leak(
      fs::read_to_string(macrosfile.path())
        .expect("Unable to read and parse macros file")
        .into_boxed_str(),
    );

    let st = assemble(static_str);

    GLOB_MACROS.set(st.macros).expect("Impossible to err");
    GLOB_VALUES.set(st.resolved).expect("Impossible");
  } else {
    GLOB_MACROS
      .set(Default::default())
      .expect("Impossible to err");
    GLOB_VALUES.set(Default::default()).expect("Impossible");
  }

  let green_bold = Style::new().green().bold();

  let pb = ProgressBar::new(files.len() as _);
  pb.set_style(
    ProgressStyle::with_template("{prefix:>12.cyan.bold} [{bar:40}] {pos}/{len}")
      .unwrap()
      .progress_chars("=> "),
  );
  pb.set_prefix("Assembling");

  let prog = Arc::new(AtomicU64::new(0));

  let prog2 = prog.clone();
  let t = thread::spawn(move || {
    loop {
      let progress = prog2.load(std::sync::atomic::Ordering::Relaxed);

      pb.set_position(progress);

      if progress == pb.length().unwrap() {
        break;
      }

      sleep(Duration::from_millis(100));
    }

    pb.abandon();
  });

  files
    .into_par_iter()
    .map(|x| {
      let fl = x
        .file_name()
        .into_string()
        .unwrap()
        .strip_suffix(".sasm")
        .expect("Unable to strip `.sasm` from file name")
        .parse::<u64>()
        .unwrap();
      let cnt = fs::read_to_string(x.path()).unwrap();

      (fl, cnt)
    })
    // Force collection
    .map(|(fl, cnt)| {
      prog.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

      (fl, assemble(&cnt).out)
    })
    // Force collect
    .for_each(|(fl, cnt)| {
      fs::write(format!("./dist/{fl}"), cnt).unwrap();
    });

  t.join().unwrap();

  let g_dark = Style::new().green().bold();
  let y = Style::new().yellow();

  println!(
    "{:>12} in {}",
    g_dark.apply_to("Compiled"),
    y.apply_to(format!("{:?}", Instant::now().duration_since(t0)))
  );
}
