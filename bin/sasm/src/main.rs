#![allow(deprecated)]

use std::{borrow::Cow, cmp::Ordering, collections::HashMap, fs, sync::OnceLock};

use console::Style;
use indicatif::{ProgressBar, ProgressStyle};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::assembler::{assemble, macros::MacroJIT};

mod assembler;

pub static GLOB_MACROS: OnceLock<HashMap<&'static str, Cow<'static, MacroJIT<'static>>>> =
  OnceLock::new();

fn main() {
  let mut files = fs::read_dir("./bin")
    .unwrap()
    .map(|x| x.unwrap())
    .collect::<Vec<_>>();

  let mut has_macros = false;
  // Send macros to the last element
  files.sort_by(|a, b| {
    let a = a.file_name();
    let b = b.file_name();
    let is_a_macros = a == "macros";
    let is_b_macros = b == "macros";

    if is_a_macros {
      has_macros = true;
      Ordering::Greater
    } else if is_b_macros {
      has_macros = true;
      Ordering::Less
    } else {
      a.cmp(&b)
    }
  });

  if has_macros {
    let macrosfile = unsafe { files.pop().unwrap_unchecked() };

    let static_str = Box::leak(
      fs::read_to_string(macrosfile.path())
        .expect("Unable to read and parse macros file")
        .into_boxed_str(),
    );

    let macros = assemble(static_str).macros;

    GLOB_MACROS.set(macros).expect("Impossible to err");
  } else {
    GLOB_MACROS
      .set(Default::default())
      .expect("Impossible to err");
  }

  let green_bold = Style::new().green().bold();

  let pb = ProgressBar::new(files.len() as _);
  pb.set_style(
    ProgressStyle::with_template("{prefix:>12.cyan.bold} [{bar:40}] {pos}/{len}")
      .unwrap()
      .progress_chars("=> "),
  );
  pb.set_prefix("Assembling");

  files.into_par_iter().for_each(|x| {
    let fl = x.file_name().into_string().unwrap().parse::<u64>().unwrap();
    let cnt = fs::read_to_string(x.path()).unwrap().into_boxed_str();

    pb.suspend(|| {
      println!(
        "{:>12} {fl} (size={})",
        green_bold.apply_to("Compiling"),
        cnt.len()
      )
    });

    fs::write(format!("./dist/{fl}"), assemble(&cnt).out).unwrap();

    // Compiled
    pb.inc(1);
  });

  pb.abandon();
}
