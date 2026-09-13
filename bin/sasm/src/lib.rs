use std::{
  borrow::Cow,
  cmp::Ordering,
  path::{Path, PathBuf},
  sync::{Arc, atomic::AtomicU64},
  thread::{self, sleep},
  time::{Duration, Instant},
};

use console::Style;
use indicatif::{ProgressBar, ProgressStyle};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::assembler::assemble;

mod assembler;

pub struct PathInfo<'a> {
  pub bindir: Cow<'a, str>,
  pub distdir: Cow<'a, str>,
}

pub struct DirFsEntry {
  pub name: Box<str>,
  pub path: PathBuf,
}

pub struct FS;
impl FileSystemImpl for FS {}

pub trait FileSystemImpl: Send + Sync {
  fn readdir<P: AsRef<Path>, T, F>(&self, path: P, cb: F) -> T
  where
    F: FnOnce(&mut dyn Iterator<Item = DirFsEntry>) -> T,
  {
    use std::fs;
    let mut val = fs::read_dir(path).unwrap().map(|x| {
      let x = x.unwrap();

      DirFsEntry {
        name: x.file_name().into_string().unwrap().into_boxed_str(),
        path: x.path(),
      }
    });

    cb(&mut val)
  }

  fn mkdir<P: AsRef<Path>>(&self, path: P) -> Option<()> {
    use std::fs;
    fs::create_dir_all(path).ok()
  }

  fn read_to_string<P: AsRef<Path>>(&self, path: P) -> Option<String> {
    use std::fs;
    fs::read_to_string(path).ok()
  }

  fn write<P: AsRef<Path>, C: AsRef<[u8]>>(&self, path: P, contents: C) -> Option<()> {
    use std::fs;
    fs::write(path, contents).ok()
  }
}

pub fn sasm<'a, T: FileSystemImpl>(rt: PathInfo<'a>, fs: T) {
  let t0 = Instant::now();
  let mut files = fs.readdir(rt.bindir.as_ref(), |x| x.collect::<Vec<_>>());

  _ = fs.mkdir(&*rt.distdir);

  let mut has_macros = false;
  // Send macros to the last element
  files.sort_unstable_by(|a, b| {
    let a = a.name.as_ref();
    let b = b.name.as_ref();
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

  let _none1 = Default::default();
  let _none2 = Default::default();

  let (macros, resolved) = if has_macros {
    let macrosfile = unsafe { files.pop().unwrap_unchecked() };

    let static_str = Box::leak(
      fs.read_to_string(macrosfile.path)
        .expect("Unable to read and parse macros file")
        .into_boxed_str(),
    );

    let st = assemble(static_str, &_none1, &_none2);

    (st.macros, st.resolved)
  } else {
    (Default::default(), Default::default())
  };

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
        .name
        .strip_suffix(".sasm")
        .expect("Unable to strip `.sasm` from file name")
        .parse::<u64>()
        .unwrap();
      let cnt = fs.read_to_string(x.path).unwrap();

      (fl, cnt)
    })
    // Force collection
    .for_each(|(fl, cnt)| {
      prog.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

      unsafe {
        let cnt = assemble(std::mem::transmute(&cnt as &str), &macros, &resolved).out;
        fs.write(format!("{}/{fl}", &rt.distdir), cnt).unwrap();
      }
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
