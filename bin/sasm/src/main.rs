use std::{borrow::Cow, env::args, process::exit};

use console::Style;
use sasm::PathInfo;

fn argparse() -> PathInfo<'static> {
  let mut argv = args();
  argv.next();

  let mut path = PathInfo {
    bindir: Cow::Borrowed("./bin"),
    distdir: Cow::Borrowed("./dist"),
  };

  let err = |arg: &str| {
    println!(
      "{}",
      Style::new()
        .red()
        .apply_to(format!("Unsupported argument : {arg}"))
    );
    exit(1);
  };

  for arg in argv {
    if arg.len() > 3 {
      match &arg[0..3] {
        "-b=" => {
          path.bindir = Cow::Owned(arg[3..].to_owned());
        }
        "-d=" => {
          path.distdir = Cow::Owned(arg[3..].to_owned());
        }
        d => {
          err(d);
        }
      }
    } else {
      err(&arg);
    }
  }

  path
}

fn main() {
  let file = argparse();

  sasm::sasm(file);
}
