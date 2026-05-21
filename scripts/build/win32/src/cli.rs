use std::env::args;

use crate::inst::{install_info,uninstall};

mod inst;

fn main() {
  if let Some(_) = args().find(|x| x as &str == "uninstall") {
    uninstall::<_, false>(|| {});
    println!("Successfully uninstalled! A reboot is required to completely clean up stray files.");
    return;
  }

  install_info::<_, _, false>(
    |x, y| {
      println!("({:06.2}%) {x}", y * 100.0);
    },
    || {
      println!("Successfully Installed!");
    },
  );
}
