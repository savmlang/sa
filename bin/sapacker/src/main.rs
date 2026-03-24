use std::{env::var, time::Instant};

use console::Style;

use crate::sql::setup_db;

mod bytecode;
mod libdata;
mod metadata;
mod resolver;
mod sql;

fn main() {
  let t0 = Instant::now();

  let distdir = var("SAVM_TARGET").unwrap_or("dist".into());

  let (mut dist, sabin) = setup_db(&distdir);

  sabin.execute_batch("BEGIN TRANSACTION;").unwrap();

  // Process
  bytecode::emit(&mut dist, &sabin);

  // Metadata
  metadata::emit(&mut dist, &sabin);

  // Parse libraries
  libdata::emit(&mut dist, &sabin);

  // Update LibFn maps
  resolver::emit(&mut dist, &sabin);

  sabin.execute("COMMIT", []).unwrap();

  sabin
    .execute_batch(
      "
    PRAGMA journal_mode = DELETE;
    PRAGMA page_size = 4096;
    VACUUM;",
    )
    .unwrap();

  let tf = t0.elapsed();

  let b_bold = Style::new().blue().bold();
  let y = Style::new().yellow();

  println!(
    "{:>12} in {}",
    b_bold.apply_to("Packed"),
    y.apply_to(format!("{tf:?}"))
  );
}
