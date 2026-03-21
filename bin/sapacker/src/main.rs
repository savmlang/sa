use std::env::var;

use crate::sql::setup_db;

mod bytecode;
mod sql;

fn main() {
  let distdir = var("SAVM_TARGET").unwrap_or("dist".into());

  let (mut dist, sabin) = setup_db(&distdir);

  sabin.execute_batch("BEGIN TRANSACTION;").unwrap();

  // Process
  bytecode::emit(&mut dist, &sabin);

  sabin
    .execute_batch(
      "
    COMMIT;
    VACUUM;
  ",
    )
    .unwrap();
}
