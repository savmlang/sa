use std::env::var;

use bindgen::builder;
use cc::Build;

fn main() {
  // Bindings
  {
    let builder = builder().header("./src_lmdb/common.h").generate().unwrap();

    let mut outdir = var("OUT_DIR").unwrap();
    outdir.push_str("/bindings.rs");

    builder.write_to_file(&outdir).unwrap();
  }

  // Build lmdb
  {
    let target = std::env::var("TARGET").unwrap_or_default();
    let mut builder = Build::new();

    builder
      .cpp(false)
      .cargo_metadata(true)
      .files(["./src_lmdb/mdb.c", "./src_lmdb/midl.c"]);

    if target.contains("linux") || target.contains("bsd") {
      builder.flag("-pthread");
    }

    builder.compile("lmdbbuild");

    if target.contains("linux") || target.contains("bsd") {
      println!("cargo:rustc-link-lib=pthread");
    }
  }
}
