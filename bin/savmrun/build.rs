fn main() {
  let target = std::env::var("TARGET").unwrap();
  println!("cargo:rustc-env=BUILD_TARGET={}", target);

  // Re-run if features change
  println!("cargo:rerun-if-changed=Cargo.toml");
}
