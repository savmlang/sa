fn main() {
  println!("cargo:rerun-if-env-changed=TARGET_PKG_ZIP");
}
