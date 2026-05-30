fn main() {
  let target = std::env::var("TARGET").unwrap();

  println!("cargo:rustc-env=BUILD_TARGET={}", target);

  #[cfg(all(feature = "nocache", feature = "jit"))]
  compile_error!(
    "Error: 'nocache' is incompatible with 'jit'.\nJIT compilation requires a cache directory to manage emitted machine code safely."
  );

  // Re-run if features change
  println!("cargo:rerun-if-changed=Cargo.toml");
}
