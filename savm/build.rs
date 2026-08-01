fn main() {
  #[cfg(all(
    feature = "cranelift",
    not(any(
      target_arch = "x86_64",
      target_arch = "aarch64",
      target_arch = "riscv64"
    ))
  ))]
  compile_error!("Cranelift is ONLY supported for x86_64, aarch64, riscv64 architectures");

  #[cfg(feature = "llvm")]
  build_ssaupdater();
}

#[cfg(feature = "llvm")]
fn build_ssaupdater() {
  use cc::Build;
  use std::env::var;

  #[cfg(target_os = "macos")]
  use std::path::Path;

  println!("cargo::rerun-if-changed=srcxx");
  println!("cargo::rerun-if-env-changed=SAJIT_SYSROOT");

  let include_llvm = savmbuild::llvm_config(&["--includedir"], true);

  // `srcxx` building
  {
    let mut build = Build::new();
    build
      .cpp(true)
      .std("c++20")
      .file("./srcxx/blockpreds.cpp")
      .include("srcxx")
      .include(include_llvm.trim());
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "linux" || target_os == "darwin" || target_os == "macos" {
      build.flag("-fno-rtti");
    }

    let cxxflags = llvm_config(&["--cxxflags"], false);

    // 2. Parse flags from llvm-config --cxxflags (macro definitions & RTTI settings)
    for flag in cxxflags.split_whitespace() {
      if flag.starts_with("-D") {
        let mut parts = flag[2..].splitn(2, '=');
        let name = parts.next().unwrap();
        let value = parts.next();
        build.define(name, value);
      } else if flag == "-fno-rtti" || flag == "-fno-exceptions" {
        build.flag(flag);
      }
    }

    build.compile("srcxx");
  }

  savmbuild::link_llvm();
  println!("cargo:rustc-link-lib=static=srcxx");
}
