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
fn llvm_config(args: &[&str]) -> String {
  use std::{borrow::Cow, env::var, ffi::OsStr, process::Command};

  let vars = [
    var("SAJIT_LLVM_PATH"),
    var("LLVM_SYS_221_PREFIX"),
    var("LLVM_SYS_211_PREFIX"),
  ];

  let program: Cow<'static, OsStr> = if let Some(Ok(path)) = vars.into_iter().find(|v| v.is_ok()) {
    use std::{env::consts::EXE_SUFFIX, path::PathBuf};

    let mut path: PathBuf = PathBuf::from(path);

    path.push("bin");
    path.push(format!("llvm-config{}", EXE_SUFFIX));

    Cow::Owned(path.into_os_string())
  } else {
    Cow::Borrowed(OsStr::new("llvm-config"))
  };

  let out = Command::new(&program)
    .args(args)
    .output()
    .expect("llvm-config not found in PATH");

  if !out.status.success() {
    panic!(
      "Running `{}` failed. Kindly check if llvm-config correctly runs on your system.\n\nStdErr: {}",
      program.display(),
      String::from_utf8_lossy(&out.stderr)
    );
  }

  String::from_utf8(out.stdout).expect("Invalid UTF8 was provided")
}

#[cfg(feature = "llvm")]
fn build_ssaupdater() {
  use cc::Build;

  println!("cargo::rerun-if-changed=ssaupdater");

  let include_llvm = llvm_config(&["--includedir"]);

  let mut build = Build::new();

  build
    .cpp(true)
    .std("c++20")
    .file("./ssaupdater/updater.cpp")
    .include("ssaupdater")
    .include(include_llvm.trim());

  let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();

  if target_os == "linux" || target_os == "darwin" || target_os == "macos"
  {
    build.flag("-fno-rtti");
  }

  build.compile("ssaupdater");
}
