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
fn llvm_config(args: &[&str], prepend: bool) -> String {
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

  let sysroot = var("SAJIT_SYSROOT").ok();
  let out = if let Some(sysroot) = &sysroot {
    Command::new("bwrap")
      .args(&[
        "--ro-bind",
        &sysroot,
        "/",
        "--proc",
        "/proc",
        "--dev",
        "/dev",
        "/usr/bin/llvm-config",
      ])
      .args(args)
      .output()
      .expect("llvm-config not found in PATH")
  } else {
    Command::new(&program)
      .args(args)
      .output()
      .expect("llvm-config not found in PATH")
  };

  if !out.status.success() {
    panic!(
      "Running `{}` failed. Kindly check if llvm-config correctly runs on your system.\n\nStdErr: {}",
      program.display(),
      String::from_utf8_lossy(&out.stderr)
    );
  }

  if prepend && let Some(sysroot) = sysroot {
    sysroot + str::from_utf8(&out.stdout).expect("Invalid UTF8")
  } else {
    String::from_utf8(out.stdout).expect("Invalid UTF8 was provided")
  }
}

#[cfg(feature = "llvm")]
fn build_ssaupdater() {
  use cc::Build;
  use std::env::var;

  println!("cargo::rerun-if-changed=srcxx");
  println!("cargo::rerun-if-env-changed=SAJIT_SYSROOT");

  let include_llvm = llvm_config(&["--includedir"], true);

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

  // LLVM CRITICAL
  {
    let sysroot = var("SAJIT_SYSROOT").map(|x| format!("{x}/")).unwrap_or_default();
    let ldflags = llvm_config(&["--ldflags"], false);

    for flag in ldflags.split_whitespace() {
      if let Some(path) = flag.strip_prefix("-LIBPATH:").or_else(|| flag.strip_prefix("-L")) {
        println!("cargo:rustc-link-search=native={}{}", sysroot, path);
      }
    }

    let libs = llvm_config(&["--link-static", "--libs"], false);
    for lib in libs.split_whitespace() {
      #[cfg(windows)]
      if let Some((dir, name)) = lib.rsplit_once("\\") {
        println!("cargo:rustc-link-search={}", dir);

        if let Some(name) = name.strip_suffix(".lib") {
          println!("cargo:rustc-link-lib=static={}", name);
        }
      }

      #[cfg(not(windows))]
      if let Some(dir) = lib.strip_prefix("-L") {
        println!("cargo:rustc-link-search=native={}", dir);
      } else if let Some(name) = lib.strip_prefix("-l") {
        println!("cargo:rustc-link-lib=static={}", name);    
      }
    }

    let libs = llvm_config(&["--link-static", "--system-libs"], false);
    for lib in libs.split_whitespace() {
      #[cfg(windows)]
      if let Some(name) = lib.strip_suffix(".lib") {
        println!("cargo:rustc-link-lib={}", name);
      }

      #[cfg(not(windows))]
      if let Some(dir) = lib.strip_prefix("-L") {
        println!("cargo:rustc-link-search=native={}", dir);
      } else if let Some(name) = lib.strip_prefix("-l") {
        println!("cargo:rustc-link-lib={}", name);    
      }
    }
  }

  println!("cargo:rustc-link-lib=static=srcxx");
}
