pub fn llvm_config(args: &[&str], prepend: bool) -> String {
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

pub fn link_llvm() {
  use cc::Build;
  use std::env::var;

  #[cfg(target_os = "macos")]
  use std::path::Path;

  println!("cargo::rerun-if-changed=srcxx");
  println!("cargo::rerun-if-env-changed=SAJIT_SYSROOT");

  // macOS
  #[cfg(target_os = "macos")]
  {
    let brew_paths = [
      "/opt/homebrew/opt/zstd/lib", // Apple Silicon Homebrew location
      "/usr/local/opt/zstd/lib",    // Intel Homebrew location
      "/opt/homebrew/lib",          // Apple Silicon default lib
      "/usr/local/lib",             // Intel default lib
    ];

    for path in brew_paths {
      if Path::new(path).exists() {
        println!("cargo:rustc-link-search=native={path}");
      }
    }
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
    
    
    #[cfg(not(windows))]
    let linker_link = |lib: &str, addn: &str| {
      if let Some(dir) = lib.strip_prefix("-L") {
        if !dir.is_empty() {
          println!("cargo:rustc-link-search=native={}{}", sysroot, dir);
        }
      } else if let Some(name) = lib.strip_prefix("-l") {
        if !name.is_empty() {
          println!("cargo:rustc-link-lib{addn}={}", name);
        }
      }
    };
    
    for lib in libs.split_whitespace() {
      let lib = lib.trim();
      #[cfg(windows)]
      if let Some((dir, name)) = lib.rsplit_once("\\") {
        println!("cargo:rustc-link-search={}", dir);

        if let Some(name) = name.strip_suffix(".lib") {
          println!("cargo:rustc-link-lib=static={}", name);
        }
      }

      #[cfg(not(windows))]
      linker_link(lib, "=static");
    }

    let libs = llvm_config(&["--link-static", "--system-libs"], false);
    for lib in libs.split_whitespace() {
      let lib = lib.trim();
      #[cfg(windows)]
      if let Some(name) = lib.strip_suffix(".lib") {
        println!("cargo:rustc-link-lib={}", name);
      }

      #[cfg(not(windows))]
      linker_link(lib, "");
    }
  }

  println!("cargo:rustc-link-lib=static=srcxx");
}
