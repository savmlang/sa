fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=src_cinder");

  #[cfg(all(
    feature = "cranelift",
    not(any(
      target_arch = "x86_64",
      target_arch = "aarch64",
      target_arch = "riscv64"
    ))
  ))]
  compile_error!("Cranelift is ONLY supported for x86_64, aarch64, riscv64 architectures");

  // Cinder
  #[cfg(feature = "native")]
  build_cinder();

  // LLVM SSAUpdater
  #[cfg(feature = "llvm")]
  build_ssaupdater();
}

#[cfg(feature = "native")]
fn build_cinder() {
  use cc::Build;
  use savmbuild_cinder::TargetMachine;
  use std::{env::var, fs};

  let arch = var("CARGO_CFG_TARGET_ARCH").unwrap();
  let supported = ["x86_64"];

  if supported.contains(&&*arch) {
    let mut defcompiler = Build::new();
    defcompiler
      .compiler("clang")
      .pic(false)
      .flags(["-std=c23", "-O3", "-mcmodel=large", "-g0"])
      .flag("-fno-pie")
      .flags([
        "-fno-builtin",
        "-fno-addrsig",
        "-fno-unwind-tables",
        "-fno-asynchronous-unwind-tables",
        "-fno-function-sections",
        "-fno-data-sections",
        "-fomit-frame-pointer",
        "-fno-stack-protector",
        "-fno-jump-tables",
        "-fno-signed-zeros",
        "-fno-vectorize",
      ])
      .include("src_cinder");

    let out_dir = var("OUT_DIR").unwrap();

    let srcs = fs::read_dir("./src_cinder")
      .unwrap()
      .map(Result::unwrap)
      .map(|x| (x.file_name().into_string().unwrap(), x.path()))
      .filter(|(x, _)| x.ends_with(".c"))
      .collect::<Box<_>>();
    let mut built = vec![];

    // Compilation
    for (name, path) in &srcs {
      defcompiler.clone().file(path).compile(&name);
      built.push(name.trim_end_matches(".c"));
    }

    let outputs = fs::read_dir(&out_dir)
      .unwrap()
      .map(Result::unwrap)
      .map(|x| (x.file_name().into_string().unwrap(), x.path()))
      .filter(|(x, _)| x.ends_with(".o"))
      .collect::<Box<_>>();

    let machine = TargetMachine {
      arch_32: arch == "x86",
      arm64: arch == "aarch64",
    };

    let stencils = built
      .iter()
      .map(|name| {
        let (_, path) = outputs
          .iter()
          .find(|&(f, _)| f.trim_end_matches(".o").ends_with(name))
          .expect("Unable to locate build");

        savmbuild_cinder::stenload(path, name, machine)
      })
      .collect::<Box<_>>();

    let file = savmbuild_cinder::emit::cinderjit_file(&stencils);

    fs::write(format!("{}/cinderjit.rs", out_dir), file).unwrap();
  }
}

#[cfg(feature = "llvm")]
fn build_ssaupdater() {
  use cc::Build;
  use savmbuild::llvm_config;

  #[cfg(target_os = "macos")]
  use std::path::Path;

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

  savmbuild::link_llvm();
  println!("cargo:rustc-link-lib=static=srcxx");
}
