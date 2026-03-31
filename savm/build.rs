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
}
