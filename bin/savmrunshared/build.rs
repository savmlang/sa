fn main() {
  #[cfg(feature = "llvm")]
  savmbuild::link_llvm();
}