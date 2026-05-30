use crate::acaot::native::llvm_compiler::CompilerMeta;

#[inline(always)]
pub fn resolve_location_src_load(
  compiler: &mut CompilerMeta,
  typ: LLVMTypeOrWidth,

  // Location-src
  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,
) {
}

#[derive(Debug, Clone, Copy)]
pub enum LLVMTypeOrWidth {
  Type(u8),
  Width(u8),
}
