// Arithmatic Logic Memory Unit

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta,
    irgen::reg::{LLVMTypeOrWidth, llvmresolve_location_src_load},
  },
  pickle::{
    def::PickleInstruction,
    reader::au::{DIVLIKE, parse_divlike},
  },
};

#[macro_export]
macro_rules! llvmreadws {
  ($meta:expr, start = $start:expr, stop = $stop:expr, $t:ty) => {
    <$t>::from_ne_bytes($meta.ws[$start..$stop].try_into().unwrap())
  };
}

pub fn handle_div(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let DIVLIKE {
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_divlike(pickle, meta.ws.as_ref());

  let datatype = LLVMTypeOrWidth::Type(datatype);

  let src1 = llvmresolve_location_src_load(meta, datatype, src1, None, of_src1, 1);
}
