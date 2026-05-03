use crate::acaot::{
  LocSrc, SigStore,
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{TypeOrWidth, resolve_location_src_store, resolve_reg},
  },
  pickle::{
    def::PickleInstruction,
    reader::spawn::{SPAWN, parse_spawn},
  },
};
use cranelift::prelude::{
  types::{I8, I64},
  *,
};
use sart::structures::QuadPackedData;

pub fn hwnd_spawn(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let SPAWN {
    section,
    launch_as_async,
    return_hwnd,
    out_loc,
  } = parse_spawn(&pickle, meta.ws.as_ref());

  // pub extern "C" fn savm_spawn(
  //   taskstate: *mut VMTaskState,
  //   section: u64,
  //   launch_async: bool,
  //   return_hwnd: bool,
  // ) -> *mut c_void
  let sig = *meta.sigref.entry(SigStore::VMSpawn).or_insert_with(|| {
    let sign = Signature {
      params: vec![
        AbiParam::new(meta.isa.pointer_type()),
        AbiParam::new(I64),
        AbiParam::new(I8),
        AbiParam::new(I8),
      ],
      returns: vec![AbiParam::new(meta.isa.pointer_type())],
      call_conv: meta.callconv,
    };

    builder.import_signature(sign)
  });

  let libfn = meta.get_fn(builder, LocSrc::VMSpawn, sig);

  // Spill all registers back!
  (0..8).for_each(|regid| {
    let rg = resolve_reg(builder, meta, regid);
    let rgval = builder.use_var(rg);

    builder.ins().stack_store(
      rgval,
      meta.regspill,
      regid as i32 * size_of::<QuadPackedData>() as i32,
    );
  });

  let vmtsk = builder.ins().stack_addr(I64, meta.regspill, 0);
  let section = builder.ins().iconst(I64, section.cast_signed());
  let launch_async = builder
    .ins()
    .iconst(I8, if launch_as_async { 1 } else { 0 });
  let return_as_hwnd = builder.ins().iconst(I8, if return_hwnd { 1 } else { 0 });

  let fncall = builder
    .ins()
    .call(libfn, &[vmtsk, section, launch_async, return_as_hwnd]);

  if return_hwnd {
    let mut outloc =
      resolve_location_src_store(builder, meta, TypeOrWidth::Width(0), out_loc, None, 0, 1);

    let vals = builder.inst_results(fncall);

    outloc.store(builder, 0, vals[0]);

    outloc.synchronize(builder, meta);
  }
}
