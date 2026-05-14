use std::mem::offset_of;

use cranelift::prelude::{
  AbiParam, FunctionBuilder, InstBuilder, MemFlags, Signature, Value,
  types::{I8, I32, I64},
};
use sart::ctr::VMTaskState;

use crate::acaot::{
  native::cranelift::{
    CompilerMeta, LocSrc, SigStore,
    irgen::reg::{
      TypeOrWidth, resolve_loc_to_ptr, resolve_location_src_load, resolve_location_src_store,
      resolve_reg,
    },
  },
  pickle::{
    def::PickleInstruction,
    reader::corevm::{Count, SCRATCH, VCOPY, parse_scratch, parse_vcopy},
  },
};

pub fn hwnd_vcopy(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let vcopy = parse_vcopy(&pickle, meta.ws.as_ref());

  let VCOPY {
    src,
    target,
    count,
    src_offset,
    target_offset,
    overlapping,
    src_align,
    target_align,
  } = vcopy;

  match count {
    // Keep it manual for upto 1024bytes
    Count::Abs(count) if count <= 1024 => {
      let typ = TypeOrWidth::Type(3);

      let src =
        resolve_location_src_load(builder, meta, typ, src, Some(src_align), src_offset, count);

      if overlapping {
        builder.ins().fence();
      }

      let mut target = resolve_location_src_store(
        builder,
        meta,
        typ,
        target,
        Some(target_align),
        target_offset,
        count,
      );

      src
        .into_iter()
        .enumerate()
        .for_each(|(idx, val)| target.store(builder, idx, val));

      target.synchronize(builder, meta);
    }

    _count => hwnd_vcopy_libccall(builder, meta, pickle, vcopy),
  }
}

fn hwnd_vcopy_libccall(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  _: PickleInstruction,
  vcopy: VCOPY,
) {
  let VCOPY {
    src,
    target,
    count,
    src_offset,
    target_offset,
    overlapping,
    ..
  } = vcopy;

  let count: Value = match count {
    Count::Abs(count) => builder.ins().iconst(I32, count as i64),
    Count::Runtime => {
      let reg = resolve_reg(builder, meta, 0);
      // r1 is I64
      // We fetch its low bits
      let r1 = builder.use_var(reg);

      builder.ins().ireduce(I32, r1)
    }
  };

  let sign = *meta.sigref.entry(SigStore::VCopyCommon).or_insert_with(|| {
    let callconv = meta.callconv;
    let mut vmmemsig = Signature::new(callconv);

    {
      let ptr = meta.isa.pointer_type();
      vmmemsig.params.push(AbiParam::new(ptr));
      vmmemsig.params.push(AbiParam::new(ptr));
      vmmemsig.params.push(AbiParam::new(I32));
    }

    let re = builder.import_signature(vmmemsig);

    re
  });

  let ffifn = meta.get_fn(
    builder,
    if overlapping {
      LocSrc::VCopyOverlapping
    } else {
      LocSrc::VCopyNoAlias
    },
    sign,
  );

  let src = resolve_loc_to_ptr(builder, meta, src, src_offset);
  let target = resolve_loc_to_ptr(builder, meta, target, target_offset);

  builder.ins().call(ffifn, &[src.ptr, target.ptr, count]);

  src.no_sync();
  target.sync(builder, meta);
}

pub fn hwnd_scratch(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: &PickleInstruction,
) {
  let scratch = parse_scratch(pickle, meta.ws.as_ref());

  let sign = *meta.sigref.entry(SigStore::VMScratch).or_insert_with(|| {
    let callconv = meta.callconv;
    let mut vmmemsig = Signature::new(callconv);

    //extern "C" fn scratch_ffi(op: u8, engine: *mut WorkingSet, arg1: *mut QuadPackedData, arg2: usize) -> *mut QuadPackedData
    {
      let ptr = meta.isa.pointer_type();
      vmmemsig.params.push(AbiParam::new(I8));
      vmmemsig.params.push(AbiParam::new(ptr));
      vmmemsig.params.push(AbiParam::new(ptr));
      vmmemsig.params.push(AbiParam::new(ptr));

      // Ret
      vmmemsig.returns.push(AbiParam::new(ptr));
    }

    let re = builder.import_signature(vmmemsig);

    re
  });

  let fref = meta.get_fn(builder, LocSrc::VMScratchAction, sign);

  let ws = builder.ins().load(
    meta.isa.pointer_type(),
    MemFlags::trusted(),
    meta.vmtaskstate,
    offset_of!(VMTaskState, ws_or_pt2) as i32,
  );

  let largepad = match scratch {
    SCRATCH::Allocate {
      size_reg,
      align_reg,
    } => {
      let size = resolve_reg(builder, meta, size_reg);
      let size = builder.use_var(size);

      let align = resolve_reg(builder, meta, align_reg);
      let align = builder.use_var(align);

      let op = builder.ins().iconst(I8, 0);

      builder.ins().call(fref, &[op, ws, size, align])
    }
    // Drop classic
    SCRATCH::DropClassic => {
      let nil = builder.ins().iconst(I64, 0);
      let op = builder.ins().iconst(I8, 1);

      builder.ins().call(fref, &[op, ws, nil, nil])
    }
    // Drop (alignment was given at alloc)
    SCRATCH::DropAligned => {
      let nil = builder.ins().iconst(I64, 0);
      let op = builder.ins().iconst(I8, 2);

      builder.ins().call(fref, &[op, ws, nil, nil])
    }
  };

  let largepad = builder.inst_results(largepad)[0];

  builder.def_var(meta.largepad, largepad);
}
