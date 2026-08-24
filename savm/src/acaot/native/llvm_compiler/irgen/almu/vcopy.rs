use llvm_sys::core::{LLVMBuildMemCpy, LLVMBuildMemMove, LLVMConstInt, LLVMSetVolatile};

use crate::acaot::{
  native::llvm_compiler::{
    CompilerMeta,
    irgen::{
      OffsetBytes, offsetload, offsetptr, offsetstore,
      reg::{
        LLVMTypeOrWidth, SrcType, StoreResolver, llvmload_src, llvmresolve_location_src_ptr,
        llvmresolve_location_src_store,
      },
    },
  },
  pickle::{
    def::PickleInstruction,
    reader::corevm::{Count, VCOPY, parse_vcopy},
  },
};

pub fn handle_vcopy(pickle: &PickleInstruction, meta: &mut CompilerMeta) {
  let VCOPY {
    src,
    target,
    count,
    src_offset,
    target_offset,
    volatile,
    overlapping,
    src_align,
    target_align,
  } = parse_vcopy(pickle, meta.ws.as_ref());

  let typ = LLVMTypeOrWidth::Type(3);

  match count {
    Count::Abs(count) => {
      let src = llvmresolve_location_src_ptr(meta, typ, src, Some(src_align), src_offset, count);
      let store =
        llvmresolve_location_src_store(meta, typ, target, Some(target_align), target_offset, count);

      let is_ptr =
        matches!(store, StoreResolver::Ptr(_, _, _)) && matches!(src, SrcType::Pointer { .. });

      if is_ptr {
        let SrcType::Pointer {
          pointerval: srcptr,
          builder,
          llvmctx,
          ..
        } = src
        else {
          unreachable!();
        };

        let StoreResolver::Ptr(output, offset, _) = store else {
          unreachable!();
        };
        let target = offsetptr(builder, llvmctx, output, offset.cast_unsigned(), true);

        unsafe {
          let volatile = if volatile { 1 } else { 0 };

          if overlapping {
            let memmove = LLVMBuildMemMove(
              builder,
              target,
              target_align as _,
              srcptr,
              src_align as _,
              LLVMConstInt(meta.i32, count as _, 0),
            );

            LLVMSetVolatile(memmove, volatile);
          } else {
            let memmove = LLVMBuildMemCpy(
              builder,
              target,
              target_align as _,
              srcptr,
              src_align as _,
              LLVMConstInt(meta.i32, count as _, 0),
            );

            LLVMSetVolatile(memmove, volatile);
          }
        }
      } else {
        store.synchronize(meta, llvmload_src(src));
      }
    }
    Count::Runtime => unsafe {
      if src < 8 || target < 8 {
        let spill = meta.regspill;

        (0..8).for_each(|regid| {
          let regval = meta.regmnt.usereg(regid);

          offsetstore(
            meta.builder,
            meta.llvmctx,
            regval,
            spill,
            OffsetBytes::U(regid as u64 * 8),
          );
        });
      }

      let srcdata = llvmresolve_location_src_ptr(meta, typ, src, Some(src_align), src_offset, 1);
      let targetdata =
        llvmresolve_location_src_ptr(meta, typ, target, Some(target_align), target_offset, 1);

      let (srcptr, srcalign) = match srcdata {
        SrcType::Pointer {
          pointerval,
          alignment,
          ..
        } => (pointerval, alignment),
        SrcType::RegMap { .. } => {
          let ptr = offsetptr(
            meta.builder,
            meta.llvmctx,
            meta.regspill,
            8 * src as u64 + src_offset as u64,
            false,
          );

          (ptr, None)
        }
      };

      let (targetptr, targetalign) = match targetdata {
        SrcType::Pointer {
          pointerval,
          alignment,
          ..
        } => (pointerval, alignment),
        SrcType::RegMap { .. } => {
          let ptr = offsetptr(
            meta.builder,
            meta.llvmctx,
            meta.regspill,
            8 * target as u64 + target_offset as u64,
            false,
          );

          (ptr, None)
        }
      };

      let volatile = if volatile { 1 } else { 0 };

      if overlapping {
        let memmove = LLVMBuildMemMove(
          meta.builder,
          targetptr,
          targetalign.map(|x| x as _).unwrap_or(1),
          srcptr,
          srcalign.map(|x| x as _).unwrap_or(1),
          meta.regmnt.usereg(0),
        );

        LLVMSetVolatile(memmove, volatile);
      } else {
        let memmove = LLVMBuildMemCpy(
          meta.builder,
          targetptr,
          targetalign.map(|x| x as _).unwrap_or(1),
          srcptr,
          srcalign.map(|x| x as _).unwrap_or(1),
          meta.regmnt.usereg(0),
        );

        LLVMSetVolatile(memmove, volatile);
      }

      // Load back updated values!
      if src < 8 || target < 8 {
        let spill = meta.regspill;

        (0..8).for_each(|regid| {
          let regval = offsetload(
            meta.builder,
            meta.llvmctx,
            meta.i64,
            spill,
            OffsetBytes::U(regid as u64 * 8),
          );

          meta.regmnt.setreg(regid, regval)
        });
      }
    },
  }
}
