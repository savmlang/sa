use crate::acaot::native::llvm_compiler::{
  CompilerMeta, LLVM_CTX, LLVM_VAR_NAME,
  irgen::{
    OffsetBytes, offsetload_aligned, offsetptr, offsetstore_aligned,
    reg::regmap::{RegMapOut, RegMask, load_all_vectored, regmapper},
  },
  ssaupdater::{LARGEPAD, REG_R2, REG_R3},
};
use llvm_sys::{
  core::{
    LLVMBuildBitCast, LLVMBuildExtractElement, LLVMBuildInsertElement, LLVMConstInt,
    LLVMDoubleTypeInContext, LLVMFloatTypeInContext, LLVMGetVectorSize, LLVMInt8TypeInContext,
    LLVMInt16TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMVectorType,
  },
  prelude::{LLVMBuilderRef, LLVMContextRef, LLVMTypeRef, LLVMValueRef},
};

pub mod regmap;

#[allow(unused)]
pub const REGISTER_WIDTH: u8 = 8;

pub fn llvmresolve_location_src_ptr(
  meta: &mut CompilerMeta,
  typ: LLVMTypeOrWidth,

  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,
) -> SrcType {
  unsafe {
    let typemap = typ.r#type();

    let meta_ptr = meta as *mut CompilerMeta;
    let offsetbytes = offset as i64 * typemap.width as i64;

    match locsrc {
      0..=7 => {
        let out = regmapper(locsrc, offsetbytes as _, typemap, count);

        let regsvalue = out
          .regstouched
          .map(|x| (*meta_ptr).regmnt.usereg(x as usize))
          .collect::<Box<[_]>>();

        SrcType::RegMap {
          builder: meta.builder,
          llvmctx: meta.llvmctx,
          vectmask: out.vectmask,
          typemap,
          regsvalue,
        }
      }
      // Scratchpad
      8 => {
        let ty = typ.vect(count as _);

        let pointerval = offsetptr(
          meta.builder,
          meta.llvmctx,
          meta.scratchpad,
          offsetbytes.cast_unsigned(),
          true,
        );

        SrcType::Pointer {
          builder: meta.builder,
          llvmctx: meta.llvmctx,
          pointerval,
          ty,
          alignment,
        }
      }
      // Largepad
      9 => {
        let ty = typ.vect(count as _);
        let val = (*meta_ptr).regmnt.usereg(LARGEPAD);

        let pointerval = offsetptr(
          meta.builder,
          meta.llvmctx,
          val,
          offsetbytes.cast_unsigned(),
          true,
        );
        SrcType::Pointer {
          builder: meta.builder,
          llvmctx: meta.llvmctx,
          pointerval,
          ty,
          alignment,
        }
      }
      // Read pointer through r2
      10 => {
        let ty = typ.vect(count as _);
        let val = (*meta_ptr).regmnt.usereg(REG_R2);

        let pointerval = offsetptr(
          meta.builder,
          meta.llvmctx,
          val,
          offsetbytes.cast_unsigned(),
          true,
        );

        SrcType::Pointer {
          builder: meta.builder,
          llvmctx: meta.llvmctx,
          pointerval,
          ty,
          alignment,
        }
      }
      // Read from r3
      11 => {
        let ty = typ.vect(count as _);
        let val = (*meta_ptr).regmnt.usereg(REG_R3);

        let pointerval = offsetptr(
          meta.builder,
          meta.llvmctx,
          val,
          offsetbytes.cast_unsigned(),
          true,
        );

        SrcType::Pointer {
          builder: meta.builder,
          llvmctx: meta.llvmctx,
          pointerval,
          ty,
          alignment,
        }
      }
      _ => unreachable!(),
    }
  }
}

pub enum SrcType {
  Pointer {
    builder: LLVMBuilderRef,
    llvmctx: LLVMContextRef,
    pointerval: LLVMValueRef,
    ty: LLVMTypeRef,
    alignment: Option<u8>,
  },
  RegMap {
    builder: LLVMBuilderRef,
    llvmctx: LLVMContextRef,
    vectmask: Vec<RegMask>,
    typemap: LLVMTypeMapping,
    regsvalue: Box<[LLVMValueRef]>,
  },
}

#[inline(always)]
pub fn llvmload_src(src: SrcType) -> LLVMValueRef {
  match src {
    SrcType::RegMap {
      builder,
      llvmctx,
      typemap,
      vectmask,
      regsvalue,
    } => load_all_vectored(builder, llvmctx, typemap, &vectmask, &regsvalue),
    SrcType::Pointer {
      builder,
      llvmctx,
      ty,
      pointerval,
      alignment,
    } => offsetload_aligned(
      builder,
      llvmctx,
      ty,
      pointerval,
      OffsetBytes::I(0),
      alignment.map(|x| x as u32),
    ),
  }
}

#[inline(always)]
pub fn llvmresolve_location_src_load(
  meta: &mut CompilerMeta,
  typ: LLVMTypeOrWidth,

  // Location-src
  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,
) -> LLVMValueRef {
  llvmload_src(llvmresolve_location_src_ptr(
    meta, typ, locsrc, alignment, offset, count,
  ))
}

#[inline(always)]
pub fn llvmresolve_location_src_store(
  meta: &mut CompilerMeta,
  typ: LLVMTypeOrWidth,

  // Location-src
  locsrc: u8,
  alignment: Option<u8>,
  offset: i32,
  count: u32,
) -> StoreResolver {
  unsafe {
    let typemap = typ.r#type();

    let meta_ptr = meta as *mut CompilerMeta;
    let offsetbytes = offset as i64 * typemap.width as i64;

    match locsrc {
      0..=7 => {
        let out = regmapper(locsrc, offsetbytes as _, typemap, count);

        let regsvalue = out
          .regstouched
          .clone()
          .map(|x| (*meta_ptr).regmnt.usereg(x as usize))
          .collect::<Box<[_]>>();

        StoreResolver::RegMapOut(typemap, regsvalue, out)
      }
      // Scratchpad
      8 => StoreResolver::Ptr(meta.scratchpad, offsetbytes, alignment),
      // Largepad
      9 => {
        let val = (*meta_ptr).regmnt.usereg(LARGEPAD);

        StoreResolver::Ptr(val, offsetbytes, alignment)
      }
      // Read pointer through r2
      10 => {
        let val = (*meta_ptr).regmnt.usereg(REG_R2);

        StoreResolver::Ptr(val, offsetbytes, alignment)
      }
      // Read pointer through r3
      11 => {
        let val = (*meta_ptr).regmnt.usereg(REG_R3);

        StoreResolver::Ptr(val, offsetbytes, alignment)
      }
      _ => unreachable!(),
    }
  }
}

pub enum StoreResolver {
  RegMapOut(LLVMTypeMapping, Box<[LLVMValueRef]>, RegMapOut),
  Ptr(LLVMValueRef, i64, Option<u8>),
}

impl StoreResolver {
  pub fn synchronize(self, meta: &mut CompilerMeta, vect: LLVMValueRef) {
    unsafe {
      match self {
        Self::RegMapOut(llvmty, mut regvals, regmap) => {
          // Direct reg width
          if llvmty.width == 8 {
            // Dont force single length into a <1 x N> vector
            if regmap.vectmask.len() == 1 {
              regvals[0] = vect;
            } else {
              regmap.vectmask.iter().for_each(|x| {
                let index = LLVMConstInt(meta.i32, x.reg_fromroot as u64, 0);

                regvals[x.reg_fromroot as usize] =
                  LLVMBuildExtractElement(meta.builder, vect, index, LLVM_VAR_NAME.0);
              });
            }

            let meta_ptr = meta as *mut CompilerMeta;
            for (regval, regid) in regvals.into_iter().zip(regmap.regstouched) {
              let regval = if llvmty.float {
                LLVMBuildBitCast(meta.builder, regval, meta.i64, LLVM_VAR_NAME.0)
              } else {
                regval
              };

              (*meta_ptr).regmnt.setreg(regid as _, regval);
            }
            return;
          }

          regvals.iter_mut().for_each(|x| {
            *x = LLVMBuildBitCast(meta.builder, *x, llvmty.xreg, LLVM_VAR_NAME.0);
          });

          regmap.vectmask.iter().enumerate().for_each(|(idx, x)| {
            let extract_index = LLVMConstInt(meta.i32, idx as u64, 0);
            let insert_index = LLVMConstInt(meta.i32, x.laneid as u64, 0);

            let regval = regvals[x.reg_fromroot as usize];

            let extracted =
              LLVMBuildExtractElement(meta.builder, vect, extract_index, LLVM_VAR_NAME.0);

            regvals[x.reg_fromroot as usize] = LLVMBuildInsertElement(
              meta.builder,
              regval,
              extracted,
              insert_index,
              LLVM_VAR_NAME.0,
            );
          });

          let meta_ptr = meta as *mut CompilerMeta;
          for (regval, regid) in regvals.into_iter().zip(regmap.regstouched) {
            let regval = LLVMBuildBitCast(meta.builder, regval, meta.i64, LLVM_VAR_NAME.0);
            (*meta_ptr).regmnt.setreg(regid as _, regval);
          }
        }
        Self::Ptr(ptr, offset, align) => {
          offsetstore_aligned(
            meta.builder,
            meta.llvmctx,
            vect,
            ptr,
            OffsetBytes::I(offset),
            align.map(|x| x as u32),
          );
        }
      }
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub enum LLVMTypeOrWidth {
  Type(u8),
  Width(u8),
}

impl LLVMTypeOrWidth {
  pub fn vect(&self, count: u32) -> LLVMTypeRef {
    let typeref = self.r#type().x1;

    if count == 1 {
      return typeref;
    }

    unsafe { LLVMVectorType(typeref, count as _) }
  }

  #[allow(unused)]
  pub fn regsized(&self, count: u32) -> bool {
    let llvmtype = self.r#type();

    if llvmtype.width == 8 {
      return count == 1;
    }

    let typeref = llvmtype.xreg;

    unsafe { LLVMGetVectorSize(typeref) < count }
  }

  pub fn r#type(&self) -> LLVMTypeMapping {
    unsafe {
      let ctx = LLVM_CTX.with(|x| x.0);

      let (width_bytes, signed, float, is_f32) = match self {
        Self::Type(x) => match *x {
          0 | 4 => (8, *x == 4, false, false), // u64 / i64
          1 | 5 => (4, *x == 5, false, false), // u32 / i32
          2 | 6 => (2, *x == 6, false, false), // u16 / i16
          3 | 7 => (1, *x == 7, false, false), // u8  / i8
          8 => (8, false, true, false),        // f64
          9 => (4, false, true, true),         // f32
          _ => unreachable!(),
        },
        Self::Width(w) => match *w {
          0 => (8, false, false, false),
          1 => (4, false, false, false),
          2 => (2, false, false, false),
          3 => (1, false, false, false),
          _ => unreachable!(),
        },
      };

      let element_type = if float {
        if is_f32 {
          LLVMFloatTypeInContext(ctx)
        } else {
          LLVMDoubleTypeInContext(ctx)
        }
      } else {
        match width_bytes {
          8 => LLVMInt64TypeInContext(ctx),
          4 => LLVMInt32TypeInContext(ctx),
          2 => LLVMInt16TypeInContext(ctx),
          1 => LLVMInt8TypeInContext(ctx),
          _ => unreachable!(),
        }
      };

      let xreg = if width_bytes == 8 {
        element_type
      } else {
        let array_len = (8 / width_bytes) as u32;
        LLVMVectorType(element_type, array_len as _)
      };

      LLVMTypeMapping {
        width: width_bytes,
        x1: element_type,
        xreg,
        signed,
        float,
      }
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMTypeMapping {
  pub width: u8,
  pub x1: LLVMTypeRef,
  pub xreg: LLVMTypeRef,
  pub signed: bool,
  pub float: bool,
}
