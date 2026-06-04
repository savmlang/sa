use crate::acaot::native::llvm_compiler::{
  CompilerMeta, LLVM_CTX, LLVM_VAR_NAME,
  irgen::{
    OffsetBytes, offsetload, offsetstore,
    reg::regmap::{RegMapOut, load_all_vectored, regmapper},
  },
  ssaupdater::{LARGEPAD, REG_R2},
};
use llvm_sys::{
  core::{
    LLVMBuildBitCast, LLVMBuildExtractElement, LLVMBuildInsertElement, LLVMConstInt,
    LLVMDoubleTypeInContext, LLVMFloatTypeInContext, LLVMGetVectorSize, LLVMInt8TypeInContext,
    LLVMInt16TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMVectorType,
  },
  prelude::{LLVMTypeRef, LLVMValueRef},
};

pub mod regmap;

pub const REGISTER_WIDTH: u8 = 8;

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
  unsafe {
    let typemap = typ.r#type();

    let meta_ptr = meta as *mut CompilerMeta;
    let offsetbytes = offset as i64 * typemap.width as i64;

    match locsrc {
      0..=7 => {
        let out = regmapper(locsrc, offsetbytes as _, typemap, count);

        let regsvalue = out
          .regstouched
          .map(|x| (*meta_ptr).regmnt.usereg(x as usize, meta_ptr))
          .collect::<Box<[_]>>();

        load_all_vectored(
          meta.builder,
          meta.llvmctx,
          typemap,
          &out.vectmask,
          &regsvalue,
        )
      }
      // Scratchpad
      8 => {
        let ty = typ.vect(count as _);

        offsetload(
          meta.builder,
          meta.llvmctx,
          ty,
          meta.scratchpad,
          OffsetBytes::I(offsetbytes),
        )
      }
      // Largepad
      9 => {
        let ty = typ.vect(count as _);
        let val = (*meta_ptr).regmnt.usereg(LARGEPAD, meta_ptr);

        offsetload(
          meta.builder,
          meta.llvmctx,
          ty,
          val,
          OffsetBytes::I(offsetbytes),
        )
      }
      // Read pointer through r2
      10 => {
        let ty = typ.vect(count as _);
        let val = (*meta_ptr).regmnt.usereg(REG_R2, meta_ptr);

        offsetload(
          meta.builder,
          meta.llvmctx,
          ty,
          val,
          OffsetBytes::I(offsetbytes),
        )
      }
      _ => unreachable!(),
    }
  }
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
          .map(|x| (*meta_ptr).regmnt.usereg(x as usize, meta_ptr))
          .collect::<Box<[_]>>();

        StoreResolver::RegMapOut(typemap, regsvalue, out)
      }
      // Scratchpad
      8 => StoreResolver::Ptr(meta.scratchpad, offsetbytes),
      // Largepad
      9 => {
        let val = (*meta_ptr).regmnt.usereg(LARGEPAD, meta_ptr);

        StoreResolver::Ptr(val, offsetbytes)
      }
      // Read pointer through r2
      10 => {
        let val = (*meta_ptr).regmnt.usereg(REG_R2, meta_ptr);

        StoreResolver::Ptr(val, offsetbytes)
      }
      _ => unreachable!(),
    }
  }
}

pub enum StoreResolver {
  RegMapOut(LLVMTypeMapping, Box<[LLVMValueRef]>, RegMapOut),
  Ptr(LLVMValueRef, i64),
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

              (*meta_ptr).regmnt.setreg(regid as _, regval, meta_ptr);
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
            (*meta_ptr).regmnt.setreg(regid as _, regval, meta_ptr);
          }
        }
        Self::Ptr(ptr, offset) => {
          offsetstore(
            meta.builder,
            meta.llvmctx,
            vect,
            ptr,
            OffsetBytes::I(offset),
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
        x1i: element_type,
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
  pub x1i: LLVMTypeRef,
  pub xreg: LLVMTypeRef,
  pub signed: bool,
  pub float: bool,
}
