use crate::acaot::native::llvm_compiler::{
  CompilerMeta, LLVM_CTX,
  irgen::{OffsetBytes, offsetload},
  ssaupdater::{LARGEPAD, REG_R2},
};
use llvm_sys::{
  core::{
    LLVMArrayType2, LLVMConstArray2, LLVMDoubleTypeInContext, LLVMFloatTypeInContext,
    LLVMGetArrayLength2, LLVMInt8TypeInContext, LLVMInt16TypeInContext, LLVMInt32TypeInContext,
    LLVMInt64TypeInContext,
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
    let meta_ptr = meta as *mut _;
    let offsetbytes = offset as i64 * typ.r#type().width as i64;

    match locsrc {
      0..=7 => {
        // let out = regmapper(locsrc, ofset, typedata, count, assumedwdt);

        println!("Found register mapping");
        todo!("Registers will be handled soon!");
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
        let val = meta.regmnt.usereg(LARGEPAD, meta_ptr);

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
        let val = meta.regmnt.usereg(REG_R2, meta_ptr);

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
) {
}

#[derive(Debug, Clone, Copy)]
pub enum LLVMTypeOrWidth {
  Type(u8),
  Width(u8),
}

impl LLVMTypeOrWidth {
  pub fn vect(&self, count: u64) -> LLVMTypeRef {
    let typeref = self.r#type().x1;

    if count == 1 {
      return typeref;
    }

    unsafe { LLVMArrayType2(typeref, count) }
  }

  pub fn regsized(&self, count: u64) -> bool {
    let llvmtype = self.r#type();

    if llvmtype.width == 8 {
      return count == 1;
    }

    let typeref = llvmtype.xreg;

    unsafe { LLVMGetArrayLength2(typeref) < count }
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
        LLVMArrayType2(element_type, array_len as _)
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

pub struct LLVMTypeMapping {
  pub width: u8,
  pub x1: LLVMTypeRef,
  pub x1i: LLVMTypeRef,
  pub xreg: LLVMTypeRef,
  pub signed: bool,
  pub float: bool,
}
