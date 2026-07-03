use std::rc::Rc;

use crate::{
  StringStore,
  mir::{
    Module,
    value::{ValueType, ValueTypeArray, ValueTypeRef},
  },
};

#[repr(align(8))]
pub struct Signature {
  pub args: Option<ValueTypeRef>,
  pub returns: Option<ValueTypeRef>,
}

impl Signature {
  pub fn new<T: StringStore>(
    module: &mut Module<T>,
    argv: &[ValueTypeRef],
    r#return: Option<ValueTypeRef>,
  ) -> Result<Self, SigError> {
    let mut args = None;
    let mut returns = None;

    // It has to be aligned by `16` atmost
    if !argv.is_empty() {
      let unified = ValueType::Composite {
        composition: ValueTypeArray::Rc(Rc::from(argv)),
        align: None,
      };

      if unified.align(module) > 16 {
        return Err(SigError::ArgvOveraligned);
      }

      if unified.size(module) > 64 {
        return Err(SigError::ArgvOversized);
      }

      args = Some(module.insert_type(unified));
    }

    if let Some(ret) = r#return {
      let rett = module
        .type_data(r#ret)
        .ok_or(SigError::ReturnTypeNotFound)?;

      if rett.size(module) > 16 {
        return Err(SigError::ReturnSizeOverflow);
      }

      if rett.align(module) > 32 {
        return Err(SigError::ReturnAlignOverflow);
      }

      returns = Some(ret);
    }

    Ok(Self { args, returns })
  }
}

#[derive(Debug, Clone, Copy)]
pub enum SigError {
  ArgvOversized,
  ArgvOveraligned,

  ReturnAlignOverflow,
  ReturnSizeOverflow,

  ReturnTypeNotFound,
}

// A signature should never be null
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SignatureRef(pub(crate) usize);

pub(crate) mod internal {
  use crate::{
    StringStore,
    mir::{
      Module,
      value::{
        ValueType, ValueTypeRef,
        consts::{D64, F32, I8, I16, I32, I64, U8, U16, U32, U64},
        sig::Signature,
      },
    },
  };
  use std::fmt::Formatter;

  impl Signature {
    pub(crate) fn print<T: StringStore>(
      &self,
      idx: usize,
      module: &Module<T>,
      f: &mut Formatter,
    ) -> std::fmt::Result {
      write!(f, "  sig #{}(", idx + 1)?;

      if let Some(args) = self.args {
        let ty = module
          .type_data(args)
          .expect("Invariant violation : Module has a dangling pointer to a variable");

        match ty {
          ValueType::Composite { composition, .. } => {
            let mut first = true;
            for comp in composition.as_ref() {
              if first {
                first = false;
              } else {
                write!(f, ",")?;
              }

              let mt = match comp {
                &I64 => "i64",
                &U64 => "u64",

                &I32 => "i32",
                &U32 => "u32",

                &I16 => "i16",
                &U16 => "u16",

                &I8 => "i8",
                &U8 => "u8",

                &F32 => "f32",
                &D64 => "f64",

                &ValueTypeRef(rf) => {
                  let rf = rf.get();

                  write!(f, " #{rf}")?;

                  ""
                }
              };

              if !mt.is_empty() {
                write!(f, " {mt}")?
              }
            }
          }
          // Guaranteed to be composite actually
          _ => {}
        }
      }

      write!(f, " ) -> (")?;

      if let Some(ret) = self.returns {
        let mt = match &ret {
          &I64 => "i64",
          &U64 => "u64",

          &I32 => "i32",
          &U32 => "u32",

          &I16 => "i16",
          &U16 => "u16",

          &I8 => "i8",
          &U8 => "u8",

          &F32 => "f32",
          &D64 => "f64",

          &ValueTypeRef(rf) => {
            let rf = rf.get();

            write!(f, "#{rf}")?;

            ""
          }
        };

        if !mt.is_empty() {
          write!(f, "{mt}")?
        }
      }

      writeln!(f, ")")?;

      Ok(())
    }
  }
}
