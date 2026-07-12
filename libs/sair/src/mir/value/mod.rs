use std::{
  marker::PhantomData,
  num::NonZeroUsize,
  ops::{Deref, Sub},
  rc::Rc,
};

pub mod calc;
pub mod sig;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(NonZeroUsize);

pub struct ValueMeta {}

#[repr(align(1))]
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ValueType<'a> {
  Base {
    base: BaseType,

    #[allow(private_interfaces)]
    _uninstantiable: PhantomData<CoreType>,
  },
  Vector {
    base: BaseType,
    count: u8,
  },
  /// The size and alignment of the largest type is respected!
  PrimaryUnion {
    composition: [BaseType; 36],
    count: u8,
    align: Option<Alignment>,
  },
  Union {
    composition: ValueTypeArray<'a>,
    align: Option<Alignment>,
  },
  /// Padding is automatically inserted!
  PrimaryComposite {
    composition: [BaseType; 36],
    count: u8,
    align: Option<Alignment>,
  },
  /// Padding is automatically inserted!
  Composite {
    composition: ValueTypeArray<'a>,
    align: Option<Alignment>,
  },
}

impl<'a> ValueType<'a> {
  pub fn is_num(&self) -> bool {
    matches!(self, Self::Base { .. } | Self::Vector { .. })
  }

  pub fn is_vector(&self) -> bool {
    matches!(self, Self::Vector { .. })
  }

  pub fn is_scalar(&self) -> bool {
    !self.is_vector()
  }

  pub fn is_int(&self) -> bool {
    self.is_num() && !self.is_float()
  }

  pub fn is_float(&self) -> bool {
    match self {
      Self::Base { base, .. } | Self::Vector { base, .. } => {
        matches!(base, BaseType::Float32 | BaseType::Double64)
      }
      _ => false,
    }
  }
}

#[repr(align(1))]
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ValueTypeArray<'a> {
  Rc(Rc<[ValueTypeRef]>),
  Slice(&'a [ValueTypeRef]),
}

impl<'a> AsRef<[ValueTypeRef]> for ValueTypeArray<'a> {
  fn as_ref(&self) -> &[ValueTypeRef] {
    match self {
      Self::Rc(x) => x.as_ref(),
      Self::Slice(x) => x,
    }
  }
}

impl<'a> Deref for ValueTypeArray<'a> {
  type Target = [ValueTypeRef];

  fn deref(&self) -> &Self::Target {
    self.as_ref()
  }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Alignment {
  B1,
  B2,
  B4,
  B8,
  B16,
  B32,
  B64,
  B128,
  B256,
  B512,
  B1024,
  B2048,
  B4096,
  B8192,
  B16384,
  B32768,
  B65536,
}

impl Alignment {
  pub fn align(self) -> usize {
    match self {
      Self::B1 => 1,
      Self::B2 => 2,

      Self::B4 => 4,
      Self::B8 => 8,

      Self::B16 => 16,
      Self::B32 => 32,

      Self::B64 => 64,
      Self::B128 => 128,

      Self::B256 => 256,
      Self::B512 => 512,

      Self::B1024 => 1024,
      Self::B2048 => 2048,

      Self::B4096 => 4096,
      Self::B8192 => 8192,

      Self::B16384 => 16384,
      Self::B32768 => 32768,

      Self::B65536 => 65536,
    }
  }

  pub const fn parse(data: usize) -> Alignment {
    match data {
      1 => Self::B1,
      2 => Self::B2,

      4 => Self::B4,
      8 => Self::B8,

      16 => Self::B16,
      32 => Self::B32,

      64 => Self::B64,
      128 => Self::B128,

      256 => Self::B256,
      512 => Self::B512,

      1024 => Self::B1024,
      2048 => Self::B2048,

      4096 => Self::B4096,
      8192 => Self::B8192,

      16384 => Self::B16384,
      32768 => Self::B32768,

      65536 => Self::B65536,

      _ => panic!("Could not correctly construct alignment"),
    }
  }
}

pub(crate) struct CoreType;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ValueTypeRef(pub(crate) NonZeroUsize);

impl ValueTypeRef {
  pub fn index(self) -> usize {
    self.0.get().sub(1)
  }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BaseType {
  Int64,
  Int32,
  Int16,
  Int8,

  UInt64,
  UInt32,
  UInt16,
  UInt8,

  Double64,
  Float32,
}

pub mod consts {
  use crate::mir::value::ValueTypeRef;
  use std::num::NonZeroUsize;

  /// Literally an Alias to [I64]
  pub const PTR: ValueTypeRef = I64;

  pub const I64: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(1) });
  pub const I32: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(2) });
  pub const I16: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(3) });
  pub const I8: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(4) });

  pub const U64: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(5) });
  pub const U32: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(6) });
  pub const U16: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(7) });
  pub const U8: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(8) });

  pub const D64: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(9) });
  pub const F32: ValueTypeRef = ValueTypeRef(unsafe { NonZeroUsize::new_unchecked(10) });
}

pub(crate) mod internal {
  use crate::{
    StringStore,
    mir::{
      Module,
      value::{BaseType, ValueType},
    },
  };
  use std::fmt::Formatter;

  impl BaseType {
    pub(crate) fn format(self, f: &mut Formatter) -> std::fmt::Result {
      write!(
        f,
        "@{}",
        match self {
          Self::Int64 => "i64",
          Self::Int32 => "i32",
          Self::Int16 => "i16",
          Self::Int8 => "i8",

          Self::UInt64 => "u64",
          Self::UInt32 => "u32",
          Self::UInt16 => "u16",
          Self::UInt8 => "u8",

          Self::Float32 => "f32",
          Self::Double64 => "d64",
        }
      )
    }
  }

  impl<'a> ValueType<'a> {
    pub(crate) fn fmt<T: StringStore>(
      &self,
      id: usize,
      store: &Module<T>,
      f: &mut Formatter,
    ) -> std::fmt::Result {
      write!(
        f,
        "  type #{}({}, {}) = ",
        id,
        self.size(store),
        self.align(store)
      )?;

      match self {
        Self::Base { base, .. } => {
          base.format(f)?;
          writeln!(f, "")?;
        }
        &Self::PrimaryUnion {
          ref composition,
          count,
          ..
        } => {
          writeln!(f, "union {{")?;

          for item in &composition[0..(count as usize)] {
            write!(f, "    ")?;
            item.format(f)?;
            writeln!(f, "")?;
          }

          writeln!(f, "  }}")?;
        }
        &Self::PrimaryComposite {
          ref composition,
          count,
          ..
        } => {
          writeln!(f, "struct {{")?;

          for item in &composition.as_ref()[0..(count as usize)] {
            write!(f, "    ")?;
            item.format(f)?;
            writeln!(f, "")?;
          }

          writeln!(f, "  }}")?;
        }
        Self::Vector { base, count } => {
          write!(f, "vector <")?;

          base.format(f)?;

          writeln!(f, " x {count}>")?;
        }
        Self::Union { composition, .. } => {
          writeln!(f, "union {{")?;

          for &vtr in composition.as_ref() {
            write!(f, "    ")?;

            if let Some(x) = store.type_data(vtr) {
              match x {
                ValueType::Base { base, .. } => base.format(f)?,
                _ => write!(f, "@type:{}", vtr.0)?,
              }
              writeln!(f, "")?;
            } else {
              writeln!(f, "<error>")?;
            }
          }

          writeln!(f, "  }}")?;
        }
        Self::Composite { composition, .. } => {
          writeln!(f, "struct {{")?;

          for &vtr in composition.as_ref() {
            write!(f, "    ")?;

            if let Some(x) = store.type_data(vtr) {
              match x {
                ValueType::Base { base, .. } => base.format(f)?,
                _ => write!(f, "@type:{}", vtr.0)?,
              }

              writeln!(f, "")?;
            } else {
              writeln!(f, "<error>")?;
            }
          }

          writeln!(f, "  }}")?;
        }
      }

      Ok(())
    }
  }
}
