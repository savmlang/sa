use std::{
  error::Error,
  fmt::{Debug, Display},
};

use crate::parse::SaBINFile;

pub mod file;
pub mod parse;

pub struct SaBINReader<'a> {
  pub sabin: SaBINFile<'a>,
}

pub const SAVM_PROLOGUE: &'static [u8] = b"SaVMBIN-AToB";

impl<'a> SaBINReader<'a> {
  pub fn create(file: &'a [u8]) -> Result<Self, ReadError> {
    let magic = file.get(0..12).ok_or(ReadError::UnexpectedEOF)?;

    if !magic.eq(SAVM_PROLOGUE) {
      return Err(ReadError::InvalidSaBIN);
    }

    let sabin = parse::parse_sabin(file)?;

    Ok(Self { sabin })
  }
}

#[derive(Debug)]
pub enum ReadError {
  UnexpectedEOF,
  InvalidSaBIN,
  SizeOverflow,
  UnsortedSaTriple,
  UnsupportedVersion(u16),
  IOErr(std::io::Error),
}

macro_rules! errs {
  (
    $(
      $typ:ty => $field:ident
    ),*
  ) => {
    $(
      impl From<$typ> for ReadError {
        fn from(value: $typ) -> Self {
          Self::$field(value)
        }
      }
    )*
  };
}

errs! {
  std::io::Error => IOErr
}

impl Display for ReadError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    Debug::fmt(&self, f)
  }
}

impl Error for ReadError {}
