use crate::{ReadError, SAVM_PROLOGUE};
use std::io::{Cursor, Read, Seek};
use zip::unstable::LittleEndianReadExt;

pub mod v0;

pub fn parse_sabin<'a>(data: &'a [u8]) -> Result<SaBINFile<'a>, ReadError> {
  let mut header = Cursor::new(data);

  header.seek_relative(SAVM_PROLOGUE.len() as _)?;

  let version = header.read_u16_le()?;

  match version {
    0 => v0::parse(data, header),
    e => Err(ReadError::UnsupportedVersion(e)),
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SaBINFile<'a> {
  pub flags: Flags,
  pub is_compact: bool,
  pub libcall_def_size: usize,

  pub metadata: Box<[&'a [u8]]>,
  pub globalmeta: GlobalMeta,

  pub rodata: &'a [u8],

  /// This is owned because RWData is mutable
  pub rwdata: Box<[u8]>,

  pub pgo_critical: Box<[u64]>,
  pub pgo_priority: Box<[u64]>,

  pub bytecodemap_begin: usize,
  pub calldecltable_begin: usize,

  pub triples: (&'a [[u8; 8]], &'a [[u8; 9]]),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlobalMeta {
  pub total_libs: u64,
  pub total_calldecls: u64,
  pub last_section_idx: u64,
  pub total_triples: u8,
}

mixedradix::mixedradix! {
  #[bits(8)]
  #[derive(Debug, Clone, Copy, PartialEq)]
  #[repr(C)]
  pub struct Flags {
    pub compactdata: 2,
    pub sabinformat: 3
  }

  #[bits(64)]
  #[derive(Debug, Clone, Copy)]
  #[repr(C)]
  pub struct SizePtr {
    pub size: 16_777_216, // 2^24
    pub ptr: 1_099_511_627_776 // 2^40
  }
}

pub trait ReadU8: Read {
  fn read_u8(&mut self) -> std::io::Result<u8> {
    let mut d = [0];
    self.read_exact(&mut d)?;
    let [d] = d;

    Ok(d)
  }

  fn read_size_u40ptr(&mut self) -> std::io::Result<(u32, usize)> {
    let mut size = [0u8; 4];
    self.read_exact(&mut size)?;
    let size = u32::from_le_bytes(size);

    let mut cursor_bytes = [0u8; 8];
    self.read_exact(&mut cursor_bytes[..5])?;

    let cursor = u64::from_le_bytes(cursor_bytes) as usize;
    let cursor = cursor as usize;

    Ok((size, cursor))
  }
}

impl<T: Read> ReadU8 for T {}
