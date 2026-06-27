use std::io::{Cursor, Seek};

use mixedradix::MixedRadixStructure;
use zip::unstable::LittleEndianReadExt;

use crate::{
  ReadError,
  parse::{Flags, GlobalMeta, ReadU8, SaBINFile, SizePtr},
};

pub fn parse<'a>(data: &'a [u8], mut header: Cursor<&'a [u8]>) -> Result<SaBINFile<'a>, ReadError> {
  let flags = {
    let flags = header.read_u8()?;
    Flags::try_from_bits(flags).ok_or(ReadError::InvalidSaBIN)?
  };

  let is_compact = flags.compactdata != 0;

  let libcall_def_size: usize = if is_compact { 2 + 4 } else { 2 + 8 };

  let total_meta_keys = header.read_u16_le()?;

  let metadata = (0..total_meta_keys)
    .map(|_| {
      let size = header.read_u8()?;
      let pos = header.position() as usize;

      header.seek_relative(size as _)?;

      data
        .get(pos..(pos + size as usize))
        .ok_or(ReadError::UnexpectedEOF)
    })
    .collect::<Result<Box<[_]>, ReadError>>()?;

  let globalmeta = {
    let total_libs = if is_compact {
      header.read_u32_le()? as u64
    } else {
      header.read_u64_le()?
    };

    let total_calldecls = if is_compact {
      header.read_u32_le()? as u64
    } else {
      header.read_u64_le()?
    };

    let last_section_idx = if is_compact {
      header.read_u32_le()? as u64
    } else {
      header.read_u64_le()?
    };

    let total_triples = header.read_u8()?;

    GlobalMeta {
      total_libs,
      last_section_idx,
      total_calldecls,
      total_triples,
    }
  };

  let rodata = {
    let (size, cursor) = header.read_size_u40ptr()?;

    data
      .get(cursor..cursor + size as usize)
      .ok_or(ReadError::UnexpectedEOF)?
  };

  let rwdata = {
    let SizePtr { size, ptr } = SizePtr::from_bits(header.read_u64_le()?);
    let cursor = ptr as usize;

    // Architectural reasons - RWDATA is mutable.
    Box::from(
      data
        .get(cursor..cursor + size as usize)
        .ok_or(ReadError::UnexpectedEOF)?,
    )
  };

  let pgo_critical = {
    let size = header.read_u8()?;

    let range = 0..size;

    if is_compact {
      range
        .map(|_| header.read_u32_le().map(|x| x as u64))
        .collect::<Result<Box<[u64]>, _>>()?
    } else {
      range
        .map(|_| header.read_u64_le())
        .collect::<Result<Box<[u64]>, _>>()?
    }
  };

  let pgo_priority = {
    let size = header.read_u8()?;

    let range = 0..size;

    if is_compact {
      range
        .map(|_| header.read_u32_le().map(|x| x as u64))
        .collect::<Result<Box<[u64]>, _>>()?
    } else {
      range
        .map(|_| header.read_u64_le())
        .collect::<Result<Box<[u64]>, _>>()?
    }
  };

  let bytecodemap_begin = {
    let idx = header.position();

    let to_seek = globalmeta
      .last_section_idx
      .checked_add(1)
      .and_then(|x| x.checked_mul(8))
      .ok_or(ReadError::SizeOverflow)?;

    header.seek_relative(i64::try_from(to_seek).map_err(|_| ReadError::SizeOverflow)?)?;

    idx as usize
  };

  let calldecltable_begin = {
    let idx = header.position();

    let to_seek = globalmeta
      .total_calldecls
      .checked_mul(8)
      .ok_or(ReadError::SizeOverflow)?;

    header.seek_relative(i64::try_from(to_seek).map_err(|_| ReadError::SizeOverflow)?)?;

    idx as usize
  };

  let triples = {
    let total = globalmeta.total_triples;
    let cursor = header.position() as usize;

    let (keys, _) = data
      .get(cursor..(cursor + 8 * total as usize))
      .ok_or(ReadError::InvalidSaBIN)?
      .as_chunks::<8>();

    if !keys.is_sorted() {
      return Err(ReadError::InvalidSaBIN);
    }

    header.seek_relative(8 * total as i64)?;

    let cursor = header.position() as usize;
    let (size_n_ptr, _) = data
      .get(cursor..(cursor + 9 * total as usize))
      .ok_or(ReadError::InvalidSaBIN)?
      .as_chunks::<9>();

    header.seek_relative(9 * total as i64)?;

    (keys, size_n_ptr)
  };

  let file = SaBINFile {
    flags,
    is_compact,
    libcall_def_size,
    metadata,
    globalmeta,
    rodata,
    rwdata,
    pgo_critical,
    pgo_priority,
    bytecodemap_begin,
    calldecltable_begin,
    triples,
  };
  Ok(file)
}
