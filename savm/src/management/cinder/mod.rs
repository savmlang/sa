use std::sync::Arc;

use sajit::{
  Executable, MemoryExecutableApi, MemorySizeInfo, SizeCheck, WriteFnResult, relcar::RELCAR_BASIC,
  relocations::Relocation,
};
use savmbuild_cinder::RelocKind;

use crate::{
  acaot::{
    Stencils,
    cinder::{Resolved, unpack_marker},
  },
  management::jitmem::JITMemoryManager,
};

pub fn link(
  entries: Arc<[Box<[u8]>]>,
  stencil: Stencils,
  sajit: &mut JITMemoryManager,
) -> (*const Executable, *mut usize) {
  let entries_size = entries
    .iter()
    .map(|x| x.len())
    .sum::<usize>()
    .next_multiple_of(16);

  let stencils_size = stencil
    .iter()
    .map(|x| {
      x.iter()
        .map(|x| x.stencil.mcemit.len().next_multiple_of(16))
        .sum::<usize>()
        .next_multiple_of(16)
    })
    .sum::<usize>();
  let size_req = entries_size + stencils_size;

  let offsets = {
    let mut offset = 0;
    entries
      .iter()
      .map(|x| {
        let v = offset;

        offset += x.len();

        v
      })
      .collect::<Box<_>>()
  };

  sajit.get_sajit_hwnd(size_req, move |sajit| {
    let entries_data = entries.iter().map(|x| x.as_ref());

    let entries_start = sajit.base_address() + sajit.cursor().next_multiple_of(16);
    let start_offset = entries_start + entries_size;
    let offsets_slice = offsets.as_ref();
    let offsets_stencil = stencil
      .iter()
      .flat_map(|stenvec| {
        stenvec
          .iter()
          .map(Some)
          .chain(std::iter::repeat(None))
          .take(6)
      })
      .scan(start_offset, |tmp_offset, maybe_stencil| {
        if let Some(stencil) = maybe_stencil {
          let out = *tmp_offset;
          *tmp_offset += stencil.stencil.mcemit.len();
          Some(out)
        } else {
          Some(0)
        }
      })
      .collect::<Box<[_]>>();

    let relocs = {
      let markers = &*entries[0];

      unsafe { markers.as_chunks_unchecked::<24>() }
        .iter()
        .enumerate()
        .map(|(id, x)| {
          let (_, internal, _) = unpack_marker(*x);

          let ptr_real = unsafe { *offsets_stencil.get_unchecked(internal as usize * 6) };
          let offset = id * 24 + 16;

          Relocation {
            offset: offset as u32,
            addend: 0,
            symbol_addr: ptr_real as _,
            kind: reloc_abs(),
          }
        })
    };

    let entries_ptr =
      match unsafe { sajit.write_fn_iterated(entries_size, entries_data, relocs, &RELCAR_BASIC) } {
        WriteFnResult::Executable(e) => e,
        _ => unreachable!(),
      };

    debug_assert!(
      entries_ptr.addr() == entries_start,
      "Real entries address is : {} while calculated was : {}",
      entries_ptr.addr(),
      entries_start
    );

    let acc_stencils = stencil
      .iter()
      .flat_map(|x| x.iter().map(|x| x.stencil.mcemit));

    let offsets_stencil_ref = &offsets_stencil;

    let relocs_iter = stencil
      .iter()
      .flat_map(|x| {
        let main_size = x.iter().map(|s| s.stencil.mcemit.len()).sum::<usize>();

        // Scan across inner stencils to track running (curroffset, accumulated)
        x.iter().scan(0usize, move |accumulated, stencil_item| {
          let current_acc = *accumulated;
          let current_size = stencil_item.stencil.mcemit.len();
          *accumulated += current_size;

          Some((current_acc, main_size, stencil_item))
        })
      })
      .scan(
        0usize,
        move |curroffset, (accumulated, main_size, stencil_item)| {
          let current_curroffset = *curroffset;
          let stencil_size = stencil_item.stencil.mcemit.len();
          *curroffset += stencil_size;

          let reloc = stencil_item.stencil.reloc;

          // Eagerly collect or map the relocations for this stencil
          let relocs = stencil_item.resolve.iter().map(move |&(name, ref res)| {
            let reloc_entry = unsafe {
              let idx = reloc.binary_search_by(|s| s.symbol.cmp(name)).unwrap();
              reloc.get_unchecked(idx)
            };

            let addr = match *res {
              Resolved::Immediate { imm } => imm,
              Resolved::MarkersArray { idx } => unsafe {
                (entries_ptr.addr() + *offsets_slice.get_unchecked(idx)) as u64
              },
              Resolved::NextStencil => (start_offset + current_curroffset + stencil_size) as u64,
              Resolved::NextMainID => {
                (start_offset + current_curroffset + main_size - accumulated) as u64
              }
              Resolved::WorkingSetId { idx } => unsafe {
                (entries_ptr.addr() + *offsets_slice.get_unchecked(idx)) as u64
              },
              Resolved::StencilId { mainid, subid } => unsafe {
                *offsets_stencil_ref.get_unchecked(mainid * 6 + subid) as u64
              },
              // Will be pre-resolved
              Resolved::ResolveLaterStencilID { .. } => unreachable!(),
            };

            Relocation {
              offset: (current_curroffset + reloc_entry.offset as usize) as u32,
              symbol_addr: addr,
              addend: 0,
              kind: reloc_abs(),
            }
          });

          Some(relocs)
        },
      )
      .flatten();

    let relocd_jit = match unsafe {
      sajit.write_fn_iterated(stencils_size, acc_stencils, relocs_iter, &RELCAR_BASIC)
    } {
      WriteFnResult::Executable(e) => e,
      _ => unreachable!(),
    };

    debug_assert!(
      relocd_jit.addr() == start_offset,
      "Real address is : {} while calculated was : {}",
      relocd_jit.addr(),
      start_offset
    );

    Some((relocd_jit, sajit.stored.as_ptr()))
  })
}

fn reloc_abs() -> RelocKind {
  #[cfg(target_pointer_width = "32")]
  return RelocKind::Abs4;

  #[cfg(target_pointer_width = "64")]
  return RelocKind::Abs8;
}
