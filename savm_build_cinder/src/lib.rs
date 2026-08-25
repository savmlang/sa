use object::{
  File as ObjFile, Object, ObjectSection, ObjectSymbol, RelocationFlags, RelocationTarget, elf, pe,
};
pub use sajit::relocations::RelocKind;
use std::{fs, path::PathBuf};

pub type Stencils<'a> = Vec<Stencil<'a>>;

pub mod emit;

pub struct Stencil<'a> {
  pub name: &'a str,
  pub mcemit: Box<[u8]>,
  pub relocs: Box<[SymbolReloc]>,
}

pub struct SymbolReloc {
  pub symbol: Box<str>,
  pub offset: u32,
  pub reloc: RelocKind,
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolRelocStatic {
  pub symbol: &'static str,
  pub offset: u32,
  pub reloc: RelocKind,
}

#[derive(Debug, Clone, Copy)]
pub struct TargetMachine {
  pub arch_32: bool,
  pub arm64: bool,
}

pub fn stenload<'a>(path: &PathBuf, name: &'a str, _mc: TargetMachine) -> Stencil<'a> {
  let buf = fs::read(path).expect("Unable to open object");
  let objfile = ObjFile::parse(&*buf).expect("ObjectFile Invalid");

  let jitfn = objfile
    .section_by_name(".jit_fn")
    .expect("Unable to get jit_fn");

  let mcemit = Box::from(
    jitfn
      .uncompressed_data()
      .expect("Unable to get uncompressed MC Emission data"),
  );

  let mut relocs = jitfn
    .relocations()
    .map(|(reloc_address, reloc)| {
      let symbol = match reloc.target() {
        RelocationTarget::Symbol(symbol) => objfile
          .symbol_by_index(symbol)
          .expect("Could not find symbol"),
        e => panic!("Unsupported relocatoin : {e:?}"),
      };

      let reloc = match reloc.flags() {
        RelocationFlags::Elf { r_type } => match r_type {
          elf::R_AARCH64_MOVW_GOTOFF_G0 => RelocKind::UserCustom { customdefined: 0 },
          elf::R_X86_64_64 => RelocKind::Abs8,
          e => {
            panic!("{e:?}");
          }
        },
        RelocationFlags::Coff { typ } => match typ {
          pe::IMAGE_REL_AMD64_ABSOLUTE | pe::IMAGE_REL_AMD64_ADDR64 => RelocKind::Abs8,
          _ => unreachable!(),
        },

        _ => unreachable!(),
      };

      SymbolReloc {
        offset: reloc_address as u32,
        reloc,
        symbol: Box::from(symbol.name().unwrap()),
      }
    })
    .collect::<Box<_>>();

  relocs.sort_unstable_by(|a, b| a.symbol.cmp(&b.symbol));

  Stencil {
    name,
    mcemit,
    relocs,
  }
}
