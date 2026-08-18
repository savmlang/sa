use object::{
  File as ObjFile, Object, ObjectSection, ObjectSymbol, RelocationKind, RelocationTarget,
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
}

pub fn stenload<'a>(path: &PathBuf, name: &'a str, mc: TargetMachine) -> Stencil<'a> {
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

      let reloc = match reloc.kind() {
        RelocationKind::Absolute => {
          if mc.arch_32 {
            RelocKind::Abs4
          } else {
            RelocKind::Abs8
          }
        }
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
