use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use sajit::relocations::RelocKind;

use crate::{Stencil, SymbolReloc};

pub fn cinderjit_file(stencils: &[Stencil]) -> String {
  let mut old = quote! {};

  for stencil in stencils {
    let name = stencil.name;
    let bytes = &*stencil.mcemit;
    let reloc = &*stencil.relocs;

    let name_ident = {
      let mut namechars = name.chars();
      let lt0 = namechars.next().unwrap().to_uppercase().collect::<String>();
      let ltrest = namechars.collect::<String>();
      format_ident!("CinderJIT{}{}", lt0, ltrest)
    };

    old.extend(quote! {
      pub mod #name_ident {
        use savmbuild_cinder::{SymbolReloc, RelocKind};

        pub static MCEMIT: &'static [u8] = &[#(#bytes),*];

        pub static RELOC: &'static [SymbolRelocStatic] = &[#(#reloc),*];
      }
    });
  }

  old.to_string()
}

impl ToTokens for SymbolReloc {
  fn to_tokens(&self, tokens: &mut TokenStream) {
    let offset = self.offset;
    let symbol = &*self.symbol;
    let reloc = match self.reloc {
      RelocKind::Abs8 => format_ident!("Abs8"),
      RelocKind::Abs4 => format_ident!("Abs4"),
      _ => unreachable!(),
    };

    tokens.extend(quote! {
      SymbolRelocStatic {
        offset: #offset,
        symbol: #symbol,
        reloc: RelocKind::#reloc
      }
    });
  }
}
