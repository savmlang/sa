use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use sajit::relocations::RelocKind;

use crate::{Stencil, SymbolReloc};

pub fn cinderjit_file(stencils: &[Stencil]) -> String {
  let mut old = quote! {
    use savmbuild_cinder::SymbolRelocStatic;

    pub struct Stencil {
      pub name: &'static str,
      pub mcemit: &'static [u8],
      pub reloc: &'static [SymbolRelocStatic]
    }
  };

  for stencil in stencils {
    let name = stencil.name;
    let bytes = &*stencil.mcemit;
    let reloc = &*stencil.relocs;

    let name_lower_ident = format_ident!("inst_{}", name);
    let name_ident = {
      let mut namechars = name.chars();
      let lt0 = namechars.next().unwrap().to_uppercase().collect::<String>();
      let ltrest = namechars.collect::<String>();
      format_ident!("CinderJIT{}{}", lt0, ltrest)
    };

    old.extend(quote! {
      pub static #name_lower_ident: Stencil = Stencil {
        name: #name,
        mcemit: #name_ident::MCEMIT,
        reloc: #name_ident::RELOC
      };

      pub mod #name_ident {
        use savmbuild_cinder::SymbolRelocStatic;

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
      RelocKind::Abs8 => quote! { savmbuild_cinder::RelocKind::Abs8 },
      RelocKind::Abs4 => quote! { savmbuild_cinder::RelocKind::Abs4 },
      RelocKind::UserCustom { customdefined } => {
        quote! { savmbuild_cinder::RelocKind::UserCustom { customdefined: #customdefined } }
      }
      _ => unreachable!(),
    };

    tokens.extend(quote! {
      SymbolRelocStatic {
        offset: #offset,
        symbol: #symbol,
        reloc: #reloc
      }
    });
  }
}
