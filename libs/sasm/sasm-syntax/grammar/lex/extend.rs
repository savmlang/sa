use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use ungrammar::Grammar;

pub fn generate_glue(grammar: &Grammar, tokens_map: &HashMap<&str, &str>) -> TokenStream {
  let lexervals = tokens_map
    .values()
    .map(|&val| format_ident!("{val}"))
    .collect::<Vec<_>>();

  let grammar = grammar.iter().map(|node| {
    let name = &grammar[node].name;

    format_ident!("{name}")
  });

  quote! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(u16)]
    pub enum SyntaxKind {
      // Special Kinds
      Tombstone = 0,
      Error,
      Eof,

      #( #lexervals ),*,
      #( #grammar ),*,

      #[doc(hidden)]
      __Last
    }

    impl From<SasmToken> for SyntaxKind {
      fn from(token: SasmToken) -> Self {
        match token {
          #(
            SasmToken::#lexervals => Self::#lexervals
          ),*
        }
      }
    }
  }
}
