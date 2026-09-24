use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use ungrammar::Grammar;

pub fn generate_token(
  _grammar: &Grammar,
  name: &str,
  tokens_map: &HashMap<&str, &str>,
  out: &mut TokenStream,
) {
  let &name = tokens_map.get(name).expect("Could not map to name");

  let ident = format_ident!("{name}Token");
  let syntaxkind = format_ident!("{name}");
  out.extend(quote! {
    #[repr(transparent)]
    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    pub struct #ident {
      pub(crate) token: SyntaxToken
    }

    impl TokenMetadata for #ident {
      #[inline]
      fn kind() -> SyntaxKind {
        SyntaxKind::#syntaxkind
      }

      #[inline]
      fn try_cast(token: SyntaxToken) -> Option<Self> {
        (token.kind() == Self::kind()).then(|| Self { token })
      }
    }

    impl Deref for #ident {
      type Target = SyntaxToken;

      fn deref(&self) -> &Self::Target {
        &self.token
      }
    }
  });
}
