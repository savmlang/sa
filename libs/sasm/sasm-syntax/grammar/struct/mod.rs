use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use ungrammar::{Grammar, Rule};

pub mod view;

pub fn generate_struct(
  grammar: &Grammar,
  tokens_map: &HashMap<&str, &str>,
  name: &str,
  rule: &Rule,
  out: &mut TokenStream,
) {
  let name = format_ident!("{name}");

  view::understand_view(&name, grammar, tokens_map, rule, out);

  let generator = quote! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct #name {
      pub syntax: SyntaxNode,
    }

    impl #name {
      pub fn view(&self) -> AstViewHWND<impl Iterator<Item = SyntaxElement>, #name> {
        self.syntax.astview()
      }
    }

    impl UnifiedMetadata for #name {
      type SyntaxKind = SyntaxKind;

      const KIND: &'static [SyntaxKind] = &[
        SyntaxKind::#name
      ];
      const RAW_KIND: &'static [SyntaxKind] = &[
        SyntaxKind::#name
      ];

      fn current_kind(&self) -> SyntaxKind {
        SyntaxKind::#name
      }

      fn castable(kind: SyntaxKind) -> bool {
        Self::can_cast(kind)
      }

      fn try_cast(syntax: SyntaxElement) -> Option<Self> {
        Self::cast(syntax.node()?)
      }
    }

    impl AstNode for #name {
      type Language = SasmLanguage;

      fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::#name
      }

      fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
          Some(Self { syntax })
        } else {
          None
        }
      }

      fn syntax(&self) -> &SyntaxNode {
        &self.syntax
      }
    }

    impl Deref for #name {
      type Target = SyntaxNode;

      fn deref(&self) -> &Self::Target {
        self.syntax()
      }
    }
  };

  out.extend(generator);
}
