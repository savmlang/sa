use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use ungrammar::{Grammar, Rule};

pub fn generate_struct(_grammar: &Grammar, name: &str, _rule: &Rule, out: &mut TokenStream) {
  let name = format_ident!("{name}");
  let generator = quote! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct #name {
      pub(crate) syntax: SyntaxNode,
    }

    impl NodeMetadata for #name {
      type SyntaxKind = SyntaxKind;

      fn kind() -> &'static [SyntaxKind] {
        &[SyntaxKind::#name]
      }

      fn current_kind(&self) -> SyntaxKind {
        SyntaxKind::#name
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
