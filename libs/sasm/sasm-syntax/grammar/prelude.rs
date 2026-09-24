use proc_macro2::TokenStream;
use quote::quote;

pub fn generate_prelude(out: &mut TokenStream) {
  out.extend(quote! {
    // Auto-generated. DO NOT EDIT.

    use std::ops::Deref;
    use crate::syntax::*;
    use rowan::ast::AstNode;

    pub type SyntaxToken = rowan::SyntaxToken<SasmLanguage>;
    pub type SyntaxNode = rowan::SyntaxNode<SasmLanguage>;

    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum AstSyntaxErrorCode {
      // We shall leave `0` as invalid bitpattern
      MissingMandatoryElement = 1,
    }

    pub type Result<T> = core::result::Result<T, AstSyntaxErrorCode>;

    pub trait TokenMetadata: Sized {
      fn kind() -> SyntaxKind;
      fn try_cast(token: SyntaxToken) -> Option<Self>;
    }

    pub trait NodeMetadata: Sized {
      type SyntaxKind: SyntaxKindWrapper;

      fn kind() -> &'static [Self::SyntaxKind];
      fn current_kind(&self) -> Self::SyntaxKind;
    }

    pub trait SyntaxKindWrapper: Sized {
      fn as_kind(&self) -> SyntaxKind;
      fn from_kind(kind: SyntaxKind) -> Option<Self>;
    }

    impl SyntaxKindWrapper for SyntaxKind {
      fn as_kind(&self) -> Self {
        *self
      }

      fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(kind)
      }
    }
  });
}
