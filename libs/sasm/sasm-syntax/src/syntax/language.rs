use rowan::Language;

use crate::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SasmLanguage {}

impl Language for SasmLanguage {
  type Kind = SyntaxKind;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    assert!(raw.0 < SyntaxKind::__Last as u16);
    unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(kind as u16)
  }
}

// Convenient type aliases
pub type SyntaxNode = rowan::SyntaxNode<SasmLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<SasmLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<SasmLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<SasmLanguage>;
