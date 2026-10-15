use std::{
  iter::{self, Peekable},
  marker::PhantomData,
};

use rowan::TextRange;

use crate::syntax::{SasmLanguage, SyntaxKind};

pub type SyntaxToken = rowan::SyntaxToken<SasmLanguage>;
pub type SyntaxNode = rowan::SyntaxNode<SasmLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<SasmLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<SasmLanguage>;

pub trait AsSpecificKind {
  fn token(self) -> Option<SyntaxToken>;
  fn node(self) -> Option<SyntaxNode>;
}

impl AsSpecificKind for SyntaxElement {
  fn node(self) -> Option<SyntaxNode> {
    self.into_node()
  }

  fn token(self) -> Option<SyntaxToken> {
    self.into_token()
  }
}

impl SyntaxKind {
  #[inline]
  pub const fn is_trivia(self) -> bool {
    matches!(
      self,
      SyntaxKind::WhiteSpace
        | SyntaxKind::Tombstone
        | SyntaxKind::BlockComment
        | SyntaxKind::SingleLineComment
    )
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AstSyntaxErrorCode {
  MissingElement {
    range: TextRange,
  },
  UnexpectedElement {
    range: TextRange,
    got: SyntaxKind,
    expected: &'static [&'static [SyntaxKind]; 2],
  },
  EON {
    range: TextRange,
  },

  // An ICE
  CastFailure,
}

pub type Result<T> = core::result::Result<T, AstSyntaxErrorCode>;

pub trait UnifiedMetadata: Sized {
  type SyntaxKind: SyntaxKindWrapper + 'static;

  const KIND: &'static [Self::SyntaxKind];
  const RAW_KIND: &'static [SyntaxKind];

  fn current_kind(&self) -> Self::SyntaxKind;
  fn castable(kind: SyntaxKind) -> bool;
  fn try_cast(token: SyntaxElement) -> Option<Self>;
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

pub struct AstViewHWND<Iter: Iterator<Item = SyntaxElement>, For> {
  pub it: Peekable<Iter>,
  pub range: TextRange,

  _data: PhantomData<For>,
}

pub trait AstView {
  fn astview<T>(&self) -> AstViewHWND<impl Iterator<Item = SyntaxElement>, T>;
}

impl AstView for SyntaxNode {
  fn astview<T>(&self) -> AstViewHWND<impl Iterator<Item = SyntaxElement>, T> {
    let range = self.text_range();

    AstViewHWND {
      it: self
        .children_with_tokens()
        .filter(|x| !x.kind().is_trivia())
        .peekable(),
      range,
      _data: PhantomData,
    }
  }
}

impl<T, E> AstViewHWND<T, E>
where
  T: Iterator<Item = SyntaxElement>,
{
  pub fn expect<Meta: UnifiedMetadata>(&mut self) -> Result<Meta> {
    let peeked = self
      .it
      .peek()
      .ok_or(AstSyntaxErrorCode::MissingElement { range: self.range })?;

    if Meta::castable(peeked.kind()) {
      let elm = self.it.next().expect("peeked item disappeared");
      return Meta::try_cast(elm).ok_or(AstSyntaxErrorCode::CastFailure);
    }

    Err(AstSyntaxErrorCode::UnexpectedElement {
      range: peeked.text_range(),
      got: peeked.kind(),
      expected: const { &[Meta::RAW_KIND, &[]] },
    })
  }

  pub fn is<Meta: UnifiedMetadata>(&mut self) -> Result<Option<Meta>> {
    let Some(peeked) = self.it.peek() else {
      return Ok(None);
    };

    if Meta::castable(peeked.kind()) {
      let elm = self.it.next().expect("peeked item disappeared");
      return Ok(Some(
        Meta::try_cast(elm).ok_or(AstSyntaxErrorCode::CastFailure)?,
      ));
    }

    Ok(None)
  }

  pub fn loop_all<'a, Item: UnifiedMetadata>(
    &'a mut self,
  ) -> impl Iterator<Item = Result<Item>> + 'a {
    iter::from_fn(|| {
      let expected = const { &[Item::RAW_KIND, &[]] };
      let token = self.it.next()?;

      let range = token.text_range();
      let got = token.kind();

      let meta = Item::try_cast(token);

      Some(meta.ok_or(AstSyntaxErrorCode::UnexpectedElement {
        range,
        expected,
        got,
      }))
    })
  }

  pub fn loop_until<'a, Until: UnifiedMetadata, Item: UnifiedMetadata>(
    &'a mut self,
  ) -> impl Iterator<Item = Result<Item>> + 'a {
    iter::from_fn(|| {
      let expected = const { &[Item::RAW_KIND, Until::RAW_KIND] };

      let peekd = self.it.peek()?;

      if Until::castable(peekd.kind()) {
        return None;
      }

      let next = self.it.next()?;

      let range = next.text_range();
      let got = next.kind();

      if Item::castable(next.kind()) {
        return Some(Item::try_cast(next).ok_or(AstSyntaxErrorCode::CastFailure));
      }

      Some(Err(AstSyntaxErrorCode::UnexpectedElement {
        range,
        expected,
        got,
      }))
    })
  }

  pub fn ensure_eon(&mut self) -> Result<()> {
    match self.it.peek() {
      Some(e) => Err(AstSyntaxErrorCode::UnexpectedElement {
        range: e.text_range(),
        expected: const { &[&[]; 2] },
        got: e.kind(),
      }),
      None => Ok(()),
    }
  }
}

#[macro_export]
macro_rules! create_struct {
  ($name:ident { $($t:tt)* }) => {
    pastey::paste! {
      [<$name DataView>] {
        $($t)*
      }
    }
  };
}
