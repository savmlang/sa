use std::ops::Range;

use rowan::{GreenNodeBuilder, NodeCache};
use sasm_lexer::token::SasmToken;

pub mod macros;

pub struct LexToken {
  pub token: SasmToken,
  pub span: Range<usize>,
}

pub struct Parser<'a, 'b> {
  pub builder: GreenNodeBuilder<'a>,
  pub tokens: Cursor<'b, u8>,
}

impl<'a, 'b> Parser<'a, 'b> {
  pub fn build(cache: &'a mut NodeCache, tokens: &'b [u8]) -> Self {
    Self {
      builder: GreenNodeBuilder::with_cache(cache),
      tokens: Cursor::new(tokens),
    }
  }
}

pub struct Cursor<'a, T> {
  slice: &'a [T],
  pos: usize,
}

impl<'a, T: Copy> Cursor<'a, T> {
  pub fn new(slice: &'a [T]) -> Self {
    Self { slice, pos: 0 }
  }

  #[inline]
  pub fn next(&mut self) -> Option<T> {
    let item = self.slice.get(self.pos)?;
    self.pos += 1;
    Some(*item)
  }

  #[inline]
  pub fn peek(&self) -> Option<T> {
    self.nth(0)
  }

  #[inline]
  pub fn nth(&self, n: usize) -> Option<T> {
    self.slice.get(self.pos + n).copied()
  }

  #[inline]
  pub fn checkpoint(&self) -> usize {
    self.pos
  }

  #[inline]
  pub fn restore(&mut self, mark: usize) {
    self.pos = mark;
  }
}
