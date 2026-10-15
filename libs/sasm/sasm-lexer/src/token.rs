use logos::Logos;

#[derive(Debug, Logos, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SasmToken {
  // Do not delete the `ENUM` declaration - used by automated scripts
  /* ENUM begin */
  // Core
  #[regex(r"[ \t\r\n]+")]
  WhiteSpace,
  #[regex(r"///[^\r\n]*", priority = 3, allow_greedy = true)]
  DocComment,
  #[regex(r"//[^\r\n]*", priority = 2, allow_greedy = true)]
  SingleLineComment,
  #[regex(r"/\*([^*]|\*+[^*/])*\*+/", priority = 2)]
  BlockComment,

  // Keywords
  #[token("return")]
  Return,
  #[token("extern")]
  Extern,
  #[token("import")]
  Import,
  #[token("const")]
  Const,
  #[token("let")]
  Let,
  #[token("if")]
  If,
  #[token("else")]
  Else,
  #[token("fn")]
  Fn,
  #[token("export")]
  Export,
  #[token("mut")]
  Mut,
  #[token("aware")]
  Aware,
  #[token("as")]
  As,
  #[token("struct")]
  Struct,
  #[token("enum")]
  Enum,
  #[token("union")]
  Union,
  #[regex("@[A-Za-z][A-Za-z0-9]+")]
  Directive,

  // Punctuations
  #[token(";")]
  SemiColon,
  #[token(":")]
  Colon,

  #[token("(")]
  LBracket,
  #[token(")")]
  RBracket,

  #[token("{")]
  LCurly,
  #[token("}")]
  RCurly,

  #[token("[")]
  LSquare,
  #[token("]")]
  RSquare,

  #[token("<")]
  LAngle,
  #[token(">")]
  RAngle,

  #[token("@")]
  At,
  #[token("$")]
  Dollar,
  #[token("%")]
  Percent,

  #[token("=")]
  Equals,
  #[token(",")]
  Comma,
  #[token(".")]
  Dot,
  #[token("+")]
  Plus,
  #[token("-")]
  Minus,
  #[token("/")]
  Slash,
  #[token("*")]
  Asterisk,
  #[token("|")]
  Pipe,

  // Conditionals
  #[token("==")]
  EqEq,
  #[token("!=")]
  NeEq,
  #[token(">=")]
  GtEq,
  #[token("<=")]
  LtEq,

  // Literals
  #[regex(r#"(true|false)"#)]
  BoolLit,
  #[regex(r#"[0-9]+\.[0-9]+"#)]
  FpLit,
  #[regex(r#"[0-9]+"#)]
  NumLit,
  #[regex(r#""([^"\\]|\\.)*""#)]
  StrLit,

  // Identifiers
  #[regex(r#"[A-Za-z_][A-Za-z0-9_]*"#)]
  Ident,
  #[regex(r#"r#[A-Za-z0-9_]+"#)]
  RawIdent,
  /* ENUM end */
}
