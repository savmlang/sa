use sasm_lexer::token::SasmToken;

pub mod language;
pub use language::SasmLanguage;

#[macro_use]
pub mod prelude;
pub use prelude::*;

include!(concat!(env!("OUT_DIR"), "/", "autoenum.rs"));
