use sasm_lexer::token::SasmToken;

pub mod language;
pub use language::SasmLanguage;

include!(concat!(env!("OUT_DIR"), "/", "autoenum.rs"));
