pub mod token;

#[cfg(test)]
mod parser {
  use crate::token::SasmToken;
  use logos::Logos;

  #[test]
  fn parse_basic() {
    let basic = r##"
import core.instrinsics.*;

export const HELLODATA = "#Hello World#";
export const FLOATING_POINT = 132.32;
export const INT_DATA = 132;

export fn main() {

}

export(C) fn hello2() {

}
    "##;

    let tokens = SasmToken::lexer(basic).collect::<Vec<_>>();
    println!("{tokens:?}");
  }
}
