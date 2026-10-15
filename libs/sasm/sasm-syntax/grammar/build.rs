use std::{env, fs, path::Path};

use proc_macro2::TokenStream;
use ungrammar::{Grammar, Rule};

pub mod lex;
use lex::parse_enum;

pub mod r#enum;
use r#enum::generate_enum;

pub mod r#struct;
use r#struct::generate_struct;

pub mod r#prelude;
use r#prelude::generate_prelude;

pub mod tokens;
use tokens::generate_token;

use crate::lex::extend::generate_glue;

fn main() {
  println!("cargo:rerun-if-changed=grammar/sasm.ungram");
  println!("cargo:rerun-if-changed=../sasm-lexer/src/token.rs");

  let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set by Cargo");

  let grammar_text =
    fs::read_to_string("grammar/sasm.ungram").expect("Failed to read grammar/sasm.ungram");
  let tokens =
    fs::read_to_string("../sasm-lexer/src/token.rs").expect("Failed to read lexer tokens");

  let tokens_map = parse_enum(&tokens);

  let grammar: Grammar = grammar_text
    .parse()
    .expect("Failed to parse sasm.ungram schema");

  // Write generated
  {
    let mut out = TokenStream::new();
    generate_prelude(&mut out);

    for token in grammar.tokens() {
      let name = &grammar[token].name as &str;
      generate_token(&grammar, name, &tokens_map, &mut out);
    }

    for node in grammar.iter() {
      let node_data = &grammar[node];
      let name = &node_data.name;

      if is_alt_rule(&node_data.rule) {
        generate_enum(&grammar, name, &node_data.rule, &mut out);
      } else {
        generate_struct(&grammar, &tokens_map, name, &node_data.rule, &mut out);
      }
    }

    eprintln!("GENERATED:\n{}", out);
    let file: syn::File = syn::parse2(out).unwrap();
    let out = prettyplease::unparse(&file);

    let dest_path = Path::new(&out_dir).join("generated.rs");
    fs::write(&dest_path, out).expect("Failed to write generated.rs");
  }

  // Write macro for auto enumeration
  {
    let glue = generate_glue(&grammar, &tokens_map);

    let file: syn::File = syn::parse2(glue).unwrap();
    let out = prettyplease::unparse(&file);

    let dest_path = Path::new(&out_dir).join("autoenum.rs");
    fs::write(&dest_path, out).expect("Failed to write autoenum.rs");
  }
}

fn is_alt_rule(rule: &Rule) -> bool {
  matches!(rule, Rule::Alt(_))
}
