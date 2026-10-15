use std::{collections::HashMap, ops::Deref};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Ident;
use ungrammar::{Grammar, Rule};

pub mod generator;
mod seq;

pub fn understand_view(
  name: &Ident,
  grammar: &Grammar,
  tokens_map: &HashMap<&str, &str>,
  rule: &Rule,
  out: &mut TokenStream,
) {
  let name_dataview = format_ident!("{name}DataView");

  match rule {
    Rule::Alt(_) => panic!("Alternate rules not allowed inside of structures"),
    Rule::Node(_) => unimplemented!("Node Referrering to ONLY another node is not allowed"),
    Rule::Opt(_) => unimplemented!("SINGLE Optionals inside of Node are not implemented"),
    Rule::Labeled { .. } => unimplemented!("SINGLE Nodes are unlabelled always"),

    Rule::Seq(rules) => {
      seq::understand_sequence(name, &name_dataview, grammar, tokens_map, &rules, out)
    }

    Rule::Token(x) => {
      let token_name = tokens_map
        .get(&grammar[*x].name as &str)
        .expect("Unable to resolve token name");
      let token_name = format_ident!("{token_name}Token");

      out.extend(quote! {
        pub struct #name_dataview {
          pub token: Result<#token_name>,
        }

        impl #name {
          pub fn understand(&self, children: &mut AstViewHWND<impl Iterator<Item = SyntaxElement>, #name>) -> Result<#name_dataview> {
            let token = children.expect::<#token_name>();
            children.ensure_eon()?;

            Ok(#name_dataview {
              token
            })
          }
        }
      });
    }

    // A*
    Rule::Rep(rule) => {
      match rule.deref() {
        Rule::Node(x) => {
          let node_name = &grammar[*x].name as &str;
          let node_name = format_ident!("{node_name}");

          out.extend(quote! {
            pub struct #name_dataview<T>
            where T: Iterator<Item = Result<#node_name>>
            {
              pub items: T,
            }

            impl #name {
              pub fn understand<'a>(&self, children: &'a mut AstViewHWND<impl Iterator<Item = SyntaxElement>, #name>) -> #name_dataview<impl Iterator<Item = Result<#node_name>> + 'a> {
                let items = children.loop_all::<#node_name>();

                #name_dataview {
                  items
                }
              }
            }
          });
        }
        _ => unreachable!("Root repetition of only an UNNAMED NODE is allowed"),
      };
    }
  }
}
