use std::collections::HashMap;

use proc_macro2::{TokenStream, TokenTree};
use quote::{format_ident, quote};
use syn::Ident;
use ungrammar::{Grammar, Node, Rule, Token};

use crate::r#struct::view::generator;

pub fn understand_sequence(
  name: &Ident,
  name_dataview: &Ident,
  grammar: &Grammar,
  tokens_map: &HashMap<&str, &str>,
  rules: &[Rule],
  out: &mut TokenStream,
) {
  let operations = rules
    .into_iter()
    .map(|rule| {
      let op: Operation;
      match rule {
        Rule::Alt(_) => unreachable!("Alternates are not allowed inside of rules"),
        Rule::Seq(_) => unimplemented!("Nested sequences are not allowed"),
        Rule::Rep(r) => match &r as &Rule {
          Rule::Token(token) => {
            op = ftoken(None, token, Attribute::Repeat);
          }
          Rule::Node(node) => {
            op = fnode(None, node, Attribute::Repeat);
          }
          _ => unimplemented!(),
        },

        Rule::Labeled { label, rule } => match &rule as &Rule {
          Rule::Alt(_) => unimplemented!("Labelled alternates are not allowed"),
          Rule::Labeled { .. } => unimplemented!("Nested labels are not allowed"),
          Rule::Seq(_) => unimplemented!("Labelled sequences are not allowed"),

          Rule::Rep(r) => match &r as &Rule {
            Rule::Token(token) => {
              op = ftoken(Some(label), token, Attribute::Repeat);
            }
            Rule::Node(node) => {
              op = fnode(Some(label), node, Attribute::Repeat);
            }
            _ => unimplemented!(),
          },
          Rule::Token(token) => {
            op = ftoken(Some(label), token, Attribute::None);
          }
          Rule::Node(node) => {
            op = fnode(Some(label), node, Attribute::None);
          }
          Rule::Opt(r) => match &r as &Rule {
            Rule::Token(token) => {
              op = ftoken(Some(label), token, Attribute::Optional);
            }
            Rule::Node(node) => {
              op = fnode(Some(label), node, Attribute::Optional);
            }
            _ => unimplemented!(),
          },
        },

        Rule::Node(node) => {
          op = fnode(None, node, Attribute::None);
        }
        Rule::Token(token) => {
          op = ftoken(None, token, Attribute::None);
        }
        Rule::Opt(opt) => match &opt as &Rule {
          Rule::Node(node) => {
            op = fnode(None, node, Attribute::Optional);
          }
          Rule::Token(token) => {
            op = ftoken(None, token, Attribute::Optional);
          }
          _ => unimplemented!(),
        },
      }
      op
    })
    .collect::<Vec<_>>();

  // generate struct defs
  let (structsig, labels) = {
    let mut generics = vec![];

    let lastid = operations.len() - 1;
    let generation = operations
      .iter()
      .enumerate()
      .filter_map(|(id, x)| match x {
        Operation::Node {
          label,
          directive,
          node,
          attribute,
        } => {
          let identlabel = label
            .as_ref()
            .map_or_else(|| format_ident!("param_{id}"), |x| format_ident!("{x}"));
          let nodestruct = format_ident!("{}", &grammar[**node].name);

          if let Some(op) = directive {
            match *op {
              "merge" if lastid == id => {
                let generic = format_ident!("Iter{id}");
                let val = Some(quote! {
                  pub #identlabel: Result<#generic>
                });

                generics.push((generic, nodestruct));

                return val;
              }
              // Ignore merges, iters
              "iter" | "merge" => return None,
              _ => unreachable!(),
            }
          }

          match attribute {
            Attribute::None => Some(quote! {
              pub #identlabel: Result<#nodestruct>
            }),
            Attribute::Optional => Some(quote! {
              pub #identlabel: Result<Option<#nodestruct>>
            }),
            Attribute::Repeat if lastid == id => {
              let generic = format_ident!("Iter{id}");
              let val = Some(quote! {
                pub #identlabel: Result<#generic>
              });

              generics.push((generic, nodestruct));

              val
            }
            _ => None,
          }
        }
        Operation::Token {
          label,
          token,
          attribute,
        } => {
          let identlabel = label
            .as_ref()
            .map_or_else(|| format_ident!("param_{id}"), |x| format_ident!("{x}"));
          let tokenstruct = format_ident!(
            "{}Token",
            tokens_map
              .get(&(&grammar[**token].name as &str))
              .expect("Could not map to name"),
          );

          match attribute {
            Attribute::None => Some(quote! {
              pub #identlabel: Result<#tokenstruct>
            }),
            Attribute::Optional => Some(quote! {
              pub #identlabel: Result<Option<#tokenstruct>>
            }),
            Attribute::Repeat if lastid == id => {
              let generic = format_ident!("Iter{id}");
              let val = Some(quote! {
                pub #identlabel: Result<#generic>
              });

              generics.push((generic, tokenstruct));

              val
            }
            _ => None,
          }
        }
      })
      .collect::<Vec<_>>();

    let (generics, tokenstructs) = generics.into_iter().unzip::<_, _, Vec<_>, Vec<_>>();

    let structlabels = generation
      .iter()
      .map(|x| {
        let mut streamiter = x.clone().into_iter();
        streamiter.next();
        let data = streamiter.next().unwrap();

        match data {
          TokenTree::Ident(x) => x,
          _ => unreachable!(),
        }
      })
      .collect::<Vec<_>>();

    let definition = quote! {
      pub struct #name_dataview<
        #( #generics: Iterator<Item = Result<#tokenstructs>> ),*
      >
      {
        #( #generation ),*
      }
    };

    let structsignature =
      quote! { #name_dataview<#( impl Iterator<Item = Result<#tokenstructs>> + 'a ),*> };

    out.extend(quote! {
      #definition
    });

    (structsignature, structlabels)
  };

  generator::understand_sequence(
    grammar,
    tokens_map,
    name,
    &operations,
    &structsig,
    &labels,
    out,
  );
}

fn fnode<'a>(txt: Option<&'a str>, node: &'a Node, attribute: Attribute) -> Operation<'a> {
  let (label, directive) = txt.map(process_label).unzip();
  Operation::Node {
    label,
    directive: directive.flatten(),
    node,
    attribute,
  }
}

fn ftoken<'a>(txt: Option<&'a str>, token: &'a Token, attribute: Attribute) -> Operation<'a> {
  Operation::Token {
    label: txt,
    token,
    attribute,
  }
}

// label -> label,directive
fn process_label<'a>(label: &'a str) -> (&'a str, Option<&'a str>) {
  let Some(val) = label.rsplit_once("_") else {
    return (label, None);
  };

  (val.1, Some(val.0))
}

pub enum Operation<'a> {
  Node {
    label: Option<&'a str>,
    directive: Option<&'a str>,

    node: &'a Node,
    attribute: Attribute,
  },

  Token {
    label: Option<&'a str>,
    token: &'a Token,

    attribute: Attribute,
  },
}

impl<'a> Operation<'a> {
  pub fn get_struct(&self, grammar: &Grammar, tokens_map: &HashMap<&str, &str>) -> Ident {
    match self {
      Self::Node { node, .. } => {
        format_ident!("{}", &grammar[**node].name)
      }
      Self::Token { token, .. } => {
        format_ident!(
          "{}Token",
          tokens_map.get(&grammar[**token].name as &str).unwrap()
        )
      }
    }
  }
}

pub enum Attribute {
  None,
  Optional,
  Repeat,
}
