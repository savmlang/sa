use std::{collections::HashMap, iter};

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{Ident, Token};
use ungrammar::Grammar;

use crate::r#struct::view::seq::Attribute;

use super::seq::Operation;

pub fn understand_sequence(
  grammar: &Grammar,
  tokens_map: &HashMap<&str, &str>,
  name: &Ident,
  operations: &Vec<Operation>,
  structsig: &TokenStream,
  labels: &[Ident],
  out: &mut TokenStream,
) {
  let mut struct_sig_bucket = Vec::new();
  let mut struct_params_bucket = Vec::new();

  for (count, op) in operations.iter().enumerate() {
    if let Operation::Node {
      label,
      directive,
      attribute,
      node,
      ..
    } = op
    {
      let is_repeat = match directive {
        Some("iter") => true,
        Some("merge") => false,
        _ => matches!(attribute, Attribute::Repeat),
      };

      if is_repeat {
        let nodename = format_ident!("{}", &grammar[**node].name);
        let iname = format_ident!("CallOnce{count}");

        struct_sig_bucket.push(quote! {
          #iname: FnOnce(&mut dyn Iterator<Item = Result<#nodename>>) -> ()
        });

        let parname = label.as_ref().map_or_else(
          || format_ident!("handle_{count}"),
          |x| format_ident!("handle_{x}"),
        );

        struct_params_bucket.push(quote! {
          #parname: #iname
        });
      }
    }
  }

  let instructions = {
    let mut ops = operations.iter().enumerate().peekable();

    iter::from_fn(move || {
      let (id, item) = ops.next()?;
      let next = ops.peek();

      let loop_type = if let Some(&(_, op)) = next {
        let mut loop_until = quote! {
          loop_until::
        };
        loop_until.extend(Token![<](Span::call_site()).into_token_stream());
        loop_until.extend(op.get_struct(grammar, tokens_map).into_token_stream());
        loop_until.extend(Token![,](Span::call_site()).into_token_stream());

        loop_until
      } else {
        let mut loop_all = quote! {
          loop_all::
        };

        loop_all.extend(Token![<](Span::call_site()).into_token_stream());
        loop_all
      };

      match item {
        Operation::Node {
          label,
          directive,
          node,
          attribute,
        } => {
          let name = label.map_or_else(|| format_ident!("param_{id}"), |x| format_ident!("{x}"));
          let parname = label.map_or_else(
            || format_ident!("handle_{id}"),
            |x| format_ident!("handle_{x}"),
          );
          let nodestruct = format_ident!("{}", &grammar[**node].name);

          if let Some(directive) = *directive {
            match directive {
              "iter" => {
                return Some(quote! {
                  let #name = core::iter::once(children.expect::<#nodestruct>());
                });
              }
              "merge" => {
                return Some(quote! {
                  let mut #name = #name.chain(
                    children.#loop_type #nodestruct>()
                      .map(|x|
                        x.map(|x| {
                          let mut view = x.view();
                          let data = x.understand(&mut view)?;

                          data.#name
                        })
                        .flatten()
                      )
                  );
                  #parname(&mut #name);

                  while let Some(_) = #name.next() {}
                  drop(#name);
                });
              }
              _ => unreachable!(),
            }
          };

          match attribute {
            Attribute::None => Some(quote! {
              let #name = children.expect::<#nodestruct>();
            }),
            Attribute::Optional => Some(quote! {
              let #name = children.is::<#nodestruct>();
            }),
            Attribute::Repeat => {
              return Some(quote! {
                let mut #name = children.loop_all::<#nodestruct>();

                #parname(&mut #name);
                // Clean up the rest of the mess
                while let Some(_) = #name.next() {}
                drop(#name);
              });
            }
          }
        }
        Operation::Token {
          label,
          token,
          attribute,
        } => {
          let name = label.map_or_else(|| format_ident!("param_{id}"), |x| format_ident!("{x}"));
          let tokenstruct = format_ident!(
            "{}Token",
            *tokens_map.get(&grammar[**token].name as &str).unwrap(),
          );

          match attribute {
            Attribute::None => Some(quote! {
              let #name = children.expect::<#tokenstruct>();
            }),
            Attribute::Optional => Some(quote! {
              let #name = children.is::<#tokenstruct>();
            }),
            Attribute::Repeat => unreachable!(),
          }
        }
      }
    })
  };

  out.extend(quote! {
    impl #name {
      #[inline]
      pub fn understand<'a, #( #struct_sig_bucket ),*>(
        &self,
        children: &'a mut AstViewHWND<impl Iterator<Item = SyntaxElement>, #name>,
        #( #struct_params_bucket ),*
      ) -> Result<#structsig> {
        #( #instructions )*

        children.ensure_eon()?;

        Ok(create_struct! {
          #name {
            #(#labels),*
          }
        })
      }
    }
  });
}
