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
  let struct_sig_bucket = {
    let mut iter_structsig = operations.iter().enumerate().peekable();

    iter::from_fn(move || {
      let (count, tok) = iter_structsig.next()?;
      let nxt = iter_structsig.peek();

      match tok {
        Operation::Node {
          directive,
          attribute,
          node,
          ..
        } => {
          let handle_repeat = || {
            let nodename = format_ident!("{}", &grammar[**node].name);
            let iname = format_ident!("CallOnce{count}");

            Some(quote! {
              #iname: FnOnce(
                &mut dyn Iterator<Item = Result<#nodename>>
              ) -> ()
            })
          };
          if let Some(directive) = *directive {
            match directive {
              "merge" if nxt.is_some() => {
                return handle_repeat();
              }
              // Ignore iter, merge
              "iter" | "merge" => return None,
              _ => unreachable!(),
            }
          }

          match attribute {
            Attribute::Repeat if nxt.is_some() => handle_repeat(),
            _ => None,
          }
        }
        _ => None,
      }
    })
  };

  let struct_params_bucket = {
    let mut iter_structsig = operations.iter().enumerate().peekable();
    iter::from_fn(move || {
      let (count, tok) = iter_structsig.next()?;
      let nxt = iter_structsig.peek();

      match tok {
        Operation::Node {
          label,
          directive,
          attribute,
          ..
        } => {
          let handle_repeat = || {
            let label = label.expect("Label name is required");
            let parname = format_ident!("handle_{label}");
            let iname = format_ident!("CallOnce{count}");

            Some(quote! {
              #parname: #iname
            })
          };
          if let Some(directive) = *directive {
            match directive {
              "merge" if nxt.is_some() => {
                return handle_repeat();
              }
              // Ignore iter, merge
              "iter" | "merge" => return None,
              _ => unreachable!(),
            }
          }

          match attribute {
            Attribute::Repeat if nxt.is_some() => handle_repeat(),
            _ => None,
          }
        }
        _ => None,
      }
    })
  };

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
          let nodestruct = format_ident!("{}", &grammar[**node].name);

          if let Some(directive) = *directive {
            match directive {
              "iter" => {
                return Some(quote! {
                  let #name = core::iter::once(children.expect::<#nodestruct>());
                });
              }
              "merge" if next.is_none() => {
                return Some(quote! {
                  let #name = #name.chain(
                    children.#loop_type #nodestruct>()
                      .map(|x|
                        x.map(|x| {
                          let view = x.view();
                          let data = x.understand(&mut view)?;

                          Ok(data.#name)
                        })
                        .flatten()
                      )
                  );
                });
              }
              "merge" if next.is_some() => {
                let parname = format_ident!("handle_{id}");

                return Some(quote! {
                  let #name = #name.chain(
                    children.#loop_type #nodestruct>()
                      .map(|x|
                        x.map(|x| {
                          let view = x.view();
                          let data = x.understand(&mut view)?;

                          data.#name
                        })
                        .flatten()
                      )
                  );
                  #parname(&mut #name);
                  // Clean up the rest of the mess
                  while let Some(_) = #name.next() {}
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
              let expr = quote! {
                let #name = children.loop_all::<#nodestruct>();
              };

              if next.is_some() {
                let parname = format_ident!("handle_{id}");
                return Some(quote! {
                  #expr
                  #parname(&mut #name);
                  // Clean up the rest of the mess
                  while let Some(_) = #name.next() {}
                });
              }

              Some(expr)
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
