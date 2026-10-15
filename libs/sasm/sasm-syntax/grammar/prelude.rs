use proc_macro2::TokenStream;
use quote::quote;

pub fn generate_prelude(out: &mut TokenStream) {
  out.extend(quote! {
    // Auto-generated. DO NOT EDIT.

    use crate::{create_struct, syntax::*};
    use std::ops::Deref;
    use rowan::ast::AstNode;
  });
}
