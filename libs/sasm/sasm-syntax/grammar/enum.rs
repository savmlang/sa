use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use ungrammar::{Grammar, Rule};

pub fn generate_enum(grammar: &Grammar, name: &str, rule: &Rule, out: &mut TokenStream) {
  let name = format_ident!("{name}");
  let enum_name = format_ident!("{name}Variants");

  let variants = extract_enum_variants(grammar, rule);

  let generator = quote! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum #name {
      #( #variants(#variants) ),*
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub enum #enum_name {
      #( #variants ),*
    }

    impl NodeMetadata for #name {
      type SyntaxKind = #enum_name;

      fn kind() -> &'static [#enum_name] {
        &[
          #( #enum_name::#variants ),*
        ]
      }

      fn current_kind(&self) -> #enum_name {
        match self {
          #( Self::#variants(_) => { #enum_name::#variants })*
        }
      }
    }


    impl SyntaxKindWrapper for #enum_name {
      fn as_kind(&self) -> SyntaxKind {
        match self {
          #( Self::#variants => { SyntaxKind::#variants })*
        }
      }

      fn from_kind(kind: SyntaxKind) -> Option<Self> {
        Some(
          match kind {
            #( SyntaxKind::#variants => { #enum_name::#variants })*
            _ => return None,
          }
        )
      }
    }

    impl AstNode for #name {
      type Language = SasmLanguage;

      fn can_cast(kind: SyntaxKind) -> bool {
        matches!(kind, #( SyntaxKind::#variants )|*)
      }

      fn cast(syntax: SyntaxNode) -> Option<Self> {
        Some(
          match syntax.kind() {
            #( SyntaxKind::#variants => { Self::#variants(#variants::cast(syntax)?) })*
            _ => return None
          }
        )
      }

      fn syntax(&self) -> &SyntaxNode {
        match self {
          #( Self::#variants(v) => { v.syntax() })*
        }
      }
    }

    impl Deref for #name {
      type Target = SyntaxNode;

      fn deref(&self) -> &Self::Target {
        self.syntax()
      }
    }
  };

  out.extend(generator);
}

fn extract_enum_variants<'a>(grammar: &'a Grammar, rule: &'a Rule) -> Vec<Ident> {
  let mut variants = Vec::new();

  if let Rule::Alt(alts) = rule {
    for alt in alts {
      match alt {
        Rule::Node(id) => {
          variants.push(format_ident!("{}", grammar[*id].name.as_str()));
        }
        _ => panic!("Found structures other than Node inside of an alternate `|` branch"),
      }
    }
  }
  variants
}

pub enum Class {
  Token,
  Node,
  None,
}
