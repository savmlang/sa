pub mod parser;

pub mod autogen {
  include!(concat!(env!("OUT_DIR"), "/", "generated.rs"));
}

pub mod syntax;
