// pub mod r;

pub(crate) mod autogen {
  include!(concat!(env!("OUT_DIR"), "/", "generated.rs"));
}

pub mod syntax;
