//! ## ACAoT Cinder
//! A copy-patch JIT compiler designed to be faster than the current
//! tier 1 compiler (Cranelift)

pub mod emit {
  include!(concat!(env!("OUT_DIR"), "/cinderjit.rs"));
}
