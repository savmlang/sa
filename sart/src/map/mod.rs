use crate::{boxed::ContainedRTBox, ctr::Instruction};
pub use hashbrown::HashMap;

use std::sync::LazyLock;

/// Please note that the heap is supposed to only have 32-bit data
///
/// It also is packaged data (oh my god how much optimizations)
/// 1st 32=btis (i.e. u32) is module id
/// 2nd 32=bits (i.e. u32) is Section id
/// 3rd 64=bits (i.e. u64) is variable name
pub type Heap = HashMap<u128, ContainedRTBox, ahash::RandomState>;

/// This `u64` is a packed data
/// 1st 32=bits (i.e. u32) is module id
/// 2nd 32=bit (i.e. u32) is module section
pub type CompiledCode = HashMap<
  u64,
  LazyLock<Box<[Instruction]>, Box<dyn FnOnce() -> Box<[Instruction]>>>,
  ahash::RandomState,
>;
