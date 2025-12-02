//! This is the ACAoT compiler
//! `Aggressive Cycle-Adaptive Ordered Threading`` Compiler
//! It is a really quick compiler build to speed up like crazy

use crate::{BytecodeResolver, acaot::compiler::SyncCompiler};
use sart::ctr::Instruction;
use std::{
  collections::HashMap,
  io::{BufReader, Seek},
};

pub mod asyncmp;
pub mod compiler;

pub enum FirstPassInstruction {
  Inst(Instruction),
  Jmp { marker: u128 },
  Jz { marker: u128 },
  JzP { marker: u128 },
  JzR6 { marker: u128 },
  JzR6U { marker: u128 },
  Jnz { marker: u128 },
  JnzP { marker: u128 },
  JnzR6 { marker: u128 },
  JnzR6U { marker: u128 },
}

pub fn sync_compile<T: BytecodeResolver + Send + Sync + 'static>(
  resolver: &T,
  module: u32,
  region: u32,
) -> Box<[Instruction]> {
  let mut bytecode = resolver
    .resolve_bytecode_exact(module, region)
    .expect("This cannot error out");

  // Lets start with a good enough instruction size
  // Optimistic, 32-kb space
  let length = bytecode.stream_len().expect("ERROR") as usize;

  let reader = BufReader::new(bytecode);

  let mut compiler = SyncCompiler {
    code: Vec::with_capacity(length),
    resolver,
    reader,
    markers: HashMap::default(),
    depth: 0,
    to_add_to_vec_len: 0,
    module: 0,
  };

  compiler.first_pass();

  compiler.second_pass()
}
