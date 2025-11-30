//! This is the ACAoT compiler
//! `All Compiled Ahead-of-Time`
//! It is a really quick compiler build to speed up like crazy

use std::io::{BufReader, ErrorKind, Read, Seek};

use sart::ctr::{Instruction, *};

use crate::{BytecodeResolver, sync::*};

pub mod asyncmp;

pub fn sync_compile<T: BytecodeResolver + Send + Sync + 'static>(
  resolver: &T,
  module: u32,
  region: u32,
) -> Box<[Instruction]> {
  let mut bytecode = resolver.resolve_bytecode_exact(module, region);

  // Lets start with a good enough instruction size
  // Optimistic, 32-kb space
  let length = bytecode.stream_len().expect("ERROR") as usize;

  let mut code = Vec::with_capacity(length);

  let mut reader = BufReader::new(bytecode);

  let mut index = 0usize;
  let mut byte = [0u8];

  loop {
    if let Err(e) = reader.read_exact(&mut byte) {
      match e.kind() {
        ErrorKind::Interrupted => continue,
        ErrorKind::UnexpectedEof => break,
        _ => panic!("{}", e),
      }
    }

    unsafe {
      // Unsafe for speed
      match *byte.get_unchecked(0) {
        INSTRUCTION_CLR => {
          let mut register = [0u8; 1];

          reader.read_exact(&mut register).expect("Error");
          index += 1;

          let [register] = register;

          code.push(Instruction {
            fn_: (register as u64, inst_clr),
          });
        }
        INSTRUCTION_CLRS => {
          code.push(Instruction {
            fn_: (0, inst_clr_full),
          });
        }
        INSTRUCTION_LIBCALL => {
          code.push(Instruction {
            fn_: (0, inst_sync_libcall::<T>),
          });
        }
        e => panic!("Unexpected {e} at {}", index + 1),
      }
    }

    index += 1;
  }

  // Discards extra space & closes
  code.into_boxed_slice()
}
