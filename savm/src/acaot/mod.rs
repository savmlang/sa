//! This is the ACAoT compiler
//! `All Compiled Ahead-of-Time`
//! It is a really quick compiler build to speed up like crazy

use crate::{BytecodeResolver, sync::*, unpack_u64};
use sart::ctr::{Instruction, *};
use std::io::{BufReader, ErrorKind, Read, Seek};

pub mod asyncmp;

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
        INSTRUCTION_ADD => {
          code.push(Instruction { fn_: (0, inst_add) });
        }
        INSTRUCTION_SUB => {
          code.push(Instruction { fn_: (0, inst_sub) });
        }
        INSTRUCTION_MUL => {
          code.push(Instruction { fn_: (0, inst_mul) });
        }
        INSTRUCTION_DIV => {
          code.push(Instruction { fn_: (0, inst_div) });
        }
        INSTRUCTION_REM => {
          code.push(Instruction { fn_: (0, inst_rem) });
        }
        INSTRUCTION_SHL => {
          code.push(Instruction { fn_: (0, inst_shl) });
        }
        INSTRUCTION_SHR => {
          code.push(Instruction { fn_: (0, inst_shr) });
        }
        INSTRUCTION_LIBCALL => {
          let mut register = [0u8; 8];

          reader.read_exact(&mut register).expect("Error");
          index += 8;

          let id = u64::from_be_bytes(register);
          let (modid, region) = unpack_u64(id);

          if let Some(_) = resolver.resolve_bytecode_exact(modid, region) {
            code.push(Instruction {
              fn_: (id, inst_sync_libcall::<T>),
            });
          } else {
            code.push(Instruction {
              fn_: (0, resolver.resolve_native(modid, region)),
            });
          }
        }
        e => panic!("Unexpected {e} at {}", index + 1),
      }
    }

    index += 1;
  }

  // Discards extra space & closes
  code.into_boxed_slice()
}
