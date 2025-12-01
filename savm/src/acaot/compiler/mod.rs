use std::io::{BufReader, ErrorKind, Read};

use ahash::HashMap;
use sart::ctr::*;

use crate::{BytecodeResolver, acaot::FirstPassInstruction, sync::*, unpack_u64};

pub struct SyncCompiler<'a, F: BytecodeResolver + Send + Sync + 'static> {
  pub code: Vec<FirstPassInstruction>,
  pub resolver: &'a F,
  pub reader: BufReader<F::Output>,
  pub markers: HashMap<u128, usize>,
  pub depth: usize,
  pub index: usize,
  pub module: CurrentModuleInfo,
}

pub struct CurrentModuleInfo {
  pub id: u64,
  pub instadded: usize,
}

impl<'a, F: BytecodeResolver + Send + Sync + 'static> SyncCompiler<'a, F> {
  pub fn first_pass(&mut self) {
    let mut byte = [0u8];

    loop {
      if let Err(e) = self.reader.read_exact(&mut byte) {
        match e.kind() {
          ErrorKind::Interrupted => continue,
          ErrorKind::UnexpectedEof => break,
          _ => panic!("{}", e),
        }
      }

      unsafe {
        let opcode = *byte.get_unchecked(0);
        self.index += self.handle_instruction(opcode).0;
      }

      self.index += 1;
    }
  }

  pub fn second_pass(self) -> Box<[Instruction]> {
    let SyncCompiler { markers, code, .. } = self;

    code
      .into_iter()
      .map(|x| match x {
        FirstPassInstruction::Inst(x) => x,
        FirstPassInstruction::Jmp { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");
          Instruction {
            fn_: (target_index as u64, inst_jmp),
          }
        }
      })
      .collect::<Box<[Instruction]>>()
  }

  unsafe fn handle_instruction(&mut self, opcode: u8) -> (usize, usize) {
    unsafe {
      match opcode {
        INSTRUCTION_CLR => self.handle_clr(),
        INSTRUCTION_CLRS => self.handle_clrs(),
        INSTRUCTION_ADD => self.handle_binary_op(inst_add),
        INSTRUCTION_SUB => self.handle_binary_op(inst_sub),
        INSTRUCTION_MUL => self.handle_binary_op(inst_mul),
        INSTRUCTION_DIV => self.handle_binary_op(inst_div),
        INSTRUCTION_REM => self.handle_binary_op(inst_rem),
        INSTRUCTION_SHL => self.handle_binary_op(inst_shl),
        INSTRUCTION_SHR => self.handle_binary_op(inst_shr),
        INSTRUCTION_LIBCALL => self.handle_libcall(),
        e => panic!("Unexpected {e}"),
      }
    }
  }

  unsafe fn handle_clr(&mut self) -> (usize, usize) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");
    let [register] = register;
    self.code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (register as u64, inst_clr),
    }));

    (1, 1)
  }

  unsafe fn handle_clrs(&mut self) -> (usize, usize) {
    self.code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (0, inst_clr_full),
    }));

    (0, 1)
  }

  unsafe fn handle_binary_op(&mut self, op: DispatchFn) -> (usize, usize) {
    self
      .code
      .push(FirstPassInstruction::Inst(Instruction { fn_: (0, op) }));

    (0, 1)
  }

  unsafe fn handle_libcall(&mut self) -> (usize, usize) {
    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");
    let id = u64::from_be_bytes(register);
    let (modid, region) = unpack_u64(id);

    if let Some(_bytecode) = self.resolver.resolve_bytecode_exact(modid, region) {
      self.code.push(FirstPassInstruction::Inst(Instruction {
        fn_: (id, inst_sync_libcall::<F>),
      }));
      // self.depth += 1;

      // unsafe { self.inline_bytecode(bytecode) };

      // self.depth -= 1;
    } else {
      self.code.push(FirstPassInstruction::Inst(Instruction {
        fn_: (0, self.resolver.resolve_native(modid, region)),
      }));
    }

    (8, 1)
  }

  unsafe fn _inline_bytecode(&mut self, bytecode: F::Output) {
    let original_reader = std::mem::replace(&mut self.reader, BufReader::new(bytecode));

    let mut byte = [0u8];
    loop {
      if let Err(e) = self.reader.read_exact(&mut byte) {
        match e.kind() {
          ErrorKind::Interrupted => continue,
          ErrorKind::UnexpectedEof => break,
          _ => panic!("{}", e),
        }
      }

      unsafe {
        let opcode = *byte.get_unchecked(0);
        self.handle_instruction(opcode);
      }
    }

    self.reader = original_reader;
  }
}
