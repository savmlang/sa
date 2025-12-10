use std::io::{BufReader, ErrorKind, Read};

use ahash::HashMap;
use sart::ctr::*;

use crate::{BytecodeResolver, acaot::FirstPassInstruction, pack_u64, sync::*, unpack_u64};

pub struct SyncCompiler<'a, F: BytecodeResolver + Send + Sync + 'static> {
  pub code: Vec<FirstPassInstruction>,
  pub resolver: &'a F,
  pub reader: BufReader<F::Output>,
  pub markers: HashMap<X2U128, usize>,
  // <instance id> - <counter of the instance>
  pub instance_counter: HashMap<u64, u128>,
  pub depth: usize,
  // How much to add to the provided Vec<FirstPassInstruction> to calculate index (simple)
  pub to_add_to_vec_len: usize,
  pub module: u64,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct X2U128 {
  u128_1: u128,
  u128_2: u128,
}

impl<'a, F: BytecodeResolver + Send + Sync + 'static> SyncCompiler<'a, F> {
  pub fn first_pass(&mut self) {
    let mut byte = [0u8];

    let mut code = vec![];

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
        self.handle_instruction(opcode, &mut code);
      }
    }

    self.code = code;
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
        FirstPassInstruction::JumpCond { marker, inst } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst),
          }
        }
      })
      .collect::<Box<[Instruction]>>()
  }

  unsafe fn handle_instruction(&mut self, opcode: u8, code: &mut Vec<FirstPassInstruction>) {
    unsafe {
      match opcode {
        // Control Block
        INSTRUCTION_MARK => self.handle_mark(code),
        INSTRUCTION_JMP => self.handle_jmp(code),
        INSTRUCTION_JZ => self.handle_jz(code),
        INSTRUCTION_JNZ => self.handle_jnz(code),
        INSTRUCTION_YIELD => self.handle_binary_op(inst_yield, code),
        INSTRUCTION_AWAIT => self.handle_binary_op(inst_sync_await, code),
        INSTRUCTION_SPAWN => self.handle_spawn(code),

        // Register Control
        INSTRUCTION_CLR => self.handle_clr(code),
        INSTRUCTION_CLRS => self.handle_clrs(code),

        // Heap Control
        INSTRUCTION_ALLOC => self.handle_alloc(code),
        INSTRUCTION_LOAD => self.handle_load(code),
        INSTRUCTION_FREE => self.handle_free(code),
        INSTRUCTION_OWN => self.handle_own(code),
        INSTRUCTION_MOV => self.handle_mov(false, code),
        INSTRUCTION_SUPER_MOV => self.handle_mov(true, code),
        INSTRUCTION_MOV_TO_SUPER => self.handle_mov_to(code),

        // Compare
        INSTRUCTION_CMP => self.handle_compare(code),

        // Arithmatic
        INSTRUCTION_ADD => self.handle_alu(ADDOPS, code),
        INSTRUCTION_SUB => self.handle_alu(SUBOPS, code),
        INSTRUCTION_MUL => self.handle_alu(MULOPS, code),
        INSTRUCTION_DIV => self.handle_alu(DIVOPS, code),
        INSTRUCTION_REM => self.handle_alu(REMOPS, code),
        INSTRUCTION_ADD_MUT => self.handle_alu(ADDMUTOPS, code),
        INSTRUCTION_SUB_MUT => self.handle_alu(SUBMUTOPS, code),
        INSTRUCTION_MUL_MUT => self.handle_alu(MULMUTOPS, code),
        INSTRUCTION_DIV_MUT => self.handle_alu(DIVMUTOPS, code),
        INSTRUCTION_REM_MUT => self.handle_alu(REMMUTOPS, code),

        // Pointer Arithmatic
        INSTRUCTION_ADD_PTR => self.handle_ptraith(PTRADDOP, code),
        INSTRUCTION_SUB_PTR => self.handle_ptraith(PTRSUBOP, code),
        INSTRUCTION_OFFSET_PTR => self.handle_ptraith(PTROFFSETOP, code),

        // Shifting
        INSTRUCTION_SHL => self.handle_alu(SHROPS, code),
        INSTRUCTION_SHR => self.handle_alu(SHLOPS, code),
        INSTRUCTION_SHL_MUT => self.handle_alu(SHLMUTOPS, code),
        INSTRUCTION_SHR_MUT => self.handle_alu(SHRMUTOPS, code),

        // Bitwise
        INSTRUCTION_AND => self.handle_bitwise(ANDOPS, code),
        INSTRUCTION_AND_MUT => self.handle_bitwise(ANDMUTOPS, code),
        INSTRUCTION_OR => self.handle_bitwise(OROPS, code),
        INSTRUCTION_OR_MUT => self.handle_bitwise(ORMUTOPS, code),
        INSTRUCTION_XOR => self.handle_bitwise(XOROPS, code),
        INSTRUCTION_XOR_MUT => self.handle_bitwise(XORMUTOPS, code),

        // Library
        INSTRUCTION_LIBCALL => self.handle_libcall(code),

        // Noop
        e => panic!("Unexpected {e}"),
      }
    }
  }

  unsafe fn handle_alu(&mut self, op: u8, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [typ] = register;

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (0, inst_arithmetic_handler(typ, op)),
    }))
  }

  unsafe fn handle_ptraith(&mut self, op: u8, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [typ] = register;

    let mut data = [0u8; 8];
    self.reader.read_exact(&mut data).expect("Error");

    let data = if op == PTROFFSETOP {
      let data1 = i64::from_le_bytes(data);

      isize_to_u64(data1.try_into().expect("Unable to convert to isize"))
    } else {
      u64::from_le_bytes(data)
    };

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (data, inst_ptrarith_handler(typ, op)),
    }))
  }

  unsafe fn handle_bitwise(&mut self, op: u8, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [typ] = register;

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (0, inst_bitwise_handler(typ, op)),
    }))
  }

  unsafe fn handle_free(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [register] = register;

    if register >= 7 {
      let mut addr = [0u8; 2];
      self.reader.read_exact(&mut addr).expect("Error");

      let addr = u16::from_le_bytes(addr);

      code.push(FirstPassInstruction::Inst(Instruction {
        fn_: (addr as _, inst_free_addr),
      }));
      return;
    }

    let inst = match register {
      1 => inst_free_r1,
      2 => inst_free_r2,
      3 => inst_free_r3,
      4 => inst_free_r4,
      5 => inst_free_r5,
      6 => inst_free_r6,
      _ => unreachable!(),
    };

    code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, inst) }));
  }

  unsafe fn handle_mov_to(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 2];
    self.reader.read_exact(&mut register).expect("Error");

    let [from, to] = register;

    let inst = generate_to_mov_super(from, to);

    code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, inst) }));
  }

  unsafe fn handle_mov(&mut self, super_: bool, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 2];
    self.reader.read_exact(&mut register).expect("Error");

    let [from, to] = register;

    let inst = if super_ {
      generate_mov_super(from, to)
    } else {
      generate_mov(from, to)
    };

    code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, inst) }));
  }

  unsafe fn handle_own(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [register] = register;

    let inst = match register {
      1 => inst_own_r1,
      2 => inst_own_r2,
      3 => inst_own_r3,
      4 => inst_own_r4,
      5 => inst_own_r5,
      6 => inst_own_r6,
      _ => unreachable!(),
    };

    let mut addr = [0u8; 2];
    self.reader.read_exact(&mut addr).expect("Error");

    let addr = u16::from_le_bytes(addr);

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (addr as _, inst),
    }));
  }

  unsafe fn handle_load(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 2];
    self.reader.read_exact(&mut register).expect("Error");

    let [register, addr] = register;

    let inst = match register {
      1 => inst_load_to_r1::<F>,
      2 => inst_load_to_r2::<F>,
      3 => inst_load_to_r3::<F>,
      4 => inst_load_to_r4::<F>,
      5 => inst_load_to_r5::<F>,
      _ => unreachable!(),
    };

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (addr as _, inst),
    }));
  }

  unsafe fn handle_compare(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 4];
    self.reader.read_exact(&mut register).expect("Error");

    let [ty, op, ra, rb] = register;

    let instruction = inst_cmp_handler(ty, op, ra, rb);

    code.push(FirstPassInstruction::Inst(Instruction { fn_: instruction }));
  }

  unsafe fn handle_alloc(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [address] = register;

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (address as _, inst_alloc::<F>),
    }));
  }

  unsafe fn handle_mark(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let instid = self
      .instance_counter
      .get(&self.module)
      .map(|x| *x)
      .unwrap_or_else(|| 1);

    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let reg = u64::from_le_bytes(register);

    let key = pack_u64(reg, self.module);

    self
      .markers
      // `-1` because the count is eagerly incremented after jump
      .insert(
        X2U128 { u128_1: key, u128_2: instid },
        (code.len() + self.to_add_to_vec_len)
          .checked_sub(1)
          .expect("Top level marks are not allowed as program counter is initially zero. You might want to add a `noop` instruction"),
      );
  }

  unsafe fn handle_spawn(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut module = [0u8; 8];
    self.reader.read_exact(&mut module).expect("Error");

    let addr = u64::from_le_bytes(module);

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (addr, inst_sync_spawn::<F>),
    }));
  }

  unsafe fn handle_jmp(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let instid = self
      .instance_counter
      .get(&self.module)
      .map(|x| *x)
      .unwrap_or_else(|| 1);

    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let key = pack_u64(u64::from_le_bytes(register), self.module);

    code.push(FirstPassInstruction::Jmp {
      marker: X2U128 {
        u128_1: key,
        u128_2: instid,
      },
    })
  }

  unsafe fn handle_jz(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let instid = self
      .instance_counter
      .get(&self.module)
      .map(|x| *x)
      .unwrap_or_else(|| 1);

    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let key = pack_u64(u64::from_le_bytes(register), self.module);

    let mut register = [0u8; 2];
    self.reader.read_exact(&mut register).expect("Error");
    let [register, datatype] = register;

    match register {
      0 => code.push(FirstPassInstruction::JumpCond {
        marker: X2U128 {
          u128_1: key,
          u128_2: instid,
        },
        inst: jz_map(datatype),
      }),
      1 => code.push(FirstPassInstruction::JumpCond {
        marker: X2U128 {
          u128_1: key,
          u128_2: instid,
        },
        inst: jz_ptr_map(datatype),
      }),
      _ => unreachable!(),
    }
  }

  unsafe fn handle_jnz(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let instid = self
      .instance_counter
      .get(&self.module)
      .map(|x| *x)
      .unwrap_or_else(|| 1);

    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let key = pack_u64(u64::from_le_bytes(register), self.module);

    let mut register = [0u8; 2];
    self.reader.read_exact(&mut register).expect("Error");
    let [register, datatype] = register;

    match register {
      0 => code.push(FirstPassInstruction::JumpCond {
        marker: X2U128 {
          u128_1: key,
          u128_2: instid,
        },
        inst: jnz_map(datatype),
      }),
      1 => code.push(FirstPassInstruction::JumpCond {
        marker: X2U128 {
          u128_1: key,
          u128_2: instid,
        },
        inst: jnz_ptr_map(datatype),
      }),
      _ => unreachable!(),
    }
  }

  unsafe fn handle_clr(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");
    let [register] = register;

    let f = match register {
      1 => inst_clr_r1,
      2 => inst_clr_r2,
      3 => inst_clr_r3,
      4 => inst_clr_r4,
      5 => inst_clr_r5,
      6 => inst_clr_r6,
      _ => unreachable!(),
    };

    code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, f) }));
  }

  unsafe fn handle_clrs(&mut self, code: &mut Vec<FirstPassInstruction>) {
    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (0, inst_clr_full),
    }));
  }

  unsafe fn handle_binary_op(&mut self, op: DispatchFn, code: &mut Vec<FirstPassInstruction>) {
    code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, op) }));
  }

  unsafe fn handle_libcall(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");
    let id = u64::from_le_bytes(register);
    let (modid, region) = unpack_u64(id);

    if let Some(bytecode) = self.resolver.resolve_bytecode_exact(modid, region) {
      self.depth += 1;

      self
        .instance_counter
        .entry(id)
        .and_modify(|x| *x += 1)
        .or_insert(1);

      let old = self.module;

      self.module = id;
      unsafe { self.inline_bytecode(id, bytecode, code) };

      self.module = old;

      self.depth -= 1;
    } else {
      code.push(FirstPassInstruction::Inst(Instruction {
        fn_: (0, self.resolver.resolve_native(modid, region)),
      }));
    }
  }

  unsafe fn inline_bytecode(
    &mut self,
    module: u64,
    bytecode: F::Output,
    code: &mut Vec<FirstPassInstruction>,
  ) {
    let original_reader = std::mem::replace(&mut self.reader, BufReader::new(bytecode));

    let mut byte = [0u8];

    let mut tmp = vec![FirstPassInstruction::Inst(Instruction {
      fn_: (0, new_context::<F>),
    })];

    tmp.reserve(200);

    let orig = self.to_add_to_vec_len;

    self.to_add_to_vec_len = orig + code.len();

    loop {
      // Too much of instruction complexity
      if tmp.len().saturating_mul(self.depth).saturating_mul(2) > 360 {
        self.to_add_to_vec_len = orig;
        code.push(FirstPassInstruction::Inst(Instruction {
          fn_: (module, inst_sync_libcall::<F>),
        }));
        return;
      }

      if let Err(e) = self.reader.read_exact(&mut byte) {
        match e.kind() {
          ErrorKind::Interrupted => continue,
          ErrorKind::UnexpectedEof => break,
          _ => panic!("{}", e),
        }
      }

      unsafe {
        let opcode = *byte.get_unchecked(0);
        self.handle_instruction(opcode, &mut tmp);
      }
    }

    tmp.push(FirstPassInstruction::Inst(Instruction {
      fn_: (0, restore_context::<F>),
    }));

    code.extend(tmp.into_iter());

    self.to_add_to_vec_len = orig;

    self.reader = original_reader;
  }
}
