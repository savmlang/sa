use std::io::{BufReader, ErrorKind, Read};

use ahash::HashMap;
use sart::ctr::*;

use crate::{BytecodeResolver, acaot::FirstPassInstruction, pack_u64, sync::*, unpack_u64};

pub struct SyncCompiler<'a, F: BytecodeResolver + Send + Sync + 'static> {
  pub code: Vec<FirstPassInstruction>,
  pub resolver: &'a F,
  pub reader: BufReader<F::Output>,
  pub markers: HashMap<u128, usize>,
  pub depth: usize,
  // How much to add to the provided Vec<FirstPassInstruction> to calculate index (simple)
  pub to_add_to_vec_len: usize,
  pub module: u64,
}

macro_rules! cmphandler {
  (
    $t:ident, $ra:ident, $rb:ident
    compute {
      $(
        action: $f:ident,
        id: $e:expr,
        involves: {
          $(
            ($a:ident { $a1:expr } and $b:ident { $a2:expr })
          ),*
        }
      ),*
    }
  ) => {
    pastey::paste! {
      match ($t, $ra, $rb) {
        $($(
          ($e, $a1, $a2) => {
            Instruction {
              fn_: (0, [<inst_cmp_ $f _ $a _ $b>])
            }
          }
        )*)*
        _ => unreachable!(),
      }
    }
  };
}

macro_rules! cmpptrhandler {
  (
    $t:ident, $ra:ident, $rb:ident
    compute {
      $(
        action: $f:ident,
        id: $e:expr,
        involves: {
          $(
            ($a:ident { $a1:expr } and $b:ident { $a2:expr })
          ),*
        }
      ),*
    }
  ) => {
    pastey::paste! {
      match ($t, $ra, $rb) {
        $($(
          ($e, $a1, $a2) => {
            Instruction {
              fn_: (0, [<inst_cmp_ $f _ $a _ $b _ptr>])
            }
          }
        )*)*
        _ => unreachable!(),
      }
    }
  };
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
        FirstPassInstruction::Jnz { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jnz_data),
          }
        }
        FirstPassInstruction::JnzP { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jnz_ptr),
          }
        }
        FirstPassInstruction::JnzR6 { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jnz_r6),
          }
        }
        FirstPassInstruction::Jz { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jz_data),
          }
        }
        FirstPassInstruction::JzP { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jz_ptr),
          }
        }
        FirstPassInstruction::JzR6 { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jz_r6),
          }
        }
        FirstPassInstruction::JzR6U { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jz_r6_unsure),
          }
        }
        FirstPassInstruction::JnzR6U { marker } => {
          let target_index = *markers
            .get(&marker)
            .expect("ACAoT Linker Error: Undefined JMP target marker!");

          Instruction {
            fn_: (target_index as u64, inst_jnz_r6_unsure),
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

        // Register Control
        INSTRUCTION_CLR => self.handle_clr(code),
        INSTRUCTION_CLRS => self.handle_clrs(code),

        // Heap Control
        INSTRUCTION_ALLOC => self.handle_alloc(false, code),
        INSTRUCTION_SUPER_ALLOC => self.handle_alloc(true, code),
        INSTRUCTION_LOAD => self.handle_load(false, code),
        INSTRUCTION_SUPER_LOAD => self.handle_load(true, code),
        INSTRUCTION_FREE => self.handle_free(false, code),
        INSTRUCTION_SUPER_FREE => self.handle_free(true, code),
        INSTRUCTION_OWN => self.handle_own(false, code),
        INSTRUCTION_SUPER_OWN => self.handle_own(true, code),

        // Compare
        INSTRUCTION_CMP => self.handle_compare(code),
        INSTRUCTION_CMP_PTR => self.handle_compare_ptr(code),

        // Arithmatic
        INSTRUCTION_ADD => self.handle_binary_op(inst_add, code),
        INSTRUCTION_SUB => self.handle_binary_op(inst_sub, code),
        INSTRUCTION_MUL => self.handle_binary_op(inst_mul, code),
        INSTRUCTION_DIV => self.handle_binary_op(inst_div, code),
        INSTRUCTION_REM => self.handle_binary_op(inst_rem, code),
        INSTRUCTION_ADD_MUT => self.handle_binary_op(inst_add_mut, code),
        INSTRUCTION_SUB_MUT => self.handle_binary_op(inst_sub_mut, code),
        INSTRUCTION_MUL_MUT => self.handle_binary_op(inst_mul_mut, code),
        INSTRUCTION_DIV_MUT => self.handle_binary_op(inst_div_mut, code),
        INSTRUCTION_REM_MUT => self.handle_binary_op(inst_rem_mut, code),
        INSTRUCTION_ADD_PTR => self.handle_binary_op(inst_add_ptr, code),
        INSTRUCTION_SUB_PTR => self.handle_binary_op(inst_sub_ptr, code),
        INSTRUCTION_MUL_PTR => self.handle_binary_op(inst_mul_ptr, code),
        INSTRUCTION_DIV_PTR => self.handle_binary_op(inst_div_ptr, code),
        INSTRUCTION_REM_PTR => self.handle_binary_op(inst_rem_ptr, code),

        // Shifting
        INSTRUCTION_SHL => self.handle_binary_op(inst_shl, code),
        INSTRUCTION_SHR => self.handle_binary_op(inst_shr, code),
        INSTRUCTION_SHL_PTR => self.handle_binary_op(inst_shl_ptr, code),
        INSTRUCTION_SHR_PTR => self.handle_binary_op(inst_shr_ptr, code),
        INSTRUCTION_SHL_MUT => self.handle_binary_op(inst_shl_mut, code),
        INSTRUCTION_SHR_MUT => self.handle_binary_op(inst_shr_mut, code),

        // Bitwise
        INSTRUCTION_AND => self.handle_binary_op(inst_and, code),
        INSTRUCTION_AND_PTR => self.handle_binary_op(inst_and_ptr, code),
        INSTRUCTION_AND_MUT => self.handle_binary_op(inst_and_mut, code),
        INSTRUCTION_OR => self.handle_binary_op(inst_or, code),
        INSTRUCTION_OR_PTR => self.handle_binary_op(inst_or_ptr, code),
        INSTRUCTION_OR_MUT => self.handle_binary_op(inst_or_mut, code),
        INSTRUCTION_XOR => self.handle_binary_op(inst_xor, code),
        INSTRUCTION_XOR_PTR => self.handle_binary_op(inst_xor_ptr, code),
        INSTRUCTION_XOR_MUT => self.handle_binary_op(inst_xor_mut, code),

        // Library
        INSTRUCTION_LIBCALL => self.handle_libcall(code),

        // Noop
        e => panic!("Unexpected {e}"),
      }
    }
  }

  unsafe fn handle_free(&mut self, use_super_: bool, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [addr] = register;

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (
        addr as _,
        if use_super_ {
          inst_free_super::<F>
        } else {
          inst_free::<F>
        },
      ),
    }));
  }

  unsafe fn handle_own(&mut self, use_super_: bool, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [addr] = register;

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (
        addr as _,
        if use_super_ {
          inst_own_super::<F>
        } else {
          inst_own::<F>
        },
      ),
    }));
  }

  unsafe fn handle_load(&mut self, use_super_: bool, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 2];
    self.reader.read_exact(&mut register).expect("Error");

    let [register, addr] = register;

    let inst = match (use_super_, register) {
      (false, 1) => inst_load_to_r1::<F>,
      (true, 1) => inst_load_to_r1_super::<F>,
      (false, 2) => inst_load_to_r2::<F>,
      (true, 2) => inst_load_to_r2_super::<F>,
      (false, 3) => inst_load_to_r3::<F>,
      (true, 3) => inst_load_to_r3_super::<F>,
      (false, 4) => inst_load_to_r4::<F>,
      (true, 4) => inst_load_to_r4_super::<F>,
      (false, 5) => inst_load_to_r5::<F>,
      (true, 5) => inst_load_to_r5_super::<F>,
      _ => unreachable!(),
    };

    code.push(FirstPassInstruction::Inst(Instruction {
      fn_: (addr as _, inst),
    }));
  }

  unsafe fn handle_compare(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 3];
    self.reader.read_exact(&mut register).expect("Error");

    let [ty, ra, rb] = register;

    let instruction = cmphandler! {
      ty, ra, rb
      compute {
        action: eq,
        id: 0,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: ne,
        id: 1,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: gt,
        id: 2,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: lt,
        id: 3,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: ge,
        id: 4,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: le,
        id: 5,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        }
      }
    };

    code.push(FirstPassInstruction::Inst(instruction));
  }

  unsafe fn handle_compare_ptr(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 3];
    self.reader.read_exact(&mut register).expect("Error");

    let [ty, ra, rb] = register;

    let instruction = cmpptrhandler! {
      ty, ra, rb
      compute {
        action: eq,
        id: 0,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: ne,
        id: 1,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: gt,
        id: 2,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: lt,
        id: 3,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: ge,
        id: 4,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        },
        action: le,
        id: 5,
        involves: {
          (r1 { 1 } and r2 { 2 }),
          (r2 { 2 } and r1 { 1 }),
          (r2 { 2 } and r3 { 3 }),
          (r3 { 3 } and r2 { 2 })
        }
      }
    };

    code.push(FirstPassInstruction::Inst(instruction));
  }

  unsafe fn handle_alloc(&mut self, use_super_: bool, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");

    let [address] = register;
    if use_super_ {
      code.push(FirstPassInstruction::Inst(Instruction {
        fn_: (address as _, inst_alloc_super::<F>),
      }));
    } else {
      code.push(FirstPassInstruction::Inst(Instruction {
        fn_: (address as _, inst_alloc::<F>),
      }));
    }
  }

  unsafe fn handle_mark(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let reg = u64::from_be_bytes(register);

    let key = pack_u64(reg, self.module);

    self
      .markers
      // `-1` because the count is eagerly incremented after jump
      .insert(
        key,
        (code.len() + self.to_add_to_vec_len)
          .checked_sub(1)
          .expect("Top level marks are not allowed as program counter is initially zero. You might want to add a `noop` instruction"),
      );
  }

  unsafe fn handle_jmp(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let key = pack_u64(u64::from_be_bytes(register), self.module);

    code.push(FirstPassInstruction::Jmp { marker: key })
  }

  unsafe fn handle_jz(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let key = pack_u64(u64::from_be_bytes(register), self.module);

    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");
    let [register] = register;

    match register {
      0 => code.push(FirstPassInstruction::Jz { marker: key }),
      1 => code.push(FirstPassInstruction::JzP { marker: key }),
      6 => code.push(FirstPassInstruction::JzR6 { marker: key }),
      7 => code.push(FirstPassInstruction::JzR6U { marker: key }),
      _ => unreachable!(),
    }
  }

  unsafe fn handle_jnz(&mut self, code: &mut Vec<FirstPassInstruction>) {
    let mut register = [0u8; 8];
    self.reader.read_exact(&mut register).expect("Error");

    let key = pack_u64(u64::from_be_bytes(register), self.module);

    let mut register = [0u8; 1];
    self.reader.read_exact(&mut register).expect("Error");
    let [register] = register;

    match register {
      0 => code.push(FirstPassInstruction::Jnz { marker: key }),
      1 => code.push(FirstPassInstruction::JnzP { marker: key }),
      6 => code.push(FirstPassInstruction::JnzR6 { marker: key }),
      7 => code.push(FirstPassInstruction::JnzR6U { marker: key }),
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
      7 => inst_clr_r6_unsure,
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
    let id = u64::from_be_bytes(register);
    let (modid, region) = unpack_u64(id);

    if let Some(bytecode) = self.resolver.resolve_bytecode_exact(modid, region) {
      self.depth += 1;

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
