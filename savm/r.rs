#![feature(prelude_import)]
#![feature(
    seek_stream_len,
    portable_simd,
    unchecked_shifts,
    exact_div,
    int_roundings,
    nonpoison_rwlock,
    sync_nonpoison
)]
#[macro_use]
extern crate std;
#[prelude_import]
use std::prelude::rust_2024::*;
pub mod acaot {
    //! This is the ACAoT compiler
    //! `Aggressive Cycle-Adaptive Ordered Threading`` Compiler
    //! It is a really quick compiler build to speed up like crazy
    use crate::{BytecodeResolver, acaot::compiler::SyncCompiler};
    use sart::ctr::{DispatchFn, Instruction};
    use std::{collections::HashMap, io::{BufReader, Seek}};
    pub mod asyncmp {
        use std::sync::Arc;
        use sart::ctr::Instruction;
        use tokio::task::spawn_blocking;
        use crate::{BytecodeResolver, acaot::sync_compile};
        pub async fn async_compile<T: BytecodeResolver + Send + Sync + 'static>(
            resolver: Arc<T>,
            module: u32,
            region: u32,
        ) -> Box<[Instruction]> {
            spawn_blocking(move || {
                    let resolver = resolver;
                    sync_compile(resolver.as_ref(), module, region)
                })
                .await
                .expect("Compile error")
        }
    }
    pub mod compiler {
        use std::io::{BufReader, ErrorKind, Read};
        use ahash::HashMap;
        use sart::ctr::*;
        use crate::{
            BytecodeResolver, acaot::FirstPassInstruction, pack_u64, sync::*, unpack_u64,
        };
        pub struct SyncCompiler<'a, F: BytecodeResolver + Send + Sync + 'static> {
            pub code: Vec<FirstPassInstruction>,
            pub resolver: &'a F,
            pub reader: BufReader<F::Output>,
            pub markers: HashMap<u128, usize>,
            pub depth: usize,
            pub to_add_to_vec_len: usize,
            pub module: u64,
        }
        impl<'a, F: BytecodeResolver + Send + Sync + 'static> SyncCompiler<'a, F> {
            pub fn first_pass(&mut self) {
                let mut byte = [0u8];
                let mut code = ::alloc::vec::Vec::new();
                loop {
                    if let Err(e) = self.reader.read_exact(&mut byte) {
                        match e.kind() {
                            ErrorKind::Interrupted => continue,
                            ErrorKind::UnexpectedEof => break,
                            _ => {
                                ::core::panicking::panic_display(&e);
                            }
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
                code.into_iter()
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
            unsafe fn handle_instruction(
                &mut self,
                opcode: u8,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                unsafe {
                    match opcode {
                        INSTRUCTION_MARK => self.handle_mark(code),
                        INSTRUCTION_JMP => self.handle_jmp(code),
                        INSTRUCTION_JZ => self.handle_jz(code),
                        INSTRUCTION_JNZ => self.handle_jnz(code),
                        INSTRUCTION_YIELD => self.handle_binary_op(inst_yield, code),
                        INSTRUCTION_AWAIT => self.handle_binary_op(inst_sync_await, code),
                        INSTRUCTION_SPAWN => self.handle_spawn(code),
                        INSTRUCTION_CLR => self.handle_clr(code),
                        INSTRUCTION_CLRS => self.handle_clrs(code),
                        INSTRUCTION_ALLOC => self.handle_alloc(false, code),
                        INSTRUCTION_SUPER_ALLOC => self.handle_alloc(true, code),
                        INSTRUCTION_LOAD => self.handle_load(false, code),
                        INSTRUCTION_SUPER_LOAD => self.handle_load(true, code),
                        INSTRUCTION_FREE => self.handle_free(false, code),
                        INSTRUCTION_SUPER_FREE => self.handle_free(true, code),
                        INSTRUCTION_OWN => self.handle_own(false, code),
                        INSTRUCTION_SUPER_OWN => self.handle_own(true, code),
                        INSTRUCTION_CMP => self.handle_compare(code),
                        INSTRUCTION_CMP_PTR => self.handle_compare_ptr(code),
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
                        INSTRUCTION_SHL => self.handle_binary_op(inst_shl, code),
                        INSTRUCTION_SHR => self.handle_binary_op(inst_shr, code),
                        INSTRUCTION_SHL_PTR => self.handle_binary_op(inst_shl_ptr, code),
                        INSTRUCTION_SHR_PTR => self.handle_binary_op(inst_shr_ptr, code),
                        INSTRUCTION_SHL_MUT => self.handle_binary_op(inst_shl_mut, code),
                        INSTRUCTION_SHR_MUT => self.handle_binary_op(inst_shr_mut, code),
                        INSTRUCTION_AND => self.handle_binary_op(inst_and, code),
                        INSTRUCTION_AND_PTR => self.handle_binary_op(inst_and_ptr, code),
                        INSTRUCTION_AND_MUT => self.handle_binary_op(inst_and_mut, code),
                        INSTRUCTION_OR => self.handle_binary_op(inst_or, code),
                        INSTRUCTION_OR_PTR => self.handle_binary_op(inst_or_ptr, code),
                        INSTRUCTION_OR_MUT => self.handle_binary_op(inst_or_mut, code),
                        INSTRUCTION_XOR => self.handle_binary_op(inst_xor, code),
                        INSTRUCTION_XOR_PTR => self.handle_binary_op(inst_xor_ptr, code),
                        INSTRUCTION_XOR_MUT => self.handle_binary_op(inst_xor_mut, code),
                        INSTRUCTION_LIBCALL => self.handle_libcall(code),
                        e => {
                            ::core::panicking::panic_fmt(
                                format_args!("Unexpected {0}", e),
                            );
                        }
                    }
                }
            }
            unsafe fn handle_free(
                &mut self,
                use_super_: bool,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                let mut register = [0u8; 1];
                self.reader.read_exact(&mut register).expect("Error");
                let [register] = register;
                if register >= 7 {
                    let mut addr = [0u8; 2];
                    self.reader.read_exact(&mut addr).expect("Error");
                    let addr = u16::from_be_bytes(addr);
                    code.push(
                        FirstPassInstruction::Inst(Instruction {
                            fn_: (addr as _, inst_free_addr),
                        }),
                    );
                    return;
                }
                let inst = if use_super_ {
                    match register {
                        1 => inst_free_r1,
                        2 => inst_free_r2,
                        3 => inst_free_r3,
                        4 => inst_free_r4,
                        5 => inst_free_r5,
                        6 => inst_free_r6,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                } else {
                    match register {
                        1 => inst_free_super_r1,
                        2 => inst_free_super_r2,
                        3 => inst_free_super_r3,
                        4 => inst_free_super_r4,
                        5 => inst_free_super_r5,
                        6 => inst_free_super_r6,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                };
                code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, inst) }));
            }
            unsafe fn handle_own(
                &mut self,
                use_super_: bool,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                let mut register = [0u8; 1];
                self.reader.read_exact(&mut register).expect("Error");
                let [register] = register;
                let mut addr = [0u8; 2];
                self.reader.read_exact(&mut addr).expect("Error");
                let addr = u16::from_be_bytes(addr);
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (
                            addr as _,
                            if use_super_ { inst_own_super::<F> } else { inst_own::<F> },
                        ),
                    }),
                );
            }
            unsafe fn handle_load(
                &mut self,
                use_super_: bool,
                code: &mut Vec<FirstPassInstruction>,
            ) {
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
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (addr as _, inst),
                    }),
                );
            }
            unsafe fn handle_compare(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut register = [0u8; 3];
                self.reader.read_exact(&mut register).expect("Error");
                let [ty, ra, rb] = register;
                let instruction = match (ty, ra, rb) {
                    (0, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r1_r2),
                        }
                    }
                    (0, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r2_r1),
                        }
                    }
                    (0, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r2_r3),
                        }
                    }
                    (0, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r3_r2),
                        }
                    }
                    (1, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r1_r2),
                        }
                    }
                    (1, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r2_r1),
                        }
                    }
                    (1, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r2_r3),
                        }
                    }
                    (1, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r3_r2),
                        }
                    }
                    (2, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r1_r2),
                        }
                    }
                    (2, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r2_r1),
                        }
                    }
                    (2, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r2_r3),
                        }
                    }
                    (2, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r3_r2),
                        }
                    }
                    (3, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r1_r2),
                        }
                    }
                    (3, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r2_r1),
                        }
                    }
                    (3, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r2_r3),
                        }
                    }
                    (3, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r3_r2),
                        }
                    }
                    (4, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r1_r2),
                        }
                    }
                    (4, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r2_r1),
                        }
                    }
                    (4, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r2_r3),
                        }
                    }
                    (4, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r3_r2),
                        }
                    }
                    (5, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r1_r2),
                        }
                    }
                    (5, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r2_r1),
                        }
                    }
                    (5, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r2_r3),
                        }
                    }
                    (5, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r3_r2),
                        }
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                code.push(FirstPassInstruction::Inst(instruction));
            }
            unsafe fn handle_compare_ptr(
                &mut self,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                let mut register = [0u8; 3];
                self.reader.read_exact(&mut register).expect("Error");
                let [ty, ra, rb] = register;
                let instruction = match (ty, ra, rb) {
                    (0, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r1_r2_ptr),
                        }
                    }
                    (0, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r2_r1_ptr),
                        }
                    }
                    (0, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r2_r3_ptr),
                        }
                    }
                    (0, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_eq_r3_r2_ptr),
                        }
                    }
                    (1, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r1_r2_ptr),
                        }
                    }
                    (1, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r2_r1_ptr),
                        }
                    }
                    (1, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r2_r3_ptr),
                        }
                    }
                    (1, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ne_r3_r2_ptr),
                        }
                    }
                    (2, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r1_r2_ptr),
                        }
                    }
                    (2, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r2_r1_ptr),
                        }
                    }
                    (2, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r2_r3_ptr),
                        }
                    }
                    (2, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_gt_r3_r2_ptr),
                        }
                    }
                    (3, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r1_r2_ptr),
                        }
                    }
                    (3, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r2_r1_ptr),
                        }
                    }
                    (3, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r2_r3_ptr),
                        }
                    }
                    (3, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_lt_r3_r2_ptr),
                        }
                    }
                    (4, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r1_r2_ptr),
                        }
                    }
                    (4, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r2_r1_ptr),
                        }
                    }
                    (4, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r2_r3_ptr),
                        }
                    }
                    (4, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_ge_r3_r2_ptr),
                        }
                    }
                    (5, 1, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r1_r2_ptr),
                        }
                    }
                    (5, 2, 1) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r2_r1_ptr),
                        }
                    }
                    (5, 2, 3) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r2_r3_ptr),
                        }
                    }
                    (5, 3, 2) => {
                        Instruction {
                            fn_: (0, inst_cmp_le_r3_r2_ptr),
                        }
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                code.push(FirstPassInstruction::Inst(instruction));
            }
            unsafe fn handle_alloc(
                &mut self,
                use_super_: bool,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                let mut register = [0u8; 1];
                self.reader.read_exact(&mut register).expect("Error");
                let [address] = register;
                if use_super_ {
                    code.push(
                        FirstPassInstruction::Inst(Instruction {
                            fn_: (address as _, inst_alloc_super::<F>),
                        }),
                    );
                } else {
                    code.push(
                        FirstPassInstruction::Inst(Instruction {
                            fn_: (address as _, inst_alloc::<F>),
                        }),
                    );
                }
            }
            unsafe fn handle_mark(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut register = [0u8; 8];
                self.reader.read_exact(&mut register).expect("Error");
                let reg = u64::from_be_bytes(register);
                let key = pack_u64(reg, self.module);
                self.markers
                    .insert(
                        key,
                        (code.len() + self.to_add_to_vec_len)
                            .checked_sub(1)
                            .expect(
                                "Top level marks are not allowed as program counter is initially zero. You might want to add a `noop` instruction",
                            ),
                    );
            }
            unsafe fn handle_spawn(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut module = [0u8; 8];
                self.reader.read_exact(&mut module).expect("Error");
                let addr = u64::from_be_bytes(module);
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (addr, inst_sync_spawn::<F>),
                    }),
                );
            }
            unsafe fn handle_jmp(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut register = [0u8; 8];
                self.reader.read_exact(&mut register).expect("Error");
                let key = pack_u64(u64::from_be_bytes(register), self.module);
                code.push(FirstPassInstruction::Jmp {
                    marker: key,
                })
            }
            unsafe fn handle_jz(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut register = [0u8; 8];
                self.reader.read_exact(&mut register).expect("Error");
                let key = pack_u64(u64::from_be_bytes(register), self.module);
                let mut register = [0u8; 1];
                self.reader.read_exact(&mut register).expect("Error");
                let [register] = register;
                match register {
                    0 => {
                        code.push(FirstPassInstruction::Jz {
                            marker: key,
                        })
                    }
                    1 => {
                        code.push(FirstPassInstruction::JzP {
                            marker: key,
                        })
                    }
                    6 => {
                        code.push(FirstPassInstruction::JzR6 {
                            marker: key,
                        })
                    }
                    7 => {
                        code.push(FirstPassInstruction::JzR6U {
                            marker: key,
                        })
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
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
                    0 => {
                        code.push(FirstPassInstruction::Jnz {
                            marker: key,
                        })
                    }
                    1 => {
                        code.push(FirstPassInstruction::JnzP {
                            marker: key,
                        })
                    }
                    6 => {
                        code.push(FirstPassInstruction::JnzR6 {
                            marker: key,
                        })
                    }
                    7 => {
                        code.push(FirstPassInstruction::JnzR6U {
                            marker: key,
                        })
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
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
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, f) }));
            }
            unsafe fn handle_clrs(&mut self, code: &mut Vec<FirstPassInstruction>) {
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (0, inst_clr_full),
                    }),
                );
            }
            unsafe fn handle_binary_op(
                &mut self,
                op: DispatchFn,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                code.push(FirstPassInstruction::Inst(Instruction { fn_: (0, op) }));
            }
            unsafe fn handle_libcall(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut register = [0u8; 8];
                self.reader.read_exact(&mut register).expect("Error");
                let id = u64::from_be_bytes(register);
                let (modid, region) = unpack_u64(id);
                if let Some(bytecode) = self
                    .resolver
                    .resolve_bytecode_exact(modid, region)
                {
                    self.depth += 1;
                    let old = self.module;
                    self.module = id;
                    unsafe { self.inline_bytecode(id, bytecode, code) };
                    self.module = old;
                    self.depth -= 1;
                } else {
                    code.push(
                        FirstPassInstruction::Inst(Instruction {
                            fn_: (0, self.resolver.resolve_native(modid, region)),
                        }),
                    );
                }
            }
            unsafe fn inline_bytecode(
                &mut self,
                module: u64,
                bytecode: F::Output,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                let original_reader = std::mem::replace(
                    &mut self.reader,
                    BufReader::new(bytecode),
                );
                let mut byte = [0u8];
                let mut tmp = <[_]>::into_vec(
                    ::alloc::boxed::box_new([
                        FirstPassInstruction::Inst(Instruction {
                            fn_: (0, new_context::<F>),
                        }),
                    ]),
                );
                tmp.reserve(200);
                let orig = self.to_add_to_vec_len;
                self.to_add_to_vec_len = orig + code.len();
                loop {
                    if tmp.len().saturating_mul(self.depth).saturating_mul(2) > 360 {
                        self.to_add_to_vec_len = orig;
                        code.push(
                            FirstPassInstruction::Inst(Instruction {
                                fn_: (module, inst_sync_libcall::<F>),
                            }),
                        );
                        return;
                    }
                    if let Err(e) = self.reader.read_exact(&mut byte) {
                        match e.kind() {
                            ErrorKind::Interrupted => continue,
                            ErrorKind::UnexpectedEof => break,
                            _ => {
                                ::core::panicking::panic_display(&e);
                            }
                        }
                    }
                    unsafe {
                        let opcode = *byte.get_unchecked(0);
                        self.handle_instruction(opcode, &mut tmp);
                    }
                }
                tmp.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (0, restore_context::<F>),
                    }),
                );
                code.extend(tmp.into_iter());
                self.to_add_to_vec_len = orig;
                self.reader = original_reader;
            }
        }
    }
    pub mod jit {}
    pub enum FirstPassInstruction {
        Inst(Instruction),
        Jmp { marker: u128 },
        JumpCond { marker: u128, inst: DispatchFn },
    }
    pub fn sync_compile<T: BytecodeResolver + Send + Sync + 'static>(
        resolver: &T,
        module: u32,
        region: u32,
    ) -> Box<[Instruction]> {
        let mut bytecode = resolver
            .resolve_bytecode_exact(module, region)
            .expect("This cannot error out");
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
}
use std::{
    fs::File, io::{Read, Seek},
    mem::zeroed, ptr::null_mut,
    sync::{
        Arc, LazyLock, OnceLock, atomic::{AtomicUsize, Ordering},
        mpsc::{Receiver, channel},
        nonpoison::RwLock,
    },
    thread::available_parallelism,
};
use crate::acaot::sync_compile;
use dashmap::DashMap;
use sart::{
    boxed::{RTSafeBoxWrapper, spawn::{SendWrapper, ThreadSpawnContext, send}},
    ctr::{DispatchFn, Instruction},
    map::{CompiledCode, HeapStructure},
};
pub use sart;
use tokio::runtime::{Builder, Runtime};
pub mod executor {
    use std::mem::zeroed;
    use sart::ctr::{Instruction, VMTaskState};
    use crate::{BytecodeResolver, VM};
    impl<T: BytecodeResolver + Send + Sync + 'static> VM<T> {
        /// Please note that from this point onwards we'll purely compute the values
        /// and wont check any single thing
        ///
        /// This strictly runs module id `0` section `0`
        pub unsafe fn run(&mut self) {
            unsafe {
                let mut task = zeroed::<VMTaskState>();
                self.run_module(&mut task, 0)
            };
        }
        pub unsafe fn run_module(&mut self, state: &mut VMTaskState, region: u64) {
            unsafe {
                let module = self.code.get(&region).unwrap_unchecked();
                state.curline = 0;
                let module: &[Instruction] = &module;
                loop {
                    if state.curline == module.len() {
                        break;
                    }
                    let inst = module.get_unchecked(state.curline);
                    let (u, f) = inst.fn_;
                    f(self as *const _ as _, state as _, u);
                    state.curline += 1;
                }
            };
        }
    }
}
pub mod sync {
    use std::{
        ffi::c_void, mem::{replace, zeroed},
        ptr::{self, null_mut},
    };
    use sart::{
        boxed::{RTSafeBoxWrapper, drop_rtbox, peek},
        ctr::{REGISTER_SET_SIZE, VMTaskState},
        map::HeapStructure,
    };
    mod alc {
        use std::{
            mem::{replace, zeroed},
            os::raw::c_void, ptr::null_mut, thread::yield_now,
        };
        use sart::{
            boxed::{RTSafeBoxWrapper, drop_rtbox},
            ctr::VMTaskState, futures::FutureTask, map::HeapStructure,
        };
        use crate::{BytecodeResolver, GLOBAL_RUNTIME, VM};
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_alloc<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            addr: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *task;
                let wrap = replace(&mut task.r6, zeroed());
                *(vm.heapmap as *mut HeapStructure).add(addr as usize) = wrap;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_alloc_super<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            addr: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *(&mut *task).super_;
                let wrap = replace(&mut task.r6, zeroed());
                *(vm.heapmap as *mut HeapStructure).add(addr as usize) = wrap;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r4_super<
            T: BytecodeResolver + Send + Sync + 'static,
        >(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let superheap = (vm.heaprestore as *mut HeapStructure).add(addr as _);
                let task = &mut *task;
                task.r4 = superheap as *const _ as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r5_super<
            T: BytecodeResolver + Send + Sync + 'static,
        >(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let data = (vm.heaprestore as *mut HeapStructure).add(addr as _);
                let task = &mut *task;
                task.r5 = data as *const _ as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r1<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            addr: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *task;
                task.r1 = RegistryValue {
                    ptr: (vm.heapmap as *mut HeapStructure).add(addr as usize) as *mut _
                        as _,
                };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r1_super<
            T: BytecodeResolver + Send + Sync + 'static,
        >(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let superheap = (vm.heaprestore as *mut HeapStructure).add(addr as _);
                let task = &mut *task;
                task.r1 = RegistryValue {
                    ptr: superheap as *const _ as _,
                };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r2<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            addr: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *task;
                task.r2 = RegistryValue {
                    ptr: (vm.heapmap as *mut HeapStructure).add(addr as usize) as *mut _
                        as _,
                };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r2_super<
            T: BytecodeResolver + Send + Sync + 'static,
        >(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let superheap = (vm.heaprestore as *mut HeapStructure).add(addr as _);
                let task = &mut *task;
                task.r2 = RegistryValue {
                    ptr: superheap as *const _ as _,
                };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r3<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            addr: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *task;
                task.r3 = RegistryValue {
                    ptr: (vm.heapmap as *mut HeapStructure).add(addr as usize) as *mut _
                        as _,
                };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r3_super<
            T: BytecodeResolver + Send + Sync + 'static,
        >(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let superheap = (vm.heaprestore as *mut HeapStructure).add(addr as _);
                let task = &mut *task;
                task.r3 = RegistryValue {
                    ptr: superheap as *const _ as _,
                };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r4<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            addr: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *task;
                task.r4 = (vm.heapmap as *mut HeapStructure).add(addr as _) as *mut _
                    as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_load_to_r5<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            addr: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *task;
                task.r5 = (vm.heapmap as *mut HeapStructure).add(addr as _) as *mut _
                    as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_yield(_: *mut c_void, _: *mut VMTaskState, _: u64) {
            yield_now();
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sync_await(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let wrap = RTSafeBoxWrapper::construct::<FutureTask>(task.r6.complex);
                task.r6 = HeapStructure {
                    complex: GLOBAL_RUNTIME
                        .block_on(async move { wrap.into_future().await })
                        .into_raw(),
                };
            }
        }
    }
    mod arithmatic {
        use std::os::raw::c_void;
        use sart::ctr::{RegistryValue, VMTaskState};
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1 / r2;
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1 / r2;
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                *r4 = r1 / *r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.wrapping_rem(r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                *r4 = r1.wrapping_rem(*r4);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.wrapping_rem(r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.unchecked_add(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.unchecked_add(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                *r4 = r1.unchecked_add(*r4 as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.unchecked_sub(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.unchecked_sub(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                *r4 = r1.unchecked_sub(*r4 as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.unchecked_mul(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.unchecked_mul(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                *r4 = r1.unchecked_mul(*r4 as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.unchecked_shl(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.unchecked_shl(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                *r4 = r1.unchecked_shl(*r4 as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.unchecked_shr(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.unchecked_shr(r2 as _);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                *r4 = r1.unchecked_shr(*r4 as _);
            }
        }
    }
    mod bitwise {
        use std::os::raw::c_void;
        use sart::ctr::{RegistryValue, VMTaskState};
        use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign};
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_and(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.bitand(&r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_and_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = &*(task.r2.ptr as *const u64);
                let r3 = r1.bitand(r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_and_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = &*(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                r4.bitand_assign(r1);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_or(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.bitor(&r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_or_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = &*(task.r2.ptr as *const u64);
                let r3 = r1.bitor(r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_or_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = &*(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                r4.bitor_assign(r1);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_xor(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.bitxor(&r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_xor_ptr(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = &*(task.r2.ptr as *const u64);
                let r3 = r1.bitxor(r2);
                task.r3 = RegistryValue { data: r3 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_xor_mut(_: *mut c_void, task: *mut VMTaskState, _: u64) {
            unsafe {
                let task = &mut *task;
                let r1 = &*(task.r1.ptr as *const u64);
                let r4 = &mut *(task.r4 as *mut u64);
                r4.bitxor_assign(r1);
            }
        }
    }
    mod cmp {
        use std::os::raw::c_void;
        use sart::ctr::VMTaskState;
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r1_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r1_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r1_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r1_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r1_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r1_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r1_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r1_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r1_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r1_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r1_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r2.data;
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r1_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r2_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r1.data;
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r2_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r2_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r1.data;
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r2_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r2_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r1.data;
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r2_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r2_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r1.data;
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r2_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r2_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r1.data;
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r2_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r2_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r1.data;
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r2_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r1_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r3.data;
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r1_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r1_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r3.data;
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r1_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r1_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r3.data;
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r1_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r1_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r3.data;
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r1_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r1_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r3.data;
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r1_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r1_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.data;
                let r2 = task.r3.data;
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r1_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r1.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r3_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r1.data;
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r3_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r3_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r1.data;
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r3_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r3_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r1.data;
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r3_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r3_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r1.data;
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r3_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r3_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r1.data;
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r3_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r3_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r1.data;
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r3_r1_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r1.ptr as *const u64);
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r2_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r3.data;
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r2_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r2_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r3.data;
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r2_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r2_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r3.data;
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r2_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r2_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r3.data;
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r2_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r2_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r3.data;
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r2_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r2_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r2.data;
                let r2 = task.r3.data;
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r2_r3_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r2.ptr as *const u64);
                let r2 = *(task.r3.ptr as *const u64);
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r3_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r2.data;
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_r3_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.eq(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r3_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r2.data;
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_r3_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.ne(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r3_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r2.data;
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_r3_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.gt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r3_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r2.data;
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_r3_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.lt(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r3_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r2.data;
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_r3_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.le(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r3_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r3.data;
                let r2 = task.r2.data;
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_r3_r2_ptr(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = *(task.r3.ptr as *const u64);
                let r2 = *(task.r2.ptr as *const u64);
                let r3 = r1.ge(&r2);
                task.r1 = RegistryValue { data: r3 as u64 };
            }
        }
    }
    mod heaps {
        use std::mem::zeroed;
        use sart::map::HeapStructure;
        pub const SYNC_HEAP: ::std::thread::LocalKey<SyncHeapMapStore> = {
            #[inline]
            fn __rust_std_internal_init_fn() -> SyncHeapMapStore {
                SyncHeapMapStore {
                    heaps: Box::new(unsafe { zeroed() }),
                    next: 0,
                }
            }
            unsafe {
                ::std::thread::LocalKey::new(const {
                    if ::std::mem::needs_drop::<SyncHeapMapStore>() {
                        |__rust_std_internal_init| {
                            #[thread_local]
                            static __RUST_STD_INTERNAL_VAL: ::std::thread::local_impl::LazyStorage<
                                SyncHeapMapStore,
                                (),
                            > = ::std::thread::local_impl::LazyStorage::new();
                            __RUST_STD_INTERNAL_VAL
                                .get_or_init(
                                    __rust_std_internal_init,
                                    __rust_std_internal_init_fn,
                                )
                        }
                    } else {
                        |__rust_std_internal_init| {
                            #[thread_local]
                            static __RUST_STD_INTERNAL_VAL: ::std::thread::local_impl::LazyStorage<
                                SyncHeapMapStore,
                                !,
                            > = ::std::thread::local_impl::LazyStorage::new();
                            __RUST_STD_INTERNAL_VAL
                                .get_or_init(
                                    __rust_std_internal_init,
                                    __rust_std_internal_init_fn,
                                )
                        }
                    }
                })
            }
        };
        pub struct SyncHeapMapStore {
            pub heaps: Box<[[HeapStructure; 256]; 21]>,
            pub next: usize,
        }
        impl SyncHeapMapStore {
            /// Returns the 1st pointer to a new heap struct
            /// This is an array but for convenience reasons we give you the 1st pointer only
            pub fn get(&mut self) -> *mut HeapStructure {
                unsafe {
                    if self.next >= 21 {
                        self.next += 1;
                        let heap: Box<[HeapStructure; 256]> = Box::new(zeroed());
                        return Box::into_raw(heap) as _;
                    }
                    let out = self.heaps.get_unchecked_mut(self.next);
                    self.next += 1;
                    out as *mut _ as *mut HeapStructure
                }
            }
            /// You must call this in exact strict deallocation order
            ///
            /// that means that
            /// if you get
            /// - ..
            /// - heap 21
            /// - heap 22
            /// - heap 23
            ///
            /// you must call this like this
            /// - heap 23
            /// - heap 22
            /// - heap 21
            /// ...
            pub fn collect(&mut self, heap: *mut HeapStructure) {
                unsafe {
                    if self.next >= 22 {
                        drop(Box::from_raw(heap));
                    }
                    self.next -= 1;
                }
            }
        }
    }
    mod threading {
        use core::ffi::c_void;
        use std::{mem::zeroed, thread::spawn};
        use sart::{ctr::VMTaskState, map::HeapStructure};
        use crate::{BytecodeResolver, MaybeBoxed, VM};
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sync_spawn<T: BytecodeResolver + Send + Sync + 'static>(
            vm: *mut c_void,
            task: *mut VMTaskState,
            module: u64,
        ) {
            unsafe {
                let vm = &mut *(vm as *mut VM<T>);
                let task = &mut *task;
                let (r8, vm) = vm.create_copy();
                *task.r6.heap() = HeapStructure { complex: r8 };
                spawn(move || match vm {
                    MaybeBoxed::Unboxed(mut vm) => {
                        let mut state: VMTaskState = zeroed();
                        vm.run_module(&mut state, module);
                    }
                    MaybeBoxed::Boxed(mut vm) => {
                        let mut state: Box<VMTaskState> = Box::new(zeroed());
                        vm.run_module(&mut state, module);
                    }
                });
            }
        }
    }
    pub use alc::*;
    pub use arithmatic::*;
    pub use bitwise::*;
    pub use cmp::*;
    pub use threading::*;
    use crate::{BytecodeResolver, CVM, VM};
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_addr(vm: *mut c_void, _: *mut VMTaskState, addr: u64) {
        unsafe {
            let data = ((&mut *(vm as *mut CVM)).heapmap.add(addr as _))
                as *mut RTSafeBoxWrapper;
            drop_rtbox(data);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_clr_r1(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            (&mut *task).r1.nullify();
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_r1(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            let r = (&mut *task).r1.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_super_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let r = (&mut *(&mut *task).super_).r1.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_r1(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *task).r1.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_super_r1(
        vm: *mut c_void,
        task: *mut VMTaskState,
        addr: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *(&mut *task).super_).r1.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_clr_r2(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            (&mut *task).r2.nullify();
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_r2(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            let r = (&mut *task).r2.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_super_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let r = (&mut *(&mut *task).super_).r2.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_r2(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *task).r2.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_super_r2(
        vm: *mut c_void,
        task: *mut VMTaskState,
        addr: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *(&mut *task).super_).r2.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_clr_r3(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            (&mut *task).r3.nullify();
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_r3(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            let r = (&mut *task).r3.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_super_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let r = (&mut *(&mut *task).super_).r3.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_r3(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *task).r3.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_super_r3(
        vm: *mut c_void,
        task: *mut VMTaskState,
        addr: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *(&mut *task).super_).r3.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_clr_r4(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            (&mut *task).r4.nullify();
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_r4(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            let r = (&mut *task).r4.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_super_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let r = (&mut *(&mut *task).super_).r4.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_r4(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *task).r4.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_super_r4(
        vm: *mut c_void,
        task: *mut VMTaskState,
        addr: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *(&mut *task).super_).r4.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_clr_r5(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            (&mut *task).r5.nullify();
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_r5(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            let r = (&mut *task).r5.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_super_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let r = (&mut *(&mut *task).super_).r5.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_r5(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *task).r5.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_super_r5(
        vm: *mut c_void,
        task: *mut VMTaskState,
        addr: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *(&mut *task).super_).r5.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_clr_r6(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            (&mut *task).r6.nullify();
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_r6(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            let r = (&mut *task).r6.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_free_super_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let r = (&mut *(&mut *task).super_).r6.heap();
            if true {
                if !!r._checknull.is_null() {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!("inst_free called with a null register."),
                        );
                    }
                }
            }
            drop_rtbox(r.complex);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_r6(vm: *mut c_void, task: *mut VMTaskState, addr: u64) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *task).r6.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_own_super_r6(
        vm: *mut c_void,
        task: *mut VMTaskState,
        addr: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;
            *(&mut *(&mut *task).super_).r6.heap() = *(vm.heapmap as *mut HeapStructure)
                .add(addr as usize);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jmp(_: *mut c_void, task: *mut VMTaskState, index: u64) {
        unsafe {
            let task = &mut *task;
            task.curline = index as usize;
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r2(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r2;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r3(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r3;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r4(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r4;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r5(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r5;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_u64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_u64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().u64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i8 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i8 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i16 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i16 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i32 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i32 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_i64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i64 == 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_i64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r6;
            if register.heap().i64 != 0 {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_clr_full(_: *mut c_void, task: *mut VMTaskState, _: u64) {
        unsafe {
            let task = &mut *task;
            ptr::write_bytes(&mut task.r1 as *mut _ as *mut u8, 0, REGISTER_SET_SIZE);
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_sync_libcall<T: BytecodeResolver + Send + Sync + 'static>(
        vm: *mut c_void,
        task: *mut VMTaskState,
        u: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut VM<T>);
            let task = &mut *task;
            let real_heap = replace(&mut vm.heapmap, zeroed());
            vm.counter += 1;
            if vm.counter > 10 {
                let mut state = Box::new(zeroed::<VMTaskState>());
                state.super_ = task as _;
                vm.run_module(state.as_mut(), u);
                drop(state);
                vm.counter -= 1;
                drop(replace(&mut vm.heapmap, real_heap));
                return;
            }
            let mut state = zeroed::<VMTaskState>();
            state.super_ = task as _;
            vm.run_module(&mut state, u);
            drop(state);
            drop(replace(&mut vm.heapmap, real_heap));
            vm.counter -= 1;
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn new_context<T: BytecodeResolver + Send + Sync + 'static>(
        vm: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut VM<T>);
            vm.heaprestore = Box::into_raw(Box::new(replace(&mut vm.heapmap, zeroed())));
            let new_task: VMTaskState = zeroed();
            let old_task = Box::into_raw(Box::new(replace(&mut *task, new_task)));
            let task = &mut *task;
            task.super_ = old_task;
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn restore_context<T: BytecodeResolver + Send + Sync + 'static>(
        vm: *mut c_void,
        task: *mut VMTaskState,
        _: u64,
    ) {
        unsafe {
            let vm = &mut *(vm as *mut VM<T>);
            let old_heapmap = *Box::from_raw(vm.heaprestore);
            drop(replace(&mut vm.heapmap, old_heapmap));
            vm.heaprestore = null_mut();
            let old_task_moved_from_heap = *Box::from_raw((*task).super_);
            let new_task = &mut *task;
            drop(replace(new_task, old_task_moved_from_heap));
        }
    }
}
pub(crate) static VMS: AtomicUsize = AtomicUsize::new(1);
static TOTAL_THREADS: LazyLock<usize> = LazyLock::new(|| {
    available_parallelism().unwrap().into()
});
static VMMADE: OnceLock<()> = OnceLock::new();
pub trait BytecodeResolver {
    type Output: Read + Seek;
    fn modules(&self) -> &[u32];
    /// We'll outselves parse it
    /// Return `NONE` only when it is not a bytecode module
    /// No other error can be accepted
    fn get_regions(&self, module: u32) -> Option<&[u32]>;
    /// We'll outselves parse it
    fn get_native_regions(&self, module: u32) -> &[u32];
    /// We'll outselves compile it
    ///
    /// Return `None` ONLY IF it is a native module (like a dylib)
    fn resolve_bytecode_exact(&self, module: u32, region: u32) -> Option<Self::Output>;
    /// No other error can be accepted
    fn resolve_native(&self, module: u32, func: u32) -> DispatchFn;
}
impl BytecodeResolver
for Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static> {
    type Output = File;
    fn get_native_regions(&self, module: u32) -> &[u32] {
        BytecodeResolver::get_native_regions(self.as_ref(), module)
    }
    fn get_regions(&self, module: u32) -> Option<&[u32]> {
        BytecodeResolver::get_regions(self.as_ref(), module)
    }
    fn modules(&self) -> &[u32] {
        BytecodeResolver::modules(self.as_ref())
    }
    fn resolve_bytecode_exact(&self, module: u32, region: u32) -> Option<Self::Output> {
        BytecodeResolver::resolve_bytecode_exact(self.as_ref(), module, region)
    }
    fn resolve_native(&self, module: u32, func: u32) -> DispatchFn {
        BytecodeResolver::resolve_native(self.as_ref(), module, func)
    }
}
pub static GLOBAL_RUNTIME: LazyLock<Runtime> = LazyLock::new(|| {
    Builder::new_multi_thread().enable_all().build().unwrap()
});
pub static VMCONF: RwLock<VmConfig> = RwLock::new(unsafe { zeroed() });
#[repr(C)]
pub struct VmConfig {
    pub jit: bool,
    pub cooperative: bool,
}
#[automatically_derived]
impl ::core::fmt::Debug for VmConfig {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field2_finish(
            f,
            "VmConfig",
            "jit",
            &self.jit,
            "cooperative",
            &&self.cooperative,
        )
    }
}
/// We create a VM for each thread executed
#[repr(C)]
pub struct VM<T: BytecodeResolver + Send + Sync + 'static> {
    pub resolve: Arc<T>,
    pub code: CompiledCode,
    pub recv: Option<Receiver<SendWrapper>>,
    pub counter: usize,
    pub heaprestore: *mut HeapStructure,
    /// This is the 1st pointer to the heap structure
    /// Ofc there are total `256` distinct addresses
    pub heapmap: *mut HeapStructure,
}
pub const INNERBYTES: usize = 2 * size_of::<Arc<()>>()
    + size_of::<Option<Receiver<SendWrapper>>>();
#[repr(C)]
/// VM but optimized to use from C/FFI Boundaries
pub struct CVM {
    pub _inner: [u8; INNERBYTES],
    pub counter: usize,
    pub heaprestore: *mut HeapStructure,
    pub heapmap: *mut HeapStructure,
}
const _PASS1: bool = size_of::<CVM>()
    == size_of::<VM<Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static>>>();
const _VERIFY1: () = if !_PASS1 {
    ::core::panicking::panic("assertion failed: _PASS1")
};
const _PASS2: bool = align_of::<CVM>()
    == align_of::<
        VM<Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static>>,
    >();
const _VERIFY2: () = if !_PASS2 {
    ::core::panicking::panic("assertion failed: _PASS2")
};
unsafe impl<T: BytecodeResolver + Send + Sync + 'static> Send for VM<T> {}
unsafe impl<T: BytecodeResolver + Send + Sync + 'static> Sync for VM<T> {}
pub fn pack_u32(high_u32: u32, low_u32: u32) -> u64 {
    let high_u64 = high_u32 as u64;
    let shifted_high = high_u64 << 32;
    let low_u64 = low_u32 as u64;
    shifted_high | low_u64
}
pub fn pack_u64(high_u64: u64, low_u64: u64) -> u128 {
    let high_u64 = high_u64 as u128;
    let shifted_high = high_u64 << 64;
    let low_u64 = low_u64 as u128;
    shifted_high | low_u64
}
pub fn unpack_u64(packed: u64) -> (u32, u32) {
    let high_u32 = (packed >> 32) as u32;
    let low_u32 = packed as u32;
    (high_u32, low_u32)
}
impl<T: BytecodeResolver + Send + Sync + 'static> VM<T> {
    /// Please note that module id `0` represents the main module
    pub fn new(data: T) -> Self {
        VMMADE
            .set(())
            .expect(
                "Cell must be initialized only once. We know there will be morons and that's why for the LOVE OF GOD, don't try this trick again",
            );
        let resolver = Arc::new(data);
        let resolve = resolver.clone();
        Self {
            resolve: resolve,
            counter: 0,
            recv: None,
            heapmap: unsafe { zeroed() },
            heaprestore: { null_mut() },
            code: {
                let out: CompiledCode = Arc::new(
                    DashMap::with_hasher(ahash::RandomState::new()),
                );
                let refsolver = resolver.as_ref();
                let refsolver_ptr = resolver.clone();
                refsolver
                    .modules()
                    .iter()
                    .for_each(|id| {
                        let modid = *id;
                        let refsolver_ptr = refsolver_ptr.clone();
                        match refsolver.get_regions(modid) {
                            Some(regions) => {
                                regions
                                    .iter()
                                    .for_each(|region| {
                                        let region = *region;
                                        let res = refsolver_ptr.clone();
                                        out.insert(
                                            pack_u32(modid, region),
                                            LazyLock::new(
                                                Box::new(move || sync_compile(res.as_ref(), modid, region)),
                                            ),
                                        );
                                    })
                            }
                            None => {
                                refsolver
                                    .get_native_regions(modid)
                                    .iter()
                                    .for_each(|region| {
                                        let region = *region;
                                        let output = refsolver_ptr.resolve_native(modid, region);
                                        out.insert(
                                            pack_u32(modid, region),
                                            LazyLock::new(
                                                Box::new(move || {
                                                    Box::new([Instruction { fn_: (0, output) }])
                                                }),
                                            ),
                                        );
                                    });
                            }
                        }
                    });
                out
            },
        }
    }
    /// This returns a Boxed copy is there are more than 5 VMs already
    pub fn create_copy(&self) -> (*mut RTSafeBoxWrapper, MaybeBoxed<Self>) {
        let old = VMS.fetch_add(1, Ordering::SeqCst);
        let (tx, rx) = channel::<SendWrapper>();
        let tx = unsafe { RTSafeBoxWrapper::new_raw(tx) };
        let tx = unsafe {
            RTSafeBoxWrapper::new_raw(ThreadSpawnContext {
                send,
                sender: tx,
            })
        };
        if old >= *TOTAL_THREADS {
            return (
                tx,
                MaybeBoxed::Boxed(
                    Box::new(Self {
                        code: self.code.clone(),
                        heapmap: unsafe { zeroed() },
                        counter: 0,
                        recv: Some(rx),
                        heaprestore: { null_mut() },
                        resolve: self.resolve.clone(),
                    }),
                ),
            );
        }
        (
            tx,
            MaybeBoxed::Unboxed(Self {
                code: self.code.clone(),
                heapmap: unsafe { zeroed() },
                counter: 0,
                recv: Some(rx),
                heaprestore: { null_mut() },
                resolve: self.resolve.clone(),
            }),
        )
    }
}
impl<T: BytecodeResolver + Send + Sync + 'static> Drop for VM<T> {
    fn drop(&mut self) {
        VMS.fetch_sub(1, Ordering::SeqCst);
    }
}
pub enum MaybeBoxed<T> {
    Boxed(Box<T>),
    Unboxed(T),
}
