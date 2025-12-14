#![feature(prelude_import)]
#![feature(
    seek_stream_len,
    portable_simd,
    unchecked_shifts,
    exact_div,
    int_roundings,
    nonpoison_rwlock,
    sync_nonpoison,
    unsafe_cell_access
)]
#[macro_use]
extern crate std;
#[prelude_import]
use std::prelude::rust_2024::*;
pub mod acaot {
    //! This is the ACAoT compiler
    //! `Aggressive Cycle-Adaptive Ordered Threading`` Compiler
    //! It is a really quick compiler build to speed up like crazy
    use crate::{BytecodeResolver, acaot::compiler::{SyncCompiler, X2U128}};
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
            pub markers: HashMap<X2U128, usize>,
            pub instance_counter: HashMap<u64, u128>,
            pub depth: usize,
            pub to_add_to_vec_len: usize,
            pub module: u64,
        }
        #[repr(C)]
        pub struct X2U128 {
            u128_1: u128,
            u128_2: u128,
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for X2U128 {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_struct_field2_finish(
                    f,
                    "X2U128",
                    "u128_1",
                    &self.u128_1,
                    "u128_2",
                    &&self.u128_2,
                )
            }
        }
        #[automatically_derived]
        #[doc(hidden)]
        unsafe impl ::core::clone::TrivialClone for X2U128 {}
        #[automatically_derived]
        impl ::core::clone::Clone for X2U128 {
            #[inline]
            fn clone(&self) -> X2U128 {
                let _: ::core::clone::AssertParamIsClone<u128>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for X2U128 {}
        #[automatically_derived]
        impl ::core::hash::Hash for X2U128 {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
                ::core::hash::Hash::hash(&self.u128_1, state);
                ::core::hash::Hash::hash(&self.u128_2, state)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for X2U128 {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for X2U128 {
            #[inline]
            fn eq(&self, other: &X2U128) -> bool {
                self.u128_1 == other.u128_1 && self.u128_2 == other.u128_2
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for X2U128 {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_receiver_is_total_eq(&self) -> () {
                let _: ::core::cmp::AssertParamIsEq<u128>;
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for X2U128 {
            #[inline]
            fn partial_cmp(
                &self,
                other: &X2U128,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                match ::core::cmp::PartialOrd::partial_cmp(&self.u128_1, &other.u128_1) {
                    ::core::option::Option::Some(::core::cmp::Ordering::Equal) => {
                        ::core::cmp::PartialOrd::partial_cmp(&self.u128_2, &other.u128_2)
                    }
                    cmp => cmp,
                }
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Ord for X2U128 {
            #[inline]
            fn cmp(&self, other: &X2U128) -> ::core::cmp::Ordering {
                match ::core::cmp::Ord::cmp(&self.u128_1, &other.u128_1) {
                    ::core::cmp::Ordering::Equal => {
                        ::core::cmp::Ord::cmp(&self.u128_2, &other.u128_2)
                    }
                    cmp => cmp,
                }
            }
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
                        INSTRUCTION_ALLOC => self.handle_alloc(code),
                        INSTRUCTION_LOAD => self.handle_load(code),
                        INSTRUCTION_FREE => self.handle_free(code),
                        INSTRUCTION_OWN => self.handle_own(code),
                        INSTRUCTION_MOV => self.handle_mov(false, code),
                        INSTRUCTION_SUPER_MOV => self.handle_mov(true, code),
                        INSTRUCTION_MOV_TO_SUPER => self.handle_mov_to(code),
                        INSTRUCTION_PUT_REG => self.put_reg(code),
                        INSTRUCTION_CMP => self.handle_compare(code),
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
                        INSTRUCTION_ADD_PTR => self.handle_ptraith(PTRADDOP, code),
                        INSTRUCTION_SUB_PTR => self.handle_ptraith(PTRSUBOP, code),
                        INSTRUCTION_OFFSET_PTR => self.handle_ptraith(PTROFFSETOP, code),
                        INSTRUCTION_SHL => self.handle_alu(SHROPS, code),
                        INSTRUCTION_SHR => self.handle_alu(SHLOPS, code),
                        INSTRUCTION_SHL_MUT => self.handle_alu(SHLMUTOPS, code),
                        INSTRUCTION_SHR_MUT => self.handle_alu(SHRMUTOPS, code),
                        INSTRUCTION_AND => self.handle_bitwise(ANDOPS, code),
                        INSTRUCTION_AND_MUT => self.handle_bitwise(ANDMUTOPS, code),
                        INSTRUCTION_OR => self.handle_bitwise(OROPS, code),
                        INSTRUCTION_OR_MUT => self.handle_bitwise(ORMUTOPS, code),
                        INSTRUCTION_XOR => self.handle_bitwise(XOROPS, code),
                        INSTRUCTION_XOR_MUT => self.handle_bitwise(XORMUTOPS, code),
                        INSTRUCTION_LIBCALL => self.handle_libcall(code),
                        e => {
                            ::core::panicking::panic_fmt(
                                format_args!("Unexpected {0}", e),
                            );
                        }
                    }
                }
            }
            unsafe fn put_reg(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut register = [0u8; 2];
                self.reader.read_exact(&mut register).expect("Error");
                let [to, typ] = register;
                let mut data = [0u8; 8];
                self.reader.read_exact(&mut data).expect("Error");
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (u64::from_le_bytes(data), put_reg_handler(to, typ)),
                    }),
                )
            }
            unsafe fn handle_alu(
                &mut self,
                op: u8,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                let mut register = [0u8; 1];
                self.reader.read_exact(&mut register).expect("Error");
                let [typ] = register;
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (0, inst_arithmetic_handler(typ, op)),
                    }),
                )
            }
            unsafe fn handle_ptraith(
                &mut self,
                op: u8,
                code: &mut Vec<FirstPassInstruction>,
            ) {
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
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (data, inst_ptrarith_handler(typ, op)),
                    }),
                )
            }
            unsafe fn handle_bitwise(
                &mut self,
                op: u8,
                code: &mut Vec<FirstPassInstruction>,
            ) {
                let mut register = [0u8; 1];
                self.reader.read_exact(&mut register).expect("Error");
                let [typ] = register;
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (0, inst_bitwise_handler(typ, op)),
                    }),
                )
            }
            unsafe fn handle_free(&mut self, code: &mut Vec<FirstPassInstruction>) {
                let mut register = [0u8; 1];
                self.reader.read_exact(&mut register).expect("Error");
                let [register] = register;
                if register >= 7 {
                    let mut addr = [0u8; 2];
                    self.reader.read_exact(&mut addr).expect("Error");
                    let addr = u16::from_le_bytes(addr);
                    code.push(
                        FirstPassInstruction::Inst(Instruction {
                            fn_: (addr as _, inst_free_addr),
                        }),
                    );
                    return;
                }
                let inst = match register {
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
            unsafe fn handle_mov(
                &mut self,
                super_: bool,
                code: &mut Vec<FirstPassInstruction>,
            ) {
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
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let mut addr = [0u8; 2];
                self.reader.read_exact(&mut addr).expect("Error");
                let addr = u16::from_le_bytes(addr);
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (addr as _, inst),
                    }),
                );
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
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (address as _, inst_alloc::<F>),
                    }),
                );
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
                self.markers
                    .insert(
                        X2U128 {
                            u128_1: key,
                            u128_2: instid,
                        },
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
                let addr = u64::from_le_bytes(module);
                code.push(
                    FirstPassInstruction::Inst(Instruction {
                        fn_: (addr, inst_sync_spawn::<F>),
                    }),
                );
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
                    0 => {
                        code.push(FirstPassInstruction::JumpCond {
                            marker: X2U128 {
                                u128_1: key,
                                u128_2: instid,
                            },
                            inst: jz_map(datatype),
                        })
                    }
                    1 => {
                        code.push(FirstPassInstruction::JumpCond {
                            marker: X2U128 {
                                u128_1: key,
                                u128_2: instid,
                            },
                            inst: jz_ptr_map(datatype),
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
                    0 => {
                        code.push(FirstPassInstruction::JumpCond {
                            marker: X2U128 {
                                u128_1: key,
                                u128_2: instid,
                            },
                            inst: jnz_map(datatype),
                        })
                    }
                    1 => {
                        code.push(FirstPassInstruction::JumpCond {
                            marker: X2U128 {
                                u128_1: key,
                                u128_2: instid,
                            },
                            inst: jnz_ptr_map(datatype),
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
                let id = u64::from_le_bytes(register);
                let (modid, region) = unpack_u64(id);
                if let Some(bytecode) = self
                    .resolver
                    .resolve_bytecode_exact(modid, region)
                {
                    self.depth += 1;
                    self.instance_counter.entry(id).and_modify(|x| *x += 1).or_insert(1);
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
        Jmp { marker: X2U128 },
        JumpCond { marker: X2U128, inst: DispatchFn },
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
            instance_counter: HashMap::default(),
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
    mem::zeroed,
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
    use crate::{BytecodeResolver, VM, sync::heaps::SYNC_HEAP};
    impl<T: BytecodeResolver + Send + Sync + 'static> VM<T> {
        /// Please note that from this point onwards we'll purely compute the values
        /// and wont check any single thing
        ///
        /// This strictly runs module id `0` section `0`
        pub unsafe fn run(&mut self) {
            unsafe {
                self.heapmap = SYNC_HEAP.with(|x| x.as_mut_unchecked().get());
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
        ptr,
    };
    use sart::{
        boxed::{RTSafeBoxWrapper, drop_rtbox},
        ctr::{DispatchFn, REGISTER_SET_SIZE, VMTaskState},
        map::HeapStructure,
    };
    pub mod heaps {
        use std::{cell::UnsafeCell, mem::zeroed};
        use sart::map::HeapStructure;
        pub const SYNC_HEAP: ::std::thread::LocalKey<UnsafeCell<SyncHeapMapStore>> = {
            #[inline]
            fn __rust_std_internal_init_fn() -> UnsafeCell<SyncHeapMapStore> {
                UnsafeCell::new(SyncHeapMapStore {
                    heaps: Box::new(unsafe { zeroed() }),
                    points: Vec::with_capacity(5),
                    length: 0,
                })
            }
            unsafe {
                ::std::thread::LocalKey::new(const {
                    if ::std::mem::needs_drop::<UnsafeCell<SyncHeapMapStore>>() {
                        |__rust_std_internal_init| {
                            #[thread_local]
                            static __RUST_STD_INTERNAL_VAL: ::std::thread::local_impl::LazyStorage<
                                UnsafeCell<SyncHeapMapStore>,
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
                                UnsafeCell<SyncHeapMapStore>,
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
        pub type OwnedHeap = [HeapStructure; 256];
        pub struct SyncHeapMapStore {
            pub heaps: Box<[OwnedHeap; 21]>,
            pub points: Vec<*mut HeapStructure>,
            pub length: usize,
        }
        impl SyncHeapMapStore {
            /// Returns the 1st pointer to a new heap struct
            /// This is an array but for convenience reasons we give you the 1st pointer only
            pub fn get(&mut self) -> *mut HeapStructure {
                unsafe {
                    let length = &mut self.length;
                    if *length >= 21 {
                        *length += 1;
                        let heap: Box<OwnedHeap> = Box::new(zeroed());
                        let pointer = Box::into_raw(heap) as *mut HeapStructure;
                        self.points.push(pointer);
                        return pointer;
                    }
                    let out = self.heaps.get_unchecked_mut(*length);
                    *length += 1;
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
            pub fn collect(&mut self) -> *mut HeapStructure {
                unsafe {
                    let length = &mut self.length;
                    if *length == 0 {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "Please use the API Correctly. E_CONDITION_01_CALLED_WITH_LENGTH_0",
                                ),
                            );
                        };
                    }
                    if *length >= 22 {
                        *length -= 1;
                        let heap = self.points.pop().unwrap_unchecked();
                        drop(Box::from_raw(heap as *mut OwnedHeap));
                        if *length > 21 {
                            return *self.points.last().unwrap_unchecked() as _;
                        }
                    } else {
                        *length -= 1;
                    }
                    if *length == 0 {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "Please use the API Correctly. E_CONDITION_01_CALLED_WITH_LENGTH_1",
                                ),
                            );
                        };
                    }
                    self.heaps.get_unchecked_mut(*length - 1) as *mut _ as _
                }
            }
        }
    }
    mod alc {
        use std::{
            mem::{replace, zeroed},
            os::raw::c_void, thread::yield_now,
        };
        use sart::{
            boxed::RTSafeBoxWrapper, ctr::VMTaskState, futures::FutureTask,
            map::HeapStructure,
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
                *(vm.heapmap as *mut HeapStructure).add(addr as usize) = wrap.data;
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
                task.r1 = HeapStructure {
                    complex: (vm.heapmap as *mut HeapStructure).add(addr as usize)
                        as *mut _ as _,
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
                task.r2 = HeapStructure {
                    complex: (vm.heapmap as *mut HeapStructure).add(addr as usize)
                        as *mut _ as _,
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
                task.r3 = HeapStructure {
                    complex: (vm.heapmap as *mut HeapStructure).add(addr as usize)
                        as *mut _ as _,
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
                task.r4 = *((vm.heapmap as *mut HeapStructure).add(addr as _) as *mut _);
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
                task.r5.ptr = (vm.heapmap as *mut HeapStructure).add(addr as _) as *mut _
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
                let wrap = RTSafeBoxWrapper::construct::<
                    FutureTask,
                >(task.r6.heap().complex);
                *task.r6.heap() = HeapStructure {
                    complex: GLOBAL_RUNTIME
                        .block_on(async move { wrap.into_future().await })
                        .into_raw(),
                };
            }
        }
    }
    mod arithmatic {
        use std::ops::{Add, Div, Mul, Rem, Shl, Shr, Sub};
        use std::os::raw::c_void;
        use sart::ctr::{DispatchFn, VMTaskState};
        pub const ADDOPS: u8 = 1;
        pub const SUBOPS: u8 = 2;
        pub const MULOPS: u8 = 3;
        pub const DIVOPS: u8 = 4;
        pub const REMOPS: u8 = 5;
        pub const SHLOPS: u8 = 6;
        pub const SHROPS: u8 = 7;
        pub const ADDMUTOPS: u8 = 8;
        pub const SUBMUTOPS: u8 = 9;
        pub const MULMUTOPS: u8 = 10;
        pub const DIVMUTOPS: u8 = 11;
        pub const REMMUTOPS: u8 = 12;
        pub const SHLMUTOPS: u8 = 13;
        pub const SHRMUTOPS: u8 = 14;
        pub fn inst_arithmetic_handler(typ: u8, op: u8) -> DispatchFn {
            let f = match typ {
                0 => {
                    match op {
                        ADDOPS => inst_add_data_is_u8,
                        SUBOPS => inst_sub_data_is_u8,
                        MULOPS => inst_mul_data_is_u8,
                        DIVOPS => inst_div_data_is_u8,
                        REMOPS => inst_rem_data_is_u8,
                        SHLOPS => inst_shl_data_is_u8,
                        SHROPS => inst_shr_data_is_u8,
                        ADDMUTOPS => inst_add_data_is_u8_mut,
                        SUBMUTOPS => inst_sub_data_is_u8_mut,
                        MULMUTOPS => inst_mul_data_is_u8_mut,
                        DIVMUTOPS => inst_div_data_is_u8_mut,
                        REMMUTOPS => inst_rem_data_is_u8_mut,
                        SHLMUTOPS => inst_shl_data_is_u8_mut,
                        SHRMUTOPS => inst_shr_data_is_u8_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                1 => {
                    match op {
                        ADDOPS => inst_add_data_is_u16,
                        SUBOPS => inst_sub_data_is_u16,
                        MULOPS => inst_mul_data_is_u16,
                        DIVOPS => inst_div_data_is_u16,
                        REMOPS => inst_rem_data_is_u16,
                        SHLOPS => inst_shl_data_is_u16,
                        SHROPS => inst_shr_data_is_u16,
                        ADDMUTOPS => inst_add_data_is_u16_mut,
                        SUBMUTOPS => inst_sub_data_is_u16_mut,
                        MULMUTOPS => inst_mul_data_is_u16_mut,
                        DIVMUTOPS => inst_div_data_is_u16_mut,
                        REMMUTOPS => inst_rem_data_is_u16_mut,
                        SHLMUTOPS => inst_shl_data_is_u16_mut,
                        SHRMUTOPS => inst_shr_data_is_u16_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                2 => {
                    match op {
                        ADDOPS => inst_add_data_is_u32,
                        SUBOPS => inst_sub_data_is_u32,
                        MULOPS => inst_mul_data_is_u32,
                        DIVOPS => inst_div_data_is_u32,
                        REMOPS => inst_rem_data_is_u32,
                        SHLOPS => inst_shl_data_is_u32,
                        SHROPS => inst_shr_data_is_u32,
                        ADDMUTOPS => inst_add_data_is_u32_mut,
                        SUBMUTOPS => inst_sub_data_is_u32_mut,
                        MULMUTOPS => inst_mul_data_is_u32_mut,
                        DIVMUTOPS => inst_div_data_is_u32_mut,
                        REMMUTOPS => inst_rem_data_is_u32_mut,
                        SHLMUTOPS => inst_shl_data_is_u32_mut,
                        SHRMUTOPS => inst_shr_data_is_u32_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                3 => {
                    match op {
                        ADDOPS => inst_add_data_is_u64,
                        SUBOPS => inst_sub_data_is_u64,
                        MULOPS => inst_mul_data_is_u64,
                        DIVOPS => inst_div_data_is_u64,
                        REMOPS => inst_rem_data_is_u64,
                        SHLOPS => inst_shl_data_is_u64,
                        SHROPS => inst_shr_data_is_u64,
                        ADDMUTOPS => inst_add_data_is_u64_mut,
                        SUBMUTOPS => inst_sub_data_is_u64_mut,
                        MULMUTOPS => inst_mul_data_is_u64_mut,
                        DIVMUTOPS => inst_div_data_is_u64_mut,
                        REMMUTOPS => inst_rem_data_is_u64_mut,
                        SHLMUTOPS => inst_shl_data_is_u64_mut,
                        SHRMUTOPS => inst_shr_data_is_u64_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                4 => {
                    match op {
                        ADDOPS => inst_add_data_is_i8,
                        SUBOPS => inst_sub_data_is_i8,
                        MULOPS => inst_mul_data_is_i8,
                        DIVOPS => inst_div_data_is_i8,
                        REMOPS => inst_rem_data_is_i8,
                        SHLOPS => inst_shl_data_is_i8,
                        SHROPS => inst_shr_data_is_i8,
                        ADDMUTOPS => inst_add_data_is_i8_mut,
                        SUBMUTOPS => inst_sub_data_is_i8_mut,
                        MULMUTOPS => inst_mul_data_is_i8_mut,
                        DIVMUTOPS => inst_div_data_is_i8_mut,
                        REMMUTOPS => inst_rem_data_is_i8_mut,
                        SHLMUTOPS => inst_shl_data_is_i8_mut,
                        SHRMUTOPS => inst_shr_data_is_i8_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                5 => {
                    match op {
                        ADDOPS => inst_add_data_is_i16,
                        SUBOPS => inst_sub_data_is_i16,
                        MULOPS => inst_mul_data_is_i16,
                        DIVOPS => inst_div_data_is_i16,
                        REMOPS => inst_rem_data_is_i16,
                        SHLOPS => inst_shl_data_is_i16,
                        SHROPS => inst_shr_data_is_i16,
                        ADDMUTOPS => inst_add_data_is_i16_mut,
                        SUBMUTOPS => inst_sub_data_is_i16_mut,
                        MULMUTOPS => inst_mul_data_is_i16_mut,
                        DIVMUTOPS => inst_div_data_is_i16_mut,
                        REMMUTOPS => inst_rem_data_is_i16_mut,
                        SHLMUTOPS => inst_shl_data_is_i16_mut,
                        SHRMUTOPS => inst_shr_data_is_i16_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                6 => {
                    match op {
                        ADDOPS => inst_add_data_is_i32,
                        SUBOPS => inst_sub_data_is_i32,
                        MULOPS => inst_mul_data_is_i32,
                        DIVOPS => inst_div_data_is_i32,
                        REMOPS => inst_rem_data_is_i32,
                        SHLOPS => inst_shl_data_is_i32,
                        SHROPS => inst_shr_data_is_i32,
                        ADDMUTOPS => inst_add_data_is_i32_mut,
                        SUBMUTOPS => inst_sub_data_is_i32_mut,
                        MULMUTOPS => inst_mul_data_is_i32_mut,
                        DIVMUTOPS => inst_div_data_is_i32_mut,
                        REMMUTOPS => inst_rem_data_is_i32_mut,
                        SHLMUTOPS => inst_shl_data_is_i32_mut,
                        SHRMUTOPS => inst_shr_data_is_i32_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                7 => {
                    match op {
                        ADDOPS => inst_add_data_is_i64,
                        SUBOPS => inst_sub_data_is_i64,
                        MULOPS => inst_mul_data_is_i64,
                        DIVOPS => inst_div_data_is_i64,
                        REMOPS => inst_rem_data_is_i64,
                        SHLOPS => inst_shl_data_is_i64,
                        SHROPS => inst_shr_data_is_i64,
                        ADDMUTOPS => inst_add_data_is_i64_mut,
                        SUBMUTOPS => inst_sub_data_is_i64_mut,
                        MULMUTOPS => inst_mul_data_is_i64_mut,
                        DIVMUTOPS => inst_div_data_is_i64_mut,
                        REMMUTOPS => inst_rem_data_is_i64_mut,
                        SHLMUTOPS => inst_shl_data_is_i64_mut,
                        SHRMUTOPS => inst_shr_data_is_i64_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                8 => {
                    match op {
                        ADDOPS => inst_add_data_is_f32,
                        SUBOPS => inst_sub_data_is_f32,
                        MULOPS => inst_mul_data_is_f32,
                        DIVOPS => inst_div_data_is_f32,
                        REMOPS => inst_rem_data_is_f32,
                        ADDMUTOPS => inst_add_data_is_f32_mut,
                        SUBMUTOPS => inst_sub_data_is_f32_mut,
                        MULMUTOPS => inst_mul_data_is_f32_mut,
                        DIVMUTOPS => inst_div_data_is_f32_mut,
                        REMMUTOPS => inst_rem_data_is_f32_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                9 => {
                    match op {
                        ADDOPS => inst_add_data_is_f64,
                        SUBOPS => inst_sub_data_is_f64,
                        MULOPS => inst_mul_data_is_f64,
                        DIVOPS => inst_div_data_is_f64,
                        REMOPS => inst_rem_data_is_f64,
                        ADDMUTOPS => inst_add_data_is_f64_mut,
                        SUBMUTOPS => inst_sub_data_is_f64_mut,
                        MULMUTOPS => inst_mul_data_is_f64_mut,
                        DIVMUTOPS => inst_div_data_is_f64_mut,
                        REMMUTOPS => inst_rem_data_is_f64_mut,
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("Invalid arithmetic operation code"),
                                ),
                            );
                        }
                    }
                }
                _ => {
                    ::core::panicking::panic_fmt(
                        format_args!(
                            "internal error: entered unreachable code: {0}",
                            format_args!("Invalid data type code"),
                        ),
                    );
                }
            };
            f
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.add(r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.add(r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.add(r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.add(r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.add(r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.add(r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.add(r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.add(r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.sub(r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.sub(r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.sub(r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.sub(r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.sub(r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.sub(r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.sub(r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.sub(r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.mul(r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.mul(r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.mul(r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.mul(r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.mul(r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.mul(r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.mul(r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.mul(r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.shl(r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.shl(r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.shl(r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.shl(r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.shl(r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.shl(r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.shl(r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.shl(r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shl_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.shl(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.shr(r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.shr(r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.shr(r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.shr(r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.shr(r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.shr(r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.shr(r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.shr(r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_shr_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.shr(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.div(r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.div(r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.div(r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.div(r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.div(r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.div(r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.div(r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.div(r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.rem(r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.rem(r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.rem(r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.rem(r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.rem(r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.rem(r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.rem(r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.rem(r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                let r3 = r1.add(r2);
                task.r1.heap().f32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_f32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                task.r2.heap().f32 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                let r3 = r1.sub(r2);
                task.r1.heap().f32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_f32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                task.r2.heap().f32 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                let r3 = r1.mul(r2);
                task.r1.heap().f32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_f32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                task.r2.heap().f32 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                let r3 = r1.div(r2);
                task.r1.heap().f32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_f32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                task.r2.heap().f32 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                let r3 = r1.rem(r2);
                task.r1.heap().f32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_f32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f32;
                let r2 = task.r2.heap().f32;
                task.r2.heap().f32 = r1.rem(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                let r3 = r1.add(r2);
                task.r1.heap().f64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_add_data_is_f64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                task.r2.heap().f64 = r1.add(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                let r3 = r1.sub(r2);
                task.r1.heap().f64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_sub_data_is_f64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                task.r2.heap().f64 = r1.sub(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                let r3 = r1.mul(r2);
                task.r1.heap().f64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mul_data_is_f64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                task.r2.heap().f64 = r1.mul(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                let r3 = r1.div(r2);
                task.r1.heap().f64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_div_data_is_f64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                task.r2.heap().f64 = r1.div(r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                let r3 = r1.rem(r2);
                task.r1.heap().f64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_rem_data_is_f64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().f64;
                let r2 = task.r2.heap().f64;
                task.r2.heap().f64 = r1.rem(r2);
            }
        }
    }
    mod bitwise {
        use std::ops::{BitAnd, BitOr, BitXor};
        use std::os::raw::c_void;
        use sart::ctr::{DispatchFn, VMTaskState};
        pub const ANDOPS: u8 = 1;
        pub const OROPS: u8 = 2;
        pub const XOROPS: u8 = 3;
        pub const ANDMUTOPS: u8 = 4;
        pub const ORMUTOPS: u8 = 5;
        pub const XORMUTOPS: u8 = 6;
        pub fn inst_bitwise_handler(typ: u8, op: u8) -> DispatchFn {
            let f = match typ {
                0 => {
                    match op {
                        1 => inst_bitwise_and_data_is_u8,
                        2 => inst_bitwise_or_data_is_u8,
                        3 => inst_bitwise_xor_data_is_u8,
                        4 => inst_bitwise_and_data_is_u8_mut,
                        5 => inst_bitwise_or_data_is_u8_mut,
                        6 => inst_bitwise_xor_data_is_u8_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                1 => {
                    match op {
                        1 => inst_bitwise_and_data_is_u16,
                        2 => inst_bitwise_or_data_is_u16,
                        3 => inst_bitwise_xor_data_is_u16,
                        4 => inst_bitwise_and_data_is_u16_mut,
                        5 => inst_bitwise_or_data_is_u16_mut,
                        6 => inst_bitwise_xor_data_is_u16_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                2 => {
                    match op {
                        1 => inst_bitwise_and_data_is_u32,
                        2 => inst_bitwise_or_data_is_u32,
                        3 => inst_bitwise_xor_data_is_u32,
                        4 => inst_bitwise_and_data_is_u32_mut,
                        5 => inst_bitwise_or_data_is_u32_mut,
                        6 => inst_bitwise_xor_data_is_u32_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                3 => {
                    match op {
                        1 => inst_bitwise_and_data_is_u64,
                        2 => inst_bitwise_or_data_is_u64,
                        3 => inst_bitwise_xor_data_is_u64,
                        4 => inst_bitwise_and_data_is_u64_mut,
                        5 => inst_bitwise_or_data_is_u64_mut,
                        6 => inst_bitwise_xor_data_is_u64_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                4 => {
                    match op {
                        1 => inst_bitwise_and_data_is_i8,
                        2 => inst_bitwise_or_data_is_i8,
                        3 => inst_bitwise_xor_data_is_i8,
                        4 => inst_bitwise_and_data_is_i8_mut,
                        5 => inst_bitwise_or_data_is_i8_mut,
                        6 => inst_bitwise_xor_data_is_i8_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                5 => {
                    match op {
                        1 => inst_bitwise_and_data_is_i16,
                        4 => inst_bitwise_and_data_is_i16_mut,
                        2 => inst_bitwise_or_data_is_i16,
                        5 => inst_bitwise_or_data_is_i16_mut,
                        3 => inst_bitwise_xor_data_is_i16,
                        6 => inst_bitwise_xor_data_is_i16_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                6 => {
                    match op {
                        1 => inst_bitwise_and_data_is_i32,
                        4 => inst_bitwise_and_data_is_i32_mut,
                        2 => inst_bitwise_or_data_is_i32,
                        5 => inst_bitwise_or_data_is_i32_mut,
                        3 => inst_bitwise_xor_data_is_i32,
                        6 => inst_bitwise_xor_data_is_i32_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                7 => {
                    match op {
                        1 => inst_bitwise_and_data_is_i64,
                        4 => inst_bitwise_and_data_is_i64_mut,
                        2 => inst_bitwise_or_data_is_i64,
                        5 => inst_bitwise_or_data_is_i64_mut,
                        3 => inst_bitwise_xor_data_is_i64,
                        6 => inst_bitwise_xor_data_is_i64_mut,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            };
            f
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.bitand(&r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.bitor(&r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().u8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u8;
                let r2 = task.r2.heap().u8;
                task.r2.heap().u8 = r1.bitxor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.bitand(&r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.bitor(&r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().u16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u16;
                let r2 = task.r2.heap().u16;
                task.r2.heap().u16 = r1.bitxor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.bitand(&r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.bitor(&r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().u32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u32;
                let r2 = task.r2.heap().u32;
                task.r2.heap().u32 = r1.bitxor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.bitand(&r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.bitor(&r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().u64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_u64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().u64;
                let r2 = task.r2.heap().u64;
                task.r2.heap().u64 = r1.bitxor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.bitand(&r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.bitor(&r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().i8 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i8_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i8;
                let r2 = task.r2.heap().i8;
                task.r2.heap().i8 = r1.bitxor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.bitand(&r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.bitor(&r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().i16 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i16_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i16;
                let r2 = task.r2.heap().i16;
                task.r2.heap().i16 = r1.bitxor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.bitand(&r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.bitor(&r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().i32 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i32_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i32;
                let r2 = task.r2.heap().i32;
                task.r2.heap().i32 = r1.bitxor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.bitand(&r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_and_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.bitand(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.bitor(&r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_or_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.bitor(&r2);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                let r3 = r1.bitxor(&r2);
                task.r1.heap().i64 = r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_bitwise_xor_data_is_i64_mut(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                let r1 = task.r1.heap().i64;
                let r2 = task.r2.heap().i64;
                task.r2.heap().i64 = r1.bitxor(&r2);
            }
        }
    }
    mod cmp {
        use std::os::raw::c_void;
        use sart::ctr::{DispatchFn, VMTaskState};
        pub fn inst_cmp_handler(typ: u8, op: u8, r1: u8, r2: u8) -> (u64, DispatchFn) {
            let data64 = u64::from_le_bytes([r1, r2, 0, 0, 0, 0, 0, 0]);
            let f = match typ {
                0 => {
                    match op {
                        1 => inst_cmp_eq_data_is_u8,
                        2 => inst_cmp_ne_data_is_u8,
                        3 => inst_cmp_gt_data_is_u8,
                        4 => inst_cmp_lt_data_is_u8,
                        5 => inst_cmp_ge_data_is_u8,
                        6 => inst_cmp_le_data_is_u8,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                1 => {
                    match op {
                        1 => inst_cmp_eq_data_is_u16,
                        2 => inst_cmp_ne_data_is_u16,
                        3 => inst_cmp_gt_data_is_u16,
                        4 => inst_cmp_lt_data_is_u16,
                        5 => inst_cmp_ge_data_is_u16,
                        6 => inst_cmp_le_data_is_u16,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                2 => {
                    match op {
                        1 => inst_cmp_eq_data_is_u32,
                        2 => inst_cmp_ne_data_is_u32,
                        3 => inst_cmp_gt_data_is_u32,
                        4 => inst_cmp_lt_data_is_u32,
                        5 => inst_cmp_ge_data_is_u32,
                        6 => inst_cmp_le_data_is_u32,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                3 => {
                    match op {
                        1 => inst_cmp_eq_data_is_u64,
                        2 => inst_cmp_ne_data_is_u64,
                        3 => inst_cmp_gt_data_is_u64,
                        4 => inst_cmp_lt_data_is_u64,
                        5 => inst_cmp_ge_data_is_u64,
                        6 => inst_cmp_le_data_is_u64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                4 => {
                    match op {
                        1 => inst_cmp_eq_data_is_i8,
                        2 => inst_cmp_ne_data_is_i8,
                        3 => inst_cmp_gt_data_is_i8,
                        4 => inst_cmp_lt_data_is_i8,
                        5 => inst_cmp_ge_data_is_i8,
                        6 => inst_cmp_le_data_is_i8,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                5 => {
                    match op {
                        1 => inst_cmp_eq_data_is_i16,
                        2 => inst_cmp_ne_data_is_i16,
                        3 => inst_cmp_gt_data_is_i16,
                        4 => inst_cmp_lt_data_is_i16,
                        5 => inst_cmp_ge_data_is_i16,
                        6 => inst_cmp_le_data_is_i16,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                6 => {
                    match op {
                        1 => inst_cmp_eq_data_is_i32,
                        2 => inst_cmp_ne_data_is_i32,
                        3 => inst_cmp_gt_data_is_i32,
                        4 => inst_cmp_lt_data_is_i32,
                        5 => inst_cmp_ge_data_is_i32,
                        6 => inst_cmp_le_data_is_i32,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                7 => {
                    match op {
                        1 => inst_cmp_eq_data_is_i64,
                        2 => inst_cmp_ne_data_is_i64,
                        3 => inst_cmp_gt_data_is_i64,
                        4 => inst_cmp_lt_data_is_i64,
                        5 => inst_cmp_ge_data_is_i64,
                        6 => inst_cmp_le_data_is_i64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                8 => {
                    match op {
                        1 => inst_cmp_eq_data_is_f32,
                        2 => inst_cmp_ne_data_is_f32,
                        3 => inst_cmp_gt_data_is_f32,
                        4 => inst_cmp_lt_data_is_f32,
                        5 => inst_cmp_ge_data_is_f32,
                        6 => inst_cmp_le_data_is_f32,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                9 => {
                    match op {
                        1 => inst_cmp_eq_data_is_f64,
                        2 => inst_cmp_ne_data_is_f64,
                        3 => inst_cmp_gt_data_is_f64,
                        4 => inst_cmp_lt_data_is_f64,
                        5 => inst_cmp_ge_data_is_f64,
                        6 => inst_cmp_le_data_is_f64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            };
            (data64, f)
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().u8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().u8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().u8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().u8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().u8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u8,
                    2 => task.r2.heap().u8,
                    3 => task.r3.heap().u8,
                    4 => task.r4.heap().u8,
                    5 => task.r5.heap().u8,
                    6 => task.r6.heap().u8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().u8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().u16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().u16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().u16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().u16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().u16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u16,
                    2 => task.r2.heap().u16,
                    3 => task.r3.heap().u16,
                    4 => task.r4.heap().u16,
                    5 => task.r5.heap().u16,
                    6 => task.r6.heap().u16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().u16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().u32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().u32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().u32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().u32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().u32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u32,
                    2 => task.r2.heap().u32,
                    3 => task.r3.heap().u32,
                    4 => task.r4.heap().u32,
                    5 => task.r5.heap().u32,
                    6 => task.r6.heap().u32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().u32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().u64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().u64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().u64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().u64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().u64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().u64,
                    2 => task.r2.heap().u64,
                    3 => task.r3.heap().u64,
                    4 => task.r4.heap().u64,
                    5 => task.r5.heap().u64,
                    6 => task.r6.heap().u64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().u64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().i8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().i8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().i8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().i8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().i8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i8,
                    2 => task.r2.heap().i8,
                    3 => task.r3.heap().i8,
                    4 => task.r4.heap().i8,
                    5 => task.r5.heap().i8,
                    6 => task.r6.heap().i8,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().i8 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().i16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().i16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().i16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().i16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().i16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i16,
                    2 => task.r2.heap().i16,
                    3 => task.r3.heap().i16,
                    4 => task.r4.heap().i16,
                    5 => task.r5.heap().i16,
                    6 => task.r6.heap().i16,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().i16 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().i32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().i32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().i32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().i32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().i32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i32,
                    2 => task.r2.heap().i32,
                    3 => task.r3.heap().i32,
                    4 => task.r4.heap().i32,
                    5 => task.r5.heap().i32,
                    6 => task.r6.heap().i32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().i32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().i64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().i64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().i64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().i64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().i64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().i64,
                    2 => task.r2.heap().i64,
                    3 => task.r3.heap().i64,
                    4 => task.r4.heap().i64,
                    5 => task.r5.heap().i64,
                    6 => task.r6.heap().i64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().i64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().f32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().f32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().f32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().f32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().f32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f32,
                    2 => task.r2.heap().f32,
                    3 => task.r3.heap().f32,
                    4 => task.r4.heap().f32,
                    5 => task.r5.heap().f32,
                    6 => task.r6.heap().f32,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().f32 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_eq_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.eq(&r2);
                task.r1.heap().f64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_lt_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.lt(&r2);
                task.r1.heap().f64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_gt_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.gt(&r2);
                task.r1.heap().f64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ne_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ne(&r2);
                task.r1.heap().f64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_le_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.le(&r2);
                task.r1.heap().f64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_cmp_ge_data_is_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            regdata: u64,
        ) {
            unsafe {
                let [r1loc, r2loc, _, _, _, _, _, _] = regdata.to_le_bytes();
                let task = &mut *task;
                let r1 = match r1loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r2 = match r2loc {
                    1 => task.r1.heap().f64,
                    2 => task.r2.heap().f64,
                    3 => task.r3.heap().f64,
                    4 => task.r4.heap().f64,
                    5 => task.r5.heap().f64,
                    6 => task.r6.heap().f64,
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                let r3 = r1.ge(&r2);
                task.r1.heap().f64 = if r3 { 1 as _ } else { 0 as _ };
            }
        }
    }
    mod ptrarith {
        use std::os::raw::c_void;
        use sart::ctr::{DispatchFn, VMTaskState};
        pub fn isize_to_u64(value: isize) -> u64 {
            return { value.cast_unsigned() as _ };
        }
        pub fn u64_to_isize(value: u64) -> isize {
            return { (value as usize).cast_signed() as _ };
        }
        pub const PTRADDOP: u8 = 0;
        pub const PTRSUBOP: u8 = 1;
        pub const PTROFFSETOP: u8 = 2;
        pub fn inst_ptrarith_handler(reg1: u8, op: u8) -> DispatchFn {
            let f = match reg1 {
                1 => {
                    match op {
                        0 => inst_ptr_addr1,
                        1 => inst_ptr_subr1,
                        2 => inst_ptr_offsetr1,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                2 => {
                    match op {
                        0 => inst_ptr_addr2,
                        1 => inst_ptr_subr2,
                        2 => inst_ptr_offsetr2,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                3 => {
                    match op {
                        0 => inst_ptr_addr3,
                        1 => inst_ptr_subr3,
                        2 => inst_ptr_offsetr3,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                4 => {
                    match op {
                        0 => inst_ptr_addr4,
                        1 => inst_ptr_subr4,
                        2 => inst_ptr_offsetr4,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                5 => {
                    match op {
                        0 => inst_ptr_addr5,
                        1 => inst_ptr_subr5,
                        2 => inst_ptr_offsetr5,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                6 => {
                    match op {
                        0 => inst_ptr_addr6,
                        1 => inst_ptr_subr6,
                        2 => inst_ptr_offsetr6,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            };
            f
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_addr1(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.heap().complex = task.r1.heap().complex.add(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_subr1(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.heap().complex = task.r1.heap().complex.sub(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_offsetr1(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.heap().complex = task
                    .r1
                    .heap()
                    .complex
                    .offset(u64_to_isize(data as _));
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_addr2(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2.heap().complex = task.r2.heap().complex.add(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_subr2(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2.heap().complex = task.r2.heap().complex.sub(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_offsetr2(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2.heap().complex = task
                    .r2
                    .heap()
                    .complex
                    .offset(u64_to_isize(data as _));
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_addr3(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3.heap().complex = task.r3.heap().complex.add(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_subr3(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3.heap().complex = task.r3.heap().complex.sub(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_offsetr3(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3.heap().complex = task
                    .r3
                    .heap()
                    .complex
                    .offset(u64_to_isize(data as _));
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_addr4(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4.heap().complex = task.r4.heap().complex.add(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_subr4(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4.heap().complex = task.r4.heap().complex.sub(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_offsetr4(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4.heap().complex = task
                    .r4
                    .heap()
                    .complex
                    .offset(u64_to_isize(data as _));
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_addr5(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r5.heap().complex = task.r5.heap().complex.add(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_subr5(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r5.heap().complex = task.r5.heap().complex.sub(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_offsetr5(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r5.heap().complex = task
                    .r5
                    .heap()
                    .complex
                    .offset(u64_to_isize(data as _));
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_addr6(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r6.heap().complex = task.r6.heap().complex.add(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_subr6(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r6.heap().complex = task.r6.heap().complex.sub(data as _);
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_ptr_offsetr6(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r6.heap().complex = task
                    .r6
                    .heap()
                    .complex
                    .offset(u64_to_isize(data as _));
            }
        }
    }
    mod regmov {
        use core::ffi::c_void;
        use sart::ctr::{DispatchFn, VMTaskState};
        pub fn generate_mov(from: u8, to: u8) -> DispatchFn {
            match from {
                1 => generate_mov_from_r1(to),
                2 => generate_mov_from_r2(to),
                3 => generate_mov_from_r3(to),
                4 => generate_mov_from_r4(to),
                5 => generate_mov_from_r5(to),
                6 => generate_mov_from_r6(to),
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_super(from: u8, to: u8) -> DispatchFn {
            match from {
                1 => generate_mov_super_from_r1(to),
                2 => generate_mov_super_from_r2(to),
                3 => generate_mov_super_from_r3(to),
                4 => generate_mov_super_from_r4(to),
                5 => generate_mov_super_from_r5(to),
                6 => generate_mov_super_from_r6(to),
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_to_mov_super(from: u8, to: u8) -> DispatchFn {
            match from {
                1 => generate_mov_from_r1_to_super(to),
                2 => generate_mov_from_r2_to_super(to),
                3 => generate_mov_from_r3_to_super(to),
                4 => generate_mov_from_r4_to_super(to),
                5 => generate_mov_from_r5_to_super(to),
                6 => generate_mov_from_r6_to_super(to),
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_from_r1(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r1_to_r1,
                2 => inst_mov_from_r1_to_r2,
                3 => inst_mov_from_r1_to_r3,
                4 => inst_mov_from_r1_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_from_r1_to_super(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r1_to_super_r1,
                2 => inst_mov_from_r1_to_super_r2,
                3 => inst_mov_from_r1_to_super_r3,
                4 => inst_mov_from_r1_to_super_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_super_from_r1(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_super_r1_to_r1,
                2 => inst_mov_from_super_r1_to_r2,
                3 => inst_mov_from_super_r1_to_r3,
                4 => inst_mov_from_super_r1_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = task.r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r1_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = (&*task.super_).r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_super_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r1 = task.r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = task.r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r1_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = (&*task.super_).r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_super_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r2 = task.r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = task.r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r1_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = (&*task.super_).r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_super_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r3 = task.r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = task.r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r1_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = (&*task.super_).r1;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r1_to_super_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r4 = task.r1;
            }
        }
        pub fn generate_mov_from_r2(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r2_to_r1,
                2 => inst_mov_from_r2_to_r2,
                3 => inst_mov_from_r2_to_r3,
                4 => inst_mov_from_r2_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_from_r2_to_super(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r2_to_super_r1,
                2 => inst_mov_from_r2_to_super_r2,
                3 => inst_mov_from_r2_to_super_r3,
                4 => inst_mov_from_r2_to_super_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_super_from_r2(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_super_r2_to_r1,
                2 => inst_mov_from_super_r2_to_r2,
                3 => inst_mov_from_super_r2_to_r3,
                4 => inst_mov_from_super_r2_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = task.r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r2_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = (&*task.super_).r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_super_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r1 = task.r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = task.r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r2_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = (&*task.super_).r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_super_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r2 = task.r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = task.r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r2_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = (&*task.super_).r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_super_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r3 = task.r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = task.r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r2_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = (&*task.super_).r2;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r2_to_super_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r4 = task.r2;
            }
        }
        pub fn generate_mov_from_r3(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r3_to_r1,
                2 => inst_mov_from_r3_to_r2,
                3 => inst_mov_from_r3_to_r3,
                4 => inst_mov_from_r3_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_from_r3_to_super(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r3_to_super_r1,
                2 => inst_mov_from_r3_to_super_r2,
                3 => inst_mov_from_r3_to_super_r3,
                4 => inst_mov_from_r3_to_super_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_super_from_r3(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_super_r3_to_r1,
                2 => inst_mov_from_super_r3_to_r2,
                3 => inst_mov_from_super_r3_to_r3,
                4 => inst_mov_from_super_r3_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = task.r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r3_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = (&*task.super_).r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_super_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r1 = task.r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = task.r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r3_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = (&*task.super_).r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_super_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r2 = task.r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = task.r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r3_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = (&*task.super_).r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_super_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r3 = task.r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = task.r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r3_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = (&*task.super_).r3;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r3_to_super_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r4 = task.r3;
            }
        }
        pub fn generate_mov_from_r4(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r4_to_r1,
                2 => inst_mov_from_r4_to_r2,
                3 => inst_mov_from_r4_to_r3,
                4 => inst_mov_from_r4_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_from_r4_to_super(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_r4_to_super_r1,
                2 => inst_mov_from_r4_to_super_r2,
                3 => inst_mov_from_r4_to_super_r3,
                4 => inst_mov_from_r4_to_super_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_super_from_r4(to: u8) -> DispatchFn {
            match to {
                1 => inst_mov_from_super_r4_to_r1,
                2 => inst_mov_from_super_r4_to_r2,
                3 => inst_mov_from_super_r4_to_r3,
                4 => inst_mov_from_super_r4_to_r4,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = task.r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r4_to_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1 = (&*task.super_).r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_super_r1(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r1 = task.r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = task.r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r4_to_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r2 = (&*task.super_).r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_super_r2(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r2 = task.r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = task.r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r4_to_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r3 = (&*task.super_).r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_super_r3(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r3 = task.r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = task.r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r4_to_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r4 = (&*task.super_).r4;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r4_to_super_r4(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r4 = task.r4;
            }
        }
        pub fn generate_mov_from_r5(to: u8) -> DispatchFn {
            match to {
                5 => inst_mov_from_r5_to_r5,
                6 => inst_mov_from_r5_to_r6,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_from_r5_to_super(to: u8) -> DispatchFn {
            match to {
                5 => inst_mov_from_r5_to_super_r5,
                6 => inst_mov_from_r5_to_super_r6,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_super_from_r5(to: u8) -> DispatchFn {
            match to {
                5 => inst_mov_from_super_r5_to_r5,
                6 => inst_mov_from_super_r5_to_r6,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r5_to_r5(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r5 = task.r5;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r5_to_r5(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r5 = (&*task.super_).r5;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r5_to_super_r5(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r5 = task.r5;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r5_to_r6(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r6 = task.r5;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r5_to_r6(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r6 = (&*task.super_).r5;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r5_to_super_r6(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r6 = task.r5;
            }
        }
        pub fn generate_mov_from_r6(to: u8) -> DispatchFn {
            match to {
                5 => inst_mov_from_r6_to_r5,
                6 => inst_mov_from_r6_to_r6,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_from_r6_to_super(to: u8) -> DispatchFn {
            match to {
                5 => inst_mov_from_r6_to_super_r5,
                6 => inst_mov_from_r6_to_super_r6,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        pub fn generate_mov_super_from_r6(to: u8) -> DispatchFn {
            match to {
                5 => inst_mov_from_super_r6_to_r5,
                6 => inst_mov_from_super_r6_to_r6,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r6_to_r5(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r5 = task.r6;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r6_to_r5(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r5 = (&*task.super_).r6;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r6_to_super_r5(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r5 = task.r6;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r6_to_r6(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r6 = task.r6;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_super_r6_to_r6(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r6 = (&*task.super_).r6;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_mov_from_r6_to_super_r6(
            _: *mut c_void,
            task: *mut VMTaskState,
            _: u64,
        ) {
            unsafe {
                let task = &mut *task;
                (&mut *task.super_).r6 = task.r6;
            }
        }
    }
    mod regput {
        use sart::ctr::DispatchFn;
        use sart::ctr::VMTaskState;
        use std::ffi::c_void;
        pub fn put_reg_handler(to: u8, typ: u8) -> DispatchFn {
            match to {
                1 => {
                    match typ {
                        0 => inst_reg_put_in_r1_and_u8,
                        0 => inst_reg_put_in_r1_and_u8,
                        0 => inst_reg_put_in_r1_and_u8,
                        0 => inst_reg_put_in_r1_and_u8,
                        0 => inst_reg_put_in_r1_and_u8,
                        0 => inst_reg_put_in_r1_and_u8,
                        1 => inst_reg_put_in_r1_and_u16,
                        1 => inst_reg_put_in_r1_and_u16,
                        1 => inst_reg_put_in_r1_and_u16,
                        1 => inst_reg_put_in_r1_and_u16,
                        1 => inst_reg_put_in_r1_and_u16,
                        1 => inst_reg_put_in_r1_and_u16,
                        2 => inst_reg_put_in_r1_and_u32,
                        2 => inst_reg_put_in_r1_and_u32,
                        2 => inst_reg_put_in_r1_and_u32,
                        2 => inst_reg_put_in_r1_and_u32,
                        2 => inst_reg_put_in_r1_and_u32,
                        2 => inst_reg_put_in_r1_and_u32,
                        3 => inst_reg_put_in_r1_and_u64,
                        3 => inst_reg_put_in_r1_and_u64,
                        3 => inst_reg_put_in_r1_and_u64,
                        3 => inst_reg_put_in_r1_and_u64,
                        3 => inst_reg_put_in_r1_and_u64,
                        3 => inst_reg_put_in_r1_and_u64,
                        4 => inst_reg_put_in_r1_and_i8,
                        4 => inst_reg_put_in_r1_and_i8,
                        4 => inst_reg_put_in_r1_and_i8,
                        4 => inst_reg_put_in_r1_and_i8,
                        4 => inst_reg_put_in_r1_and_i8,
                        4 => inst_reg_put_in_r1_and_i8,
                        5 => inst_reg_put_in_r1_and_i16,
                        5 => inst_reg_put_in_r1_and_i16,
                        5 => inst_reg_put_in_r1_and_i16,
                        5 => inst_reg_put_in_r1_and_i16,
                        5 => inst_reg_put_in_r1_and_i16,
                        5 => inst_reg_put_in_r1_and_i16,
                        6 => inst_reg_put_in_r1_and_i32,
                        6 => inst_reg_put_in_r1_and_i32,
                        6 => inst_reg_put_in_r1_and_i32,
                        6 => inst_reg_put_in_r1_and_i32,
                        6 => inst_reg_put_in_r1_and_i32,
                        6 => inst_reg_put_in_r1_and_i32,
                        7 => inst_reg_put_in_r1_and_i64,
                        7 => inst_reg_put_in_r1_and_i64,
                        7 => inst_reg_put_in_r1_and_i64,
                        7 => inst_reg_put_in_r1_and_i64,
                        7 => inst_reg_put_in_r1_and_i64,
                        7 => inst_reg_put_in_r1_and_i64,
                        8 => inst_reg_put_in_r1_and_f32,
                        8 => inst_reg_put_in_r1_and_f32,
                        8 => inst_reg_put_in_r1_and_f32,
                        8 => inst_reg_put_in_r1_and_f32,
                        8 => inst_reg_put_in_r1_and_f32,
                        8 => inst_reg_put_in_r1_and_f32,
                        9 => inst_reg_put_in_r1_and_f64,
                        9 => inst_reg_put_in_r1_and_f64,
                        9 => inst_reg_put_in_r1_and_f64,
                        9 => inst_reg_put_in_r1_and_f64,
                        9 => inst_reg_put_in_r1_and_f64,
                        9 => inst_reg_put_in_r1_and_f64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                2 => {
                    match typ {
                        0 => inst_reg_put_in_r2_and_u8,
                        0 => inst_reg_put_in_r2_and_u8,
                        0 => inst_reg_put_in_r2_and_u8,
                        0 => inst_reg_put_in_r2_and_u8,
                        0 => inst_reg_put_in_r2_and_u8,
                        0 => inst_reg_put_in_r2_and_u8,
                        1 => inst_reg_put_in_r2_and_u16,
                        1 => inst_reg_put_in_r2_and_u16,
                        1 => inst_reg_put_in_r2_and_u16,
                        1 => inst_reg_put_in_r2_and_u16,
                        1 => inst_reg_put_in_r2_and_u16,
                        1 => inst_reg_put_in_r2_and_u16,
                        2 => inst_reg_put_in_r2_and_u32,
                        2 => inst_reg_put_in_r2_and_u32,
                        2 => inst_reg_put_in_r2_and_u32,
                        2 => inst_reg_put_in_r2_and_u32,
                        2 => inst_reg_put_in_r2_and_u32,
                        2 => inst_reg_put_in_r2_and_u32,
                        3 => inst_reg_put_in_r2_and_u64,
                        3 => inst_reg_put_in_r2_and_u64,
                        3 => inst_reg_put_in_r2_and_u64,
                        3 => inst_reg_put_in_r2_and_u64,
                        3 => inst_reg_put_in_r2_and_u64,
                        3 => inst_reg_put_in_r2_and_u64,
                        4 => inst_reg_put_in_r2_and_i8,
                        4 => inst_reg_put_in_r2_and_i8,
                        4 => inst_reg_put_in_r2_and_i8,
                        4 => inst_reg_put_in_r2_and_i8,
                        4 => inst_reg_put_in_r2_and_i8,
                        4 => inst_reg_put_in_r2_and_i8,
                        5 => inst_reg_put_in_r2_and_i16,
                        5 => inst_reg_put_in_r2_and_i16,
                        5 => inst_reg_put_in_r2_and_i16,
                        5 => inst_reg_put_in_r2_and_i16,
                        5 => inst_reg_put_in_r2_and_i16,
                        5 => inst_reg_put_in_r2_and_i16,
                        6 => inst_reg_put_in_r2_and_i32,
                        6 => inst_reg_put_in_r2_and_i32,
                        6 => inst_reg_put_in_r2_and_i32,
                        6 => inst_reg_put_in_r2_and_i32,
                        6 => inst_reg_put_in_r2_and_i32,
                        6 => inst_reg_put_in_r2_and_i32,
                        7 => inst_reg_put_in_r2_and_i64,
                        7 => inst_reg_put_in_r2_and_i64,
                        7 => inst_reg_put_in_r2_and_i64,
                        7 => inst_reg_put_in_r2_and_i64,
                        7 => inst_reg_put_in_r2_and_i64,
                        7 => inst_reg_put_in_r2_and_i64,
                        8 => inst_reg_put_in_r2_and_f32,
                        8 => inst_reg_put_in_r2_and_f32,
                        8 => inst_reg_put_in_r2_and_f32,
                        8 => inst_reg_put_in_r2_and_f32,
                        8 => inst_reg_put_in_r2_and_f32,
                        8 => inst_reg_put_in_r2_and_f32,
                        9 => inst_reg_put_in_r2_and_f64,
                        9 => inst_reg_put_in_r2_and_f64,
                        9 => inst_reg_put_in_r2_and_f64,
                        9 => inst_reg_put_in_r2_and_f64,
                        9 => inst_reg_put_in_r2_and_f64,
                        9 => inst_reg_put_in_r2_and_f64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                3 => {
                    match typ {
                        0 => inst_reg_put_in_r3_and_u8,
                        0 => inst_reg_put_in_r3_and_u8,
                        0 => inst_reg_put_in_r3_and_u8,
                        0 => inst_reg_put_in_r3_and_u8,
                        0 => inst_reg_put_in_r3_and_u8,
                        0 => inst_reg_put_in_r3_and_u8,
                        1 => inst_reg_put_in_r3_and_u16,
                        1 => inst_reg_put_in_r3_and_u16,
                        1 => inst_reg_put_in_r3_and_u16,
                        1 => inst_reg_put_in_r3_and_u16,
                        1 => inst_reg_put_in_r3_and_u16,
                        1 => inst_reg_put_in_r3_and_u16,
                        2 => inst_reg_put_in_r3_and_u32,
                        2 => inst_reg_put_in_r3_and_u32,
                        2 => inst_reg_put_in_r3_and_u32,
                        2 => inst_reg_put_in_r3_and_u32,
                        2 => inst_reg_put_in_r3_and_u32,
                        2 => inst_reg_put_in_r3_and_u32,
                        3 => inst_reg_put_in_r3_and_u64,
                        3 => inst_reg_put_in_r3_and_u64,
                        3 => inst_reg_put_in_r3_and_u64,
                        3 => inst_reg_put_in_r3_and_u64,
                        3 => inst_reg_put_in_r3_and_u64,
                        3 => inst_reg_put_in_r3_and_u64,
                        4 => inst_reg_put_in_r3_and_i8,
                        4 => inst_reg_put_in_r3_and_i8,
                        4 => inst_reg_put_in_r3_and_i8,
                        4 => inst_reg_put_in_r3_and_i8,
                        4 => inst_reg_put_in_r3_and_i8,
                        4 => inst_reg_put_in_r3_and_i8,
                        5 => inst_reg_put_in_r3_and_i16,
                        5 => inst_reg_put_in_r3_and_i16,
                        5 => inst_reg_put_in_r3_and_i16,
                        5 => inst_reg_put_in_r3_and_i16,
                        5 => inst_reg_put_in_r3_and_i16,
                        5 => inst_reg_put_in_r3_and_i16,
                        6 => inst_reg_put_in_r3_and_i32,
                        6 => inst_reg_put_in_r3_and_i32,
                        6 => inst_reg_put_in_r3_and_i32,
                        6 => inst_reg_put_in_r3_and_i32,
                        6 => inst_reg_put_in_r3_and_i32,
                        6 => inst_reg_put_in_r3_and_i32,
                        7 => inst_reg_put_in_r3_and_i64,
                        7 => inst_reg_put_in_r3_and_i64,
                        7 => inst_reg_put_in_r3_and_i64,
                        7 => inst_reg_put_in_r3_and_i64,
                        7 => inst_reg_put_in_r3_and_i64,
                        7 => inst_reg_put_in_r3_and_i64,
                        8 => inst_reg_put_in_r3_and_f32,
                        8 => inst_reg_put_in_r3_and_f32,
                        8 => inst_reg_put_in_r3_and_f32,
                        8 => inst_reg_put_in_r3_and_f32,
                        8 => inst_reg_put_in_r3_and_f32,
                        8 => inst_reg_put_in_r3_and_f32,
                        9 => inst_reg_put_in_r3_and_f64,
                        9 => inst_reg_put_in_r3_and_f64,
                        9 => inst_reg_put_in_r3_and_f64,
                        9 => inst_reg_put_in_r3_and_f64,
                        9 => inst_reg_put_in_r3_and_f64,
                        9 => inst_reg_put_in_r3_and_f64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                4 => {
                    match typ {
                        0 => inst_reg_put_in_r4_and_u8,
                        0 => inst_reg_put_in_r4_and_u8,
                        0 => inst_reg_put_in_r4_and_u8,
                        0 => inst_reg_put_in_r4_and_u8,
                        0 => inst_reg_put_in_r4_and_u8,
                        0 => inst_reg_put_in_r4_and_u8,
                        1 => inst_reg_put_in_r4_and_u16,
                        1 => inst_reg_put_in_r4_and_u16,
                        1 => inst_reg_put_in_r4_and_u16,
                        1 => inst_reg_put_in_r4_and_u16,
                        1 => inst_reg_put_in_r4_and_u16,
                        1 => inst_reg_put_in_r4_and_u16,
                        2 => inst_reg_put_in_r4_and_u32,
                        2 => inst_reg_put_in_r4_and_u32,
                        2 => inst_reg_put_in_r4_and_u32,
                        2 => inst_reg_put_in_r4_and_u32,
                        2 => inst_reg_put_in_r4_and_u32,
                        2 => inst_reg_put_in_r4_and_u32,
                        3 => inst_reg_put_in_r4_and_u64,
                        3 => inst_reg_put_in_r4_and_u64,
                        3 => inst_reg_put_in_r4_and_u64,
                        3 => inst_reg_put_in_r4_and_u64,
                        3 => inst_reg_put_in_r4_and_u64,
                        3 => inst_reg_put_in_r4_and_u64,
                        4 => inst_reg_put_in_r4_and_i8,
                        4 => inst_reg_put_in_r4_and_i8,
                        4 => inst_reg_put_in_r4_and_i8,
                        4 => inst_reg_put_in_r4_and_i8,
                        4 => inst_reg_put_in_r4_and_i8,
                        4 => inst_reg_put_in_r4_and_i8,
                        5 => inst_reg_put_in_r4_and_i16,
                        5 => inst_reg_put_in_r4_and_i16,
                        5 => inst_reg_put_in_r4_and_i16,
                        5 => inst_reg_put_in_r4_and_i16,
                        5 => inst_reg_put_in_r4_and_i16,
                        5 => inst_reg_put_in_r4_and_i16,
                        6 => inst_reg_put_in_r4_and_i32,
                        6 => inst_reg_put_in_r4_and_i32,
                        6 => inst_reg_put_in_r4_and_i32,
                        6 => inst_reg_put_in_r4_and_i32,
                        6 => inst_reg_put_in_r4_and_i32,
                        6 => inst_reg_put_in_r4_and_i32,
                        7 => inst_reg_put_in_r4_and_i64,
                        7 => inst_reg_put_in_r4_and_i64,
                        7 => inst_reg_put_in_r4_and_i64,
                        7 => inst_reg_put_in_r4_and_i64,
                        7 => inst_reg_put_in_r4_and_i64,
                        7 => inst_reg_put_in_r4_and_i64,
                        8 => inst_reg_put_in_r4_and_f32,
                        8 => inst_reg_put_in_r4_and_f32,
                        8 => inst_reg_put_in_r4_and_f32,
                        8 => inst_reg_put_in_r4_and_f32,
                        8 => inst_reg_put_in_r4_and_f32,
                        8 => inst_reg_put_in_r4_and_f32,
                        9 => inst_reg_put_in_r4_and_f64,
                        9 => inst_reg_put_in_r4_and_f64,
                        9 => inst_reg_put_in_r4_and_f64,
                        9 => inst_reg_put_in_r4_and_f64,
                        9 => inst_reg_put_in_r4_and_f64,
                        9 => inst_reg_put_in_r4_and_f64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                5 => {
                    match typ {
                        0 => inst_reg_put_in_r5_and_u8,
                        0 => inst_reg_put_in_r5_and_u8,
                        0 => inst_reg_put_in_r5_and_u8,
                        0 => inst_reg_put_in_r5_and_u8,
                        0 => inst_reg_put_in_r5_and_u8,
                        0 => inst_reg_put_in_r5_and_u8,
                        1 => inst_reg_put_in_r5_and_u16,
                        1 => inst_reg_put_in_r5_and_u16,
                        1 => inst_reg_put_in_r5_and_u16,
                        1 => inst_reg_put_in_r5_and_u16,
                        1 => inst_reg_put_in_r5_and_u16,
                        1 => inst_reg_put_in_r5_and_u16,
                        2 => inst_reg_put_in_r5_and_u32,
                        2 => inst_reg_put_in_r5_and_u32,
                        2 => inst_reg_put_in_r5_and_u32,
                        2 => inst_reg_put_in_r5_and_u32,
                        2 => inst_reg_put_in_r5_and_u32,
                        2 => inst_reg_put_in_r5_and_u32,
                        3 => inst_reg_put_in_r5_and_u64,
                        3 => inst_reg_put_in_r5_and_u64,
                        3 => inst_reg_put_in_r5_and_u64,
                        3 => inst_reg_put_in_r5_and_u64,
                        3 => inst_reg_put_in_r5_and_u64,
                        3 => inst_reg_put_in_r5_and_u64,
                        4 => inst_reg_put_in_r5_and_i8,
                        4 => inst_reg_put_in_r5_and_i8,
                        4 => inst_reg_put_in_r5_and_i8,
                        4 => inst_reg_put_in_r5_and_i8,
                        4 => inst_reg_put_in_r5_and_i8,
                        4 => inst_reg_put_in_r5_and_i8,
                        5 => inst_reg_put_in_r5_and_i16,
                        5 => inst_reg_put_in_r5_and_i16,
                        5 => inst_reg_put_in_r5_and_i16,
                        5 => inst_reg_put_in_r5_and_i16,
                        5 => inst_reg_put_in_r5_and_i16,
                        5 => inst_reg_put_in_r5_and_i16,
                        6 => inst_reg_put_in_r5_and_i32,
                        6 => inst_reg_put_in_r5_and_i32,
                        6 => inst_reg_put_in_r5_and_i32,
                        6 => inst_reg_put_in_r5_and_i32,
                        6 => inst_reg_put_in_r5_and_i32,
                        6 => inst_reg_put_in_r5_and_i32,
                        7 => inst_reg_put_in_r5_and_i64,
                        7 => inst_reg_put_in_r5_and_i64,
                        7 => inst_reg_put_in_r5_and_i64,
                        7 => inst_reg_put_in_r5_and_i64,
                        7 => inst_reg_put_in_r5_and_i64,
                        7 => inst_reg_put_in_r5_and_i64,
                        8 => inst_reg_put_in_r5_and_f32,
                        8 => inst_reg_put_in_r5_and_f32,
                        8 => inst_reg_put_in_r5_and_f32,
                        8 => inst_reg_put_in_r5_and_f32,
                        8 => inst_reg_put_in_r5_and_f32,
                        8 => inst_reg_put_in_r5_and_f32,
                        9 => inst_reg_put_in_r5_and_f64,
                        9 => inst_reg_put_in_r5_and_f64,
                        9 => inst_reg_put_in_r5_and_f64,
                        9 => inst_reg_put_in_r5_and_f64,
                        9 => inst_reg_put_in_r5_and_f64,
                        9 => inst_reg_put_in_r5_and_f64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                6 => {
                    match typ {
                        0 => inst_reg_put_in_r6_and_u8,
                        0 => inst_reg_put_in_r6_and_u8,
                        0 => inst_reg_put_in_r6_and_u8,
                        0 => inst_reg_put_in_r6_and_u8,
                        0 => inst_reg_put_in_r6_and_u8,
                        0 => inst_reg_put_in_r6_and_u8,
                        1 => inst_reg_put_in_r6_and_u16,
                        1 => inst_reg_put_in_r6_and_u16,
                        1 => inst_reg_put_in_r6_and_u16,
                        1 => inst_reg_put_in_r6_and_u16,
                        1 => inst_reg_put_in_r6_and_u16,
                        1 => inst_reg_put_in_r6_and_u16,
                        2 => inst_reg_put_in_r6_and_u32,
                        2 => inst_reg_put_in_r6_and_u32,
                        2 => inst_reg_put_in_r6_and_u32,
                        2 => inst_reg_put_in_r6_and_u32,
                        2 => inst_reg_put_in_r6_and_u32,
                        2 => inst_reg_put_in_r6_and_u32,
                        3 => inst_reg_put_in_r6_and_u64,
                        3 => inst_reg_put_in_r6_and_u64,
                        3 => inst_reg_put_in_r6_and_u64,
                        3 => inst_reg_put_in_r6_and_u64,
                        3 => inst_reg_put_in_r6_and_u64,
                        3 => inst_reg_put_in_r6_and_u64,
                        4 => inst_reg_put_in_r6_and_i8,
                        4 => inst_reg_put_in_r6_and_i8,
                        4 => inst_reg_put_in_r6_and_i8,
                        4 => inst_reg_put_in_r6_and_i8,
                        4 => inst_reg_put_in_r6_and_i8,
                        4 => inst_reg_put_in_r6_and_i8,
                        5 => inst_reg_put_in_r6_and_i16,
                        5 => inst_reg_put_in_r6_and_i16,
                        5 => inst_reg_put_in_r6_and_i16,
                        5 => inst_reg_put_in_r6_and_i16,
                        5 => inst_reg_put_in_r6_and_i16,
                        5 => inst_reg_put_in_r6_and_i16,
                        6 => inst_reg_put_in_r6_and_i32,
                        6 => inst_reg_put_in_r6_and_i32,
                        6 => inst_reg_put_in_r6_and_i32,
                        6 => inst_reg_put_in_r6_and_i32,
                        6 => inst_reg_put_in_r6_and_i32,
                        6 => inst_reg_put_in_r6_and_i32,
                        7 => inst_reg_put_in_r6_and_i64,
                        7 => inst_reg_put_in_r6_and_i64,
                        7 => inst_reg_put_in_r6_and_i64,
                        7 => inst_reg_put_in_r6_and_i64,
                        7 => inst_reg_put_in_r6_and_i64,
                        7 => inst_reg_put_in_r6_and_i64,
                        8 => inst_reg_put_in_r6_and_f32,
                        8 => inst_reg_put_in_r6_and_f32,
                        8 => inst_reg_put_in_r6_and_f32,
                        8 => inst_reg_put_in_r6_and_f32,
                        8 => inst_reg_put_in_r6_and_f32,
                        8 => inst_reg_put_in_r6_and_f32,
                        9 => inst_reg_put_in_r6_and_f64,
                        9 => inst_reg_put_in_r6_and_f64,
                        9 => inst_reg_put_in_r6_and_f64,
                        9 => inst_reg_put_in_r6_and_f64,
                        9 => inst_reg_put_in_r6_and_f64,
                        9 => inst_reg_put_in_r6_and_f64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u8 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u8 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u8 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u8 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u8 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_u8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u8 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u16 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u16 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u16 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u16 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u16 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_u16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u16 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u32 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u32 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u32 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u32 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u32 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_u32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u32 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u64 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u64 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u64 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u64 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u64 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_u64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.u64 = data as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i8 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i8 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i8 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i8 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i8 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_i8(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i8 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i16 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i16 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i16 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i16 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i16 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_i16(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i16 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i32 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i32 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i32 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i32 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i32 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_i32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i32 = i64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i64 = i64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i64 = i64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i64 = i64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i64 = i64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i64 = i64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_i64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.i64 = i64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f32 = f64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f32 = f64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f32 = f64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f32 = f64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f32 = f64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_f32(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f32 = f64::from_le_bytes(data.to_le_bytes()) as _;
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r1_and_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f64 = f64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r2_and_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f64 = f64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r3_and_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f64 = f64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r4_and_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f64 = f64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r5_and_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f64 = f64::from_le_bytes(data.to_le_bytes());
            }
        }
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn inst_reg_put_in_r6_and_f64(
            _: *mut c_void,
            task: *mut VMTaskState,
            data: u64,
        ) {
            unsafe {
                let task = &mut *task;
                task.r1.f64 = f64::from_le_bytes(data.to_le_bytes());
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
    pub use ptrarith::*;
    pub use regmov::*;
    pub use regput::*;
    pub use threading::*;
    use crate::{BytecodeResolver, CVM, VM, sync::heaps::SYNC_HEAP};
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
            if register.heap().u8.eq(&(0 as _)) {
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
            if register.heap().u8.ne(&(0 as _)) {
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
            if register.heap().u16.eq(&(0 as _)) {
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
            if register.heap().u16.ne(&(0 as _)) {
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
            if register.heap().u32.eq(&(0 as _)) {
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
            if register.heap().u32.ne(&(0 as _)) {
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
            if register.heap().u64.eq(&(0 as _)) {
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
            if register.heap().u64.ne(&(0 as _)) {
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
            if register.heap().i8.eq(&(0 as _)) {
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
            if register.heap().i8.ne(&(0 as _)) {
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
            if register.heap().i16.eq(&(0 as _)) {
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
            if register.heap().i16.ne(&(0 as _)) {
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
            if register.heap().i32.eq(&(0 as _)) {
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
            if register.heap().i32.ne(&(0 as _)) {
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
            if register.heap().i64.eq(&(0 as _)) {
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
            if register.heap().i64.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_f32_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().f32.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_f32_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().f32.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jz_f64_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().f64.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    pub extern "C" fn inst_jnz_f64_register_r1(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = (&mut *task);
            let register = &mut task.r1;
            if register.heap().f64.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_u8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u8);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_u8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u8);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_u16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u16);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_u16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u16);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_u32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u32);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_u32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u32);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_u64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u64);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_u64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const u64);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_i8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i8);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_i8_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i8);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_i16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i16);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_i16_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i16);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_i32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i32);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_i32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i32);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_i64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i64);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_i64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const i64);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_f32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const f32);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_f32_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const f32);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jz_ptr_f64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const f64);
            if data.eq(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    #[inline(never)]
    #[unsafe(link_section = ".vm_fast_instructions")]
    /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
    /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
    pub extern "C" fn inst_jnz_ptr_f64_register_r6(
        _: *mut c_void,
        task: *mut VMTaskState,
        index: u64,
    ) {
        unsafe {
            let task = &mut *task;
            let data = *(task.r6.ptr as *const f64);
            if data.ne(&(0 as _)) {
                task.curline = index as usize;
            }
        }
    }
    pub fn jz_map(ty: u8) -> DispatchFn {
        match ty {
            1 => inst_jz_u8_register_r1,
            2 => inst_jz_u16_register_r1,
            3 => inst_jz_u32_register_r1,
            4 => inst_jz_u64_register_r1,
            5 => inst_jz_i8_register_r1,
            6 => inst_jz_i16_register_r1,
            7 => inst_jz_i32_register_r1,
            8 => inst_jz_i64_register_r1,
            9 => inst_jz_f32_register_r1,
            10 => inst_jz_f64_register_r1,
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        }
    }
    pub fn jnz_map(ty: u8) -> DispatchFn {
        match ty {
            1 => inst_jnz_u8_register_r1,
            2 => inst_jnz_u16_register_r1,
            3 => inst_jnz_u32_register_r1,
            4 => inst_jnz_u64_register_r1,
            5 => inst_jnz_i8_register_r1,
            6 => inst_jnz_i16_register_r1,
            7 => inst_jnz_i32_register_r1,
            8 => inst_jnz_i64_register_r1,
            9 => inst_jnz_f32_register_r1,
            10 => inst_jnz_f64_register_r1,
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        }
    }
    pub fn jz_ptr_map(ty: u8) -> DispatchFn {
        match ty {
            1 => inst_jz_ptr_u8_register_r6,
            2 => inst_jz_ptr_u16_register_r6,
            3 => inst_jz_ptr_u32_register_r6,
            4 => inst_jz_ptr_u64_register_r6,
            5 => inst_jz_ptr_i8_register_r6,
            6 => inst_jz_ptr_i16_register_r6,
            7 => inst_jz_ptr_i32_register_r6,
            8 => inst_jz_ptr_i64_register_r6,
            9 => inst_jz_ptr_f32_register_r6,
            10 => inst_jz_ptr_f64_register_r6,
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
        }
    }
    pub fn jnz_ptr_map(ty: u8) -> DispatchFn {
        match ty {
            1 => inst_jnz_ptr_u8_register_r6,
            2 => inst_jnz_ptr_u16_register_r6,
            3 => inst_jnz_ptr_u32_register_r6,
            4 => inst_jnz_ptr_u64_register_r6,
            5 => inst_jnz_ptr_i8_register_r6,
            6 => inst_jnz_ptr_i16_register_r6,
            7 => inst_jnz_ptr_i32_register_r6,
            8 => inst_jnz_ptr_i64_register_r6,
            9 => inst_jnz_ptr_f32_register_r6,
            10 => inst_jnz_ptr_f64_register_r6,
            _ => ::core::panicking::panic("internal error: entered unreachable code"),
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
            _ = replace(&mut vm.heapmap, SYNC_HEAP.with(|x| x.as_mut_unchecked().get()));
            vm.counter += 1;
            if vm.counter > 10 {
                let mut state = Box::new(zeroed::<VMTaskState>());
                state.super_ = task as _;
                vm.run_module(state.as_mut(), u);
                drop(state);
                vm.counter -= 1;
                _ = replace(
                    &mut vm.heapmap,
                    SYNC_HEAP.with(|x| x.as_mut_unchecked().collect()),
                );
                return;
            }
            let mut state = zeroed::<VMTaskState>();
            state.super_ = task as _;
            vm.run_module(&mut state, u);
            drop(state);
            _ = replace(
                &mut vm.heapmap,
                SYNC_HEAP.with(|x| x.as_mut_unchecked().collect()),
            );
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
            let task = &mut *task;
            _ = replace(&mut vm.heapmap, SYNC_HEAP.with(|x| x.as_mut_unchecked().get()));
            let new_task: VMTaskState = zeroed();
            let old_task = Box::into_raw(Box::new(replace(task, new_task)));
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
            let task = &mut *task;
            _ = replace(
                &mut vm.heapmap,
                SYNC_HEAP.with(|x| x.as_mut_unchecked().collect()),
            );
            let old_task_moved_from_heap = *Box::from_raw(task.super_);
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
