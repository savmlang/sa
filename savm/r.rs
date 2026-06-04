#![feature(prelude_import)]
extern crate std;
#[prelude_import]
use std::prelude::rust_2024::*;
pub mod acaot {
    //! # SaVM ACAoT
    //!
    //! Adaptive Cached Ahead-of-Time (and Just-in-Time) Compiler
    //!
    //! ACAoT is a compiler & Optimizer collection aimed at empowering codebase
    //! with static deterministic optimization.
    //!
    //! ACAoT has the [pickle] subsystem to convert Sa Bytecode to its own Pickle format
    //! (which is used by chocolate interpreter) and compilers like Crafter [cranelift]
    //! and Crater [llvm-sys] converts that to reality for SaVMJIT Tiers like Crafter, Crater, Epicenter, Epitome.
    //!
    //!
    //! # Meet ACAoT
    //! The compiler toolchain backend for SaVM
    //! Revolutionalize compilation, featuring IR Generation
    //! - LLVM IR
    //! - Cranelift IR
    //!
    //! and Bytecode Parsing
    //! - Pickle IR
    //!
    //! Powering Chocolate, Crafter and Crater!
    //!
    //! ## Meet the project
    //! Designed for years, written in days!
    use serde::{Deserialize, Serialize};
    pub mod pickle {
        use std::{collections::HashMap, io::{Read, Seek}};
        use ahash::HashSet;
        use sart::ctr::*;
        use crate::acaot::pickle::def::{PickleInstruction, *};
        pub mod def {
            use crate::acaot::pickle::implementation::*;
            /// Pickle is our own internal NE implementation
            /// for converting variable width bytecode into pickle
            #[repr(C)]
            pub struct PickleInstruction {
                pub opcode: u8,
                pub u1: u8,
                pub u2: u8,
                pub u3: u8,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for PickleInstruction {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_struct_field4_finish(
                        f,
                        "PickleInstruction",
                        "opcode",
                        &self.opcode,
                        "u1",
                        &self.u1,
                        "u2",
                        &self.u2,
                        "u3",
                        &&self.u3,
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for PickleInstruction {}
            #[automatically_derived]
            impl ::core::clone::Clone for PickleInstruction {
                #[inline]
                fn clone(&self) -> PickleInstruction {
                    let _: ::core::clone::AssertParamIsClone<u8>;
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for PickleInstruction {}
            pub const PICKLE_OPCODE_HINT: u8 = 0;
            pub const PICKLE_OPCODE_WS_PUT: u8 = 1;
            pub const PICKLE_OPCODE_MOV: u8 = 2;
            pub const PICKLE_OPCODE_REG: u8 = 3;
            pub const PICKLE_OPCODE_MARK: u8 = 4;
            pub const PICKLE_OPCODE_JMP: u8 = 5;
            pub const PICKLE_OPCODE_JIF: u8 = 6;
            pub const PICKLE_OPCODE_VCMP: u8 = 7;
            pub const PICKLE_OPCODE_SCRATCH: u8 = 8;
            pub const PICKLE_OPCODE_VCOPY: u8 = 9;
            pub const PICKLE_OPCODE_VADD: u8 = 10;
            pub const PICKLE_OPCODE_VADDF: u8 = 11;
            pub const PICKLE_OPCODE_VSUB: u8 = 12;
            pub const PICKLE_OPCODE_VSUBF: u8 = 13;
            pub const PICKLE_OPCODE_VMUL: u8 = 14;
            pub const PICKLE_OPCODE_VMULF: u8 = 15;
            pub const PICKLE_OPCODE_VDIVF: u8 = 16;
            pub const PICKLE_OPCODE_DIV: u8 = 17;
            pub const PICKLE_OPCODE_REM: u8 = 18;
            pub const PICKLE_OPCODE_CAST: u8 = 19;
            pub const PICKLE_OPCODE_VNEG: u8 = 20;
            pub const PICKLE_OPCODE_VABS: u8 = 21;
            pub const PICKLE_OPCODE_VFOP: u8 = 22;
            pub const PICKLE_OPCODE_VFCAST: u8 = 23;
            pub const PICKLE_OPCODE_VBIT: u8 = 24;
            pub const PICKLE_OPCODE_VROT: u8 = 25;
            pub const PICKLE_OPCODE_VSH: u8 = 26;
            pub const PICKLE_OPCODE_VCNT: u8 = 27;
            pub const PICKLE_OPCODE_VMINIMAX: u8 = 28;
            pub const PICKLE_OPCODE_VFMA: u8 = 29;
            pub const PICKLE_OPCODE_SYNCCALL: u8 = 30;
            pub const PICKLE_OPCODE_ASYNCCALL: u8 = 31;
            pub const PICKLE_OPCODE_SPAWN: u8 = 32;
            pub const PICKLE_OPCODE_TASK: u8 = 33;
            pub const PICKLE_OPCODE_ATOMIC: u8 = 34;
            const TOTAL_ITEMS: usize = data("HINT") + data("WS_PUT") + data("MOV")
                + data("REG") + data("MARK") + data("JMP") + data("JIF") + data("VCMP")
                + data("SCRATCH") + data("VCOPY") + data("VADD") + data("VADDF")
                + data("VSUB") + data("VSUBF") + data("VMUL") + data("VMULF")
                + data("VDIVF") + data("DIV") + data("REM") + data("CAST") + data("VNEG")
                + data("VABS") + data("VFOP") + data("VFCAST") + data("VBIT")
                + data("VROT") + data("VSH") + data("VCNT") + data("VMINIMAX")
                + data("VFMA") + data("SYNCCALL") + data("ASYNCCALL") + data("SPAWN")
                + data("TASK") + data("ATOMIC") + 0;
            const fn data(_: &str) -> usize {
                1
            }
            pub(crate) const PICKLE_DISPATCH_TABLE: [ResolveFn; TOTAL_ITEMS] = [
                call_hint,
                call_ws_put,
                call_mov,
                call_reg,
                call_mark,
                call_jmp,
                call_jif,
                call_vcmp,
                call_scratch,
                call_vcopy,
                call_vadd,
                call_vaddf,
                call_vsub,
                call_vsubf,
                call_vmul,
                call_vmulf,
                call_vdivf,
                call_div,
                call_rem,
                call_cast,
                call_vneg,
                call_vabs,
                call_vfop,
                call_vfcast,
                call_vbit,
                call_vrot,
                call_vsh,
                call_vcnt,
                call_vminimax,
                call_vfma,
                call_synccall,
                call_asynccall,
                call_spawn,
                call_task,
                call_atomic,
            ];
        }
        pub mod reader {
            use std::sync::atomic::Ordering;
            use crate::acaot::pickle::def::PickleInstruction;
            pub mod au {
                use crate::{acaot::pickle::def::PickleInstruction, wspickle};
                pub struct DIVLIKE {
                    pub datatype: u8,
                    pub src1: u8,
                    pub of_src1: i32,
                    pub src2: u8,
                    pub of_src2: i32,
                    pub tgt: u8,
                    pub of_tgt: i32,
                }
                pub fn parse_divlike(pickle: &PickleInstruction, ws: &[u8]) -> DIVLIKE {
                    let args = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                    let typ = (args >> 12) as u8;
                    let of_src1 = <i32>::from_ne_bytes(ws[0..4].try_into().unwrap());
                    let of_src2 = <i32>::from_ne_bytes(ws[4..8].try_into().unwrap());
                    let of_tgt = <i32>::from_ne_bytes(ws[8..12].try_into().unwrap());
                    let src1 = {
                        let src = (args >> 8) & 0x0F;
                        src as u8
                    };
                    let src2 = {
                        let src = (args as u8) >> 4;
                        src as u8
                    };
                    let tgt = {
                        let src = (args as u8) & 0x0F;
                        src as u8
                    };
                    DIVLIKE {
                        datatype: typ,
                        src1,
                        src2,
                        tgt,
                        of_src1,
                        of_src2,
                        of_tgt,
                    }
                }
                pub struct ARITH {
                    pub datatype: u8,
                    pub count: u32,
                    pub instdefined: u16,
                    pub src1: u8,
                    pub of_src1: i32,
                    pub src2: u8,
                    pub of_src2: i32,
                    pub tgt: u8,
                    pub of_tgt: i32,
                }
                pub fn parse_arith(ws: &[u8]) -> ARITH {
                    let flags = <u32>::from_ne_bytes(ws[0..4].try_into().unwrap());
                    let instdefined = flags as u16;
                    let topflags = (flags >> 16) as u16;
                    let datatype = (topflags >> 12) as u8;
                    let count = <u32>::from_ne_bytes(ws[4..8].try_into().unwrap());
                    let of_src1 = <i32>::from_ne_bytes(ws[8..12].try_into().unwrap());
                    let of_src2 = <i32>::from_ne_bytes(ws[12..16].try_into().unwrap());
                    let of_tgt = <i32>::from_ne_bytes(ws[16..20].try_into().unwrap());
                    let src1 = ((topflags >> 8) & 0x0F) as u8;
                    let src2 = (topflags as u8) >> 4;
                    let tgt = (topflags as u8) & 0x0F;
                    ARITH {
                        datatype,
                        count,
                        instdefined,
                        src1,
                        of_src1,
                        src2,
                        of_src2,
                        tgt,
                        of_tgt,
                    }
                }
            }
            pub mod cast {
                use crate::{acaot::pickle::def::PickleInstruction, wspickle};
                pub struct CAST {
                    pub offset_src: i32,
                    pub offset_target: i32,
                    pub src: u8,
                    pub target: u8,
                    pub type_initial: u8,
                    pub type_final: u8,
                }
                pub struct VFCAST {
                    pub offset_src: i32,
                    pub offset_target: i32,
                    pub count: u32,
                    pub src: u8,
                    pub target: u8,
                    pub type_initial: u8,
                    pub type_final: u8,
                }
                #[inline(always)]
                pub fn parse_cast(pickle: &PickleInstruction, ws: &[u8]) -> CAST {
                    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                    let offset_src = <i32>::from_ne_bytes(ws[0..4].try_into().unwrap());
                    let offset_target = <i32>::from_ne_bytes(
                        ws[4..8].try_into().unwrap(),
                    );
                    let src = (flags as u8) >> 4;
                    let target = (flags as u8) & 0x0F;
                    let type_initial = (flags >> 12) as u8;
                    let type_final = ((flags >> 8) as u8) & 0x0F;
                    CAST {
                        offset_src,
                        offset_target,
                        src,
                        target,
                        type_initial,
                        type_final,
                    }
                }
                #[inline(always)]
                pub fn parse_vfcast(pickle: &PickleInstruction, ws: &[u8]) -> VFCAST {
                    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                    let count = <u32>::from_ne_bytes(ws[0..4].try_into().unwrap());
                    let offset_src = <i32>::from_ne_bytes(ws[4..8].try_into().unwrap());
                    let offset_target = <i32>::from_ne_bytes(
                        ws[8..12].try_into().unwrap(),
                    );
                    let src = (flags as u8) >> 4;
                    let target = (flags as u8) & 0x0F;
                    let type_int = (flags >> 8) as u8 & 0x03;
                    let type_float = match (flags >> 9) as u8 & 0x01 {
                        0 => 8,
                        1 => 9,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    };
                    let (type_initial, type_final) = match (flags >> 9) as u8 & 0x01 {
                        0 => (type_float, type_int),
                        1 => (type_int, type_float),
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    };
                    VFCAST {
                        offset_src,
                        offset_target,
                        count,
                        src,
                        target,
                        type_initial,
                        type_final,
                    }
                }
            }
            pub mod corevm {
                use std::{mem::offset_of, ptr::{self, null_mut}};
                use sart::{ctr::VMTaskState, structures::QuadPackedData};
                use crate::{
                    acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
                    wspickle,
                };
                pub struct VCOPY {
                    pub src: u8,
                    pub target: u8,
                    pub count: Count,
                    pub src_offset: i32,
                    pub target_offset: i32,
                    pub volatile: bool,
                    pub overlapping: bool,
                    pub src_align: u8,
                    pub target_align: u8,
                }
                #[automatically_derived]
                #[doc(hidden)]
                unsafe impl ::core::clone::TrivialClone for VCOPY {}
                #[automatically_derived]
                impl ::core::clone::Clone for VCOPY {
                    #[inline]
                    fn clone(&self) -> VCOPY {
                        let _: ::core::clone::AssertParamIsClone<u8>;
                        let _: ::core::clone::AssertParamIsClone<Count>;
                        let _: ::core::clone::AssertParamIsClone<i32>;
                        let _: ::core::clone::AssertParamIsClone<bool>;
                        *self
                    }
                }
                #[automatically_derived]
                impl ::core::marker::Copy for VCOPY {}
                pub enum Count {
                    Runtime,
                    Abs(u32),
                }
                #[automatically_derived]
                #[doc(hidden)]
                unsafe impl ::core::clone::TrivialClone for Count {}
                #[automatically_derived]
                impl ::core::clone::Clone for Count {
                    #[inline]
                    fn clone(&self) -> Count {
                        let _: ::core::clone::AssertParamIsClone<u32>;
                        *self
                    }
                }
                #[automatically_derived]
                impl ::core::marker::Copy for Count {}
                impl Count {
                    pub fn is_runtime(self) -> bool {
                        #[allow(non_exhaustive_omitted_patterns)]
                        match self {
                            Self::Runtime => true,
                            _ => false,
                        }
                    }
                    pub fn get(self, ts: *mut VMTaskState) -> u32 {
                        match self {
                            Self::Runtime => {
                                unsafe {
                                    ptr::read(
                                        ts.add(const { builtin # offset_of(VMTaskState, r1) })
                                            as *mut u32,
                                    )
                                }
                            }
                            Self::Abs(count) => count,
                        }
                    }
                }
                fn alignment(flags: u8) -> u8 {
                    match flags {
                        0 => 1,
                        1 => 16,
                        2 => 32,
                        3 => 64,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                pub fn parse_vcopy(pickle: &PickleInstruction, ws: &[u8]) -> VCOPY {
                    let memflags = pickle.u1;
                    let srcflags = pickle.u2;
                    let memory_flags = memflags & 0x7F;
                    let target_align = alignment(memory_flags & 0x03);
                    let src_align = alignment((memory_flags >> 2) & 0x03);
                    let overlapping = (memory_flags & 0x10) == 0;
                    let volatile = (memory_flags & (1 << 5)) != 0;
                    let countbit = memflags & 0x80;
                    let src = srcflags >> 4;
                    let target = srcflags & 0x0F;
                    let count = if countbit > 0 {
                        Count::Runtime
                    } else {
                        Count::Abs(<u32>::from_ne_bytes(ws[0..4].try_into().unwrap()))
                    };
                    let src_offset = <i32>::from_ne_bytes(ws[4..8].try_into().unwrap());
                    let target_offset = <i32>::from_ne_bytes(
                        ws[8..12].try_into().unwrap(),
                    );
                    VCOPY {
                        src,
                        target,
                        count,
                        src_offset,
                        target_offset,
                        src_align,
                        target_align,
                        overlapping,
                        volatile,
                    }
                }
                pub enum SCRATCH {
                    Allocate { size_reg: u8, align_reg: u8 },
                    DropClassic,
                    DropAligned,
                }
                pub fn parse_scratch(pickle: &PickleInstruction, _: &[u8]) -> SCRATCH {
                    let op_class = pickle.u1;
                    let payload = u16::from_ne_bytes([pickle.u2, pickle.u3]);
                    match op_class {
                        0b00 => {
                            SCRATCH::Allocate {
                                size_reg: payload as u8 >> 4,
                                align_reg: payload as u8 & 0x0F,
                            }
                        }
                        0b01 => SCRATCH::DropClassic,
                        0b10 => SCRATCH::DropAligned,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
                pub extern "C" fn jitcall_scratch_ffi(
                    op: u8,
                    ws: *mut WorkingSet,
                    arg1: *mut QuadPackedData,
                    arg2: usize,
                ) -> *mut QuadPackedData {
                    unsafe {
                        match op {
                            0 => {
                                let size = arg1.addr();
                                let align = arg2;
                                return WorkingSet::allocate(&mut *ws, size, align);
                            }
                            1 => {
                                WorkingSet::free(&mut *ws, arg1);
                            }
                            2 => {
                                WorkingSet::salloc_free(&mut *ws, arg1);
                            }
                            _ => ::core::panicking::panic("not implemented"),
                        }
                        null_mut()
                    }
                }
            }
            pub mod fp {
                use crate::{acaot::pickle::def::PickleInstruction, wspickle};
                pub struct VFP {
                    pub instdef: u8,
                    pub count: u32,
                    pub datatype: u8,
                    pub src1: u8,
                    pub src2: u8,
                    pub tgt: u8,
                    pub of_src1: i32,
                    pub of_src2: i32,
                    pub of_tgt: i32,
                }
                pub fn parse_vfp(pickle: &PickleInstruction, meta: &[u8]) -> VFP {
                    let f1 = pickle.u1;
                    let f2 = pickle.u2;
                    let flags = u16::from_ne_bytes([f1, f2]);
                    let fptype = ((flags >> 12) & 0x01) as u8;
                    let datatype = match fptype {
                        0 => 8,
                        1 => 9,
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    };
                    let instdef = ((flags >> 14) & 0x01) as u8;
                    let count = <u32>::from_ne_bytes(meta[0..4].try_into().unwrap());
                    let of_src1 = <i32>::from_ne_bytes(meta[4..8].try_into().unwrap());
                    let of_src2 = <i32>::from_ne_bytes(meta[8..12].try_into().unwrap());
                    let of_tgt = <i32>::from_ne_bytes(meta[12..16].try_into().unwrap());
                    let src1 = {
                        let src = (flags >> 8 as u8) & 0x0F;
                        src as u8
                    };
                    let src2 = {
                        let src = (flags as u8) >> 4;
                        src as u8
                    };
                    let tgt = {
                        let src = (flags as u8) & 0x0F;
                        src as u8
                    };
                    VFP {
                        instdef,
                        count,
                        datatype,
                        src1,
                        src2,
                        tgt,
                        of_src1,
                        of_src2,
                        of_tgt,
                    }
                }
            }
            pub mod spawn {
                use crate::{acaot::pickle::def::PickleInstruction, wspickle};
                pub struct SPAWN {
                    pub section: u64,
                    pub launch_as_async: bool,
                    pub return_hwnd: bool,
                    pub out_loc: u8,
                }
                pub fn parse_spawn(pickle: &PickleInstruction, ws: &[u8]) -> SPAWN {
                    let section = <u64>::from_ne_bytes(ws[0..8].try_into().unwrap());
                    let hwnd = (pickle.u1 & 0x1) > 0;
                    let launch_async = (pickle.u1 & 0x4) > 0;
                    let taskout = (pickle.u1 >> 2) & 0xF;
                    SPAWN {
                        section,
                        launch_as_async: launch_async,
                        out_loc: taskout,
                        return_hwnd: hwnd,
                    }
                }
            }
            pub mod vfop {
                use crate::{acaot::pickle::def::PickleInstruction, constdef, wspickle};
                pub struct VFOP {
                    pub src: u8,
                    pub target: u8,
                    pub subop: u8,
                    pub offset_src: i32,
                    pub offset_target: i32,
                    pub count: u32,
                    pub typetag: u8,
                }
                pub const FOP_CEIL: u8 = 0;
                pub const FOP_FLOOR: u8 = 1;
                pub const FOP_TRUNC: u8 = 2;
                pub const FOP_ROUND: u8 = 3;
                pub const FOP_SQRT: u8 = 4;
                pub fn parse_vfop(pickle: &PickleInstruction, ws: &[u8]) -> VFOP {
                    let flags = u16::from_le_bytes([pickle.u1, pickle.u2]);
                    let count = <u32>::from_ne_bytes(ws[0..4].try_into().unwrap());
                    let offset_src = <i32>::from_ne_bytes(ws[4..8].try_into().unwrap());
                    let offset_target = <i32>::from_ne_bytes(
                        ws[8..12].try_into().unwrap(),
                    );
                    let subop = (flags as u8) & 0x7;
                    let target = (flags as u8) >> 4;
                    let src = (flags >> 8) as u8 & 0xF;
                    let float_type = match ((flags >> 3) as u8) & 0x1 {
                        0 => 8,
                        1 => 9,
                        _ => ::core::panicking::panic("not implemented"),
                    };
                    VFOP {
                        src,
                        target,
                        offset_src,
                        offset_target,
                        count,
                        typetag: float_type,
                        subop,
                    }
                }
            }
            pub mod vminimax {
                use crate::{acaot::pickle::def::PickleInstruction, wspickle};
                pub struct VMINIMAX {
                    pub op: u8,
                    pub flags_src1: u8,
                    pub flags_src2: u8,
                    pub flags_target: u8,
                    pub count: u32,
                    pub of_src1: u8,
                    pub of_src2: u8,
                    pub of_target: u8,
                    pub typ: u8,
                    pub alignment_src1: Option<u8>,
                    pub alignment_src2: Option<u8>,
                    pub alignment_target: Option<u8>,
                }
                #[inline(always)]
                pub fn parse_vminimax(
                    pickle: &PickleInstruction,
                    ws: &[u8],
                ) -> VMINIMAX {
                    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                    let op = pickle.u3 & 0x01;
                    let typ = (flags >> 12) as u8;
                    let align_target = pickle.u3 >> 1 as u8 & 0x03;
                    let align_src2 = pickle.u3 >> 3 as u8 & 0x03;
                    let align_src1 = pickle.u3 >> 5 as u8 & 0x03;
                    let align = |align: u8| match align {
                        1 => Some(16),
                        2 => Some(32),
                        3 => Some(64),
                        _ => None,
                    };
                    let count = {
                        let countdata = <u32>::from_ne_bytes(
                            ws[0..4].try_into().unwrap(),
                        );
                        countdata
                    };
                    let flags_src1 = (flags >> 8) as u8 & 0x0F;
                    let flags_src2 = (flags >> 4) as u8 & 0x0F;
                    let flags_target = flags as u8 & 0x0F;
                    let of_src1 = <u8>::from_ne_bytes(ws[4..5].try_into().unwrap());
                    let of_src2 = <u8>::from_ne_bytes(ws[5..6].try_into().unwrap());
                    let of_target = <u8>::from_ne_bytes(ws[6..7].try_into().unwrap());
                    VMINIMAX {
                        op,
                        flags_src1,
                        flags_src2,
                        flags_target,
                        count,
                        of_src1,
                        of_src2,
                        of_target,
                        typ,
                        alignment_src1: align(align_src1),
                        alignment_src2: align(align_src2),
                        alignment_target: align(align_target),
                    }
                }
                pub struct VCNT {
                    pub op: u8,
                    pub flags_src: u8,
                    pub flags_target: u8,
                    pub count: u32,
                    pub of_src: u8,
                    pub of_target: u8,
                    pub typ: u8,
                    pub alignment_src: Option<u8>,
                    pub alignment_target: Option<u8>,
                }
                #[inline(always)]
                pub fn parse_vcnt(pickle: &PickleInstruction, ws: &[u8]) -> VCNT {
                    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                    let op = (flags as u8) & 0x0F;
                    let typ = (flags >> 12) as u8 & 0x03;
                    let count = {
                        let countdata = <u32>::from_ne_bytes(
                            ws[0..4].try_into().unwrap(),
                        );
                        countdata
                    };
                    let align_target = pickle.u3 as u8 & 0x03;
                    let align_src = pickle.u3 >> 2 as u8 & 0x03;
                    let align = |align: u8| match align {
                        1 => Some(16),
                        2 => Some(32),
                        3 => Some(64),
                        _ => None,
                    };
                    let flags_src = (flags >> 8) as u8 & 0x0F;
                    let flags_target = (flags >> 4) as u8 & 0x0F;
                    let of_src = <u8>::from_ne_bytes(ws[4..5].try_into().unwrap());
                    let of_target = <u8>::from_ne_bytes(ws[5..6].try_into().unwrap());
                    VCNT {
                        op,
                        flags_src,
                        flags_target,
                        count,
                        of_src,
                        of_target,
                        typ,
                        alignment_src: align(align_src),
                        alignment_target: align(align_target),
                    }
                }
            }
            pub mod vsh {
                use crate::{acaot::pickle::def::PickleInstruction, wspickle};
                pub struct VSH {
                    pub op: u8,
                    pub flags_src1: u8,
                    pub flags_src2: u8,
                    pub flags_target: u8,
                    pub count: u32,
                    pub of_src1: i8,
                    pub of_src2: i8,
                    pub of_target: i8,
                    pub typ: u8,
                }
                #[inline(always)]
                pub fn parse_vsh(pickle: &PickleInstruction, ws: &[u8]) -> VSH {
                    let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                    let typ = (flags >> 13) as u8;
                    let op = (flags >> 12) as u8 & 0x01;
                    let count = <u32>::from_ne_bytes(ws[0..4].try_into().unwrap());
                    let flags_src1 = (flags as u8) & 0x0F;
                    let flags_src2 = (flags as u8) >> 4 & 0x0F;
                    let flags_target = (flags >> 12) as u8 & 0x0F;
                    let of_src1 = <i8>::from_ne_bytes(ws[4..5].try_into().unwrap());
                    let of_src2 = <i8>::from_ne_bytes(ws[5..6].try_into().unwrap());
                    let of_target = <i8>::from_ne_bytes(ws[6..7].try_into().unwrap());
                    VSH {
                        op,
                        flags_src1,
                        flags_src2,
                        flags_target,
                        count,
                        of_src1,
                        of_src2,
                        of_target,
                        typ,
                    }
                }
            }
            pub const ATOMIC_CAS: u8 = 0;
            pub const ATOMIC_LOAD: u8 = 1;
            pub const ATOMIC_RMW: u8 = 2;
            pub const ATOMIC_STORE: u8 = 3;
            pub enum ATOMIC {
                CAS {
                    typedata: u8,
                    ptr_loc: u8,
                    ptr_loc_of: u8,
                    val_stored_loc: u8,
                    val_store_of: u8,
                    expected_loc: u8,
                    expected_of: u8,
                    ret_loc: u8,
                    ret_of: u8,
                    ord_success: Ordering,
                    ord_failure: Ordering,
                },
                RMW {
                    typedata: u8,
                    ptr_loc: u8,
                    ptr_loc_of: u8,
                    load_loc: u8,
                    load_loc_of: u8,
                    rhs_loc: u8,
                    rhs_loc_of: u8,
                    op: ATOMICRmwOp,
                    ord: Ordering,
                },
                STORE {
                    typedata: u8,
                    ptr_loc: u8,
                    ptr_loc_of: u8,
                    val_stored_loc: u8,
                    val_store_of: u8,
                    ord: Ordering,
                },
                LOAD {
                    typedata: u8,
                    ptr_loc: u8,
                    ptr_loc_of: u8,
                    load_loc: u8,
                    load_loc_of: u8,
                    ord: Ordering,
                },
            }
            pub enum ATOMICRmwOp {
                Add,
                Sub,
                And,
                Nand,
                Or,
                Xor,
                Xchg,
                Min,
                Max,
            }
            pub fn parse_atomic(pickle: &PickleInstruction, ws: &[u8]) -> ATOMIC {
                let flags = pickle.u1;
                let ordering = flags & 0x7;
                let ordering2 = <u8>::from_ne_bytes(ws[0..1].try_into().unwrap());
                let ty = (flags >> 3) & 0x7;
                let subop = flags >> 6;
                let of_v0 = pickle.u2;
                let of_v1 = pickle.u3;
                let of_v2 = <u8>::from_ne_bytes(ws[1..2].try_into().unwrap());
                let of_v3 = <u8>::from_ne_bytes(ws[2..3].try_into().unwrap());
                let instdefined = <u16>::from_ne_bytes(ws[3..5].try_into().unwrap());
                let v0 = (instdefined as u8) & 0x0F;
                let v1 = (instdefined >> 4) as u8 & 0x0F;
                let v2 = (instdefined >> 8) as u8 & 0x0F;
                let v3 = (instdefined >> 12) as u8 & 0x0F;
                let ord = match ordering {
                    0 => Ordering::SeqCst,
                    1 => Ordering::Relaxed,
                    2 => Ordering::Acquire,
                    3 => Ordering::Release,
                    4 => Ordering::AcqRel,
                    e => {
                        ::core::panicking::panic_fmt(format_args!("Unknown {0}", e));
                    }
                };
                let ord2 = match ordering2 {
                    0 => Ordering::SeqCst,
                    1 => Ordering::Relaxed,
                    2 => Ordering::Acquire,
                    3 => Ordering::Release,
                    4 => Ordering::AcqRel,
                    e => {
                        ::core::panicking::panic_fmt(format_args!("Unknown {0}", e));
                    }
                };
                match subop {
                    ATOMIC_CAS => {
                        ATOMIC::CAS {
                            typedata: ty,
                            ptr_loc: v0,
                            ptr_loc_of: of_v0,
                            val_stored_loc: v1,
                            val_store_of: of_v1,
                            expected_loc: v2,
                            expected_of: of_v2,
                            ret_loc: v3,
                            ret_of: of_v3,
                            ord_success: ord,
                            ord_failure: ord2,
                        }
                    }
                    ATOMIC_LOAD => {
                        ATOMIC::LOAD {
                            typedata: ty,
                            ptr_loc: v0,
                            ptr_loc_of: of_v0,
                            load_loc: v1,
                            load_loc_of: of_v1,
                            ord,
                        }
                    }
                    ATOMIC_RMW => {
                        ATOMIC::RMW {
                            typedata: ty,
                            ptr_loc: v0,
                            ptr_loc_of: of_v0,
                            load_loc: v1,
                            load_loc_of: of_v1,
                            rhs_loc: v2,
                            rhs_loc_of: of_v2,
                            op: match v3 {
                                0 => ATOMICRmwOp::Add,
                                1 => ATOMICRmwOp::Sub,
                                2 => ATOMICRmwOp::And,
                                3 => ATOMICRmwOp::Nand,
                                4 => ATOMICRmwOp::Or,
                                5 => ATOMICRmwOp::Xor,
                                6 => ATOMICRmwOp::Xchg,
                                7 => ATOMICRmwOp::Min,
                                8 => ATOMICRmwOp::Max,
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
                            },
                            ord,
                        }
                    }
                    ATOMIC_STORE => {
                        ATOMIC::STORE {
                            typedata: ty,
                            ptr_loc: v0,
                            ptr_loc_of: of_v0,
                            val_stored_loc: v1,
                            val_store_of: of_v1,
                            ord,
                        }
                    }
                    _ => ::core::panicking::panic("not implemented"),
                }
            }
        }
        pub mod implementation {
            #![allow(unused_unsafe)]
            use std::{
                collections::HashMap, hint::cold_path, mem::{transmute_copy, zeroed},
                ptr::read_unaligned, sync::Arc,
            };
            mod almu {
                //! Arithmatic, Logic, Memory Unit
                use std::ptr::null_mut;
                use sart::ctr::VMTaskState;
                use crate::{
                    acaot::pickle::{
                        def::PickleInstruction, implementation::WorkingSet,
                        reader::corevm::{SCRATCH, parse_scratch},
                    },
                    resolve,
                };
                mod atomic {
                    use std::sync::atomic::{
                        AtomicI8, AtomicI16, AtomicI32, AtomicI64, AtomicU8, AtomicU16,
                        AtomicU32, AtomicU64, Ordering,
                    };
                    use sart::ctr::VMTaskState;
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::{ATOMIC, ATOMICRmwOp, parse_atomic},
                        },
                        resolve_location_src,
                    };
                    pub fn call_atomic(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let ts = taskstate;
                        match parse_atomic(pickle, unsafe { &(&(*ws).arr) }) {
                            ATOMIC::LOAD {
                                typedata,
                                ptr_loc,
                                ptr_loc_of,
                                load_loc,
                                load_loc_of,
                                ord,
                            } => {
                                let _d = &[
                                    call_load::<u64>,
                                    call_load::<u32>,
                                    call_load::<u16>,
                                    call_load::<u8>,
                                    call_load::<i64>,
                                    call_load::<i32>,
                                    call_load::<i16>,
                                    call_load::<i8>,
                                ];
                                unsafe {
                                    (_d
                                        .get_unchecked(
                                            typedata as usize,
                                        ))(ts, ord, ptr_loc, load_loc, ptr_loc_of, load_loc_of)
                                }
                            }
                            ATOMIC::STORE {
                                typedata,
                                ptr_loc,
                                ptr_loc_of,
                                val_stored_loc,
                                val_store_of,
                                ord,
                            } => {
                                let _d = &[
                                    call_store::<u64>,
                                    call_store::<u32>,
                                    call_store::<u16>,
                                    call_store::<u8>,
                                    call_store::<i64>,
                                    call_store::<i32>,
                                    call_store::<i16>,
                                    call_store::<i8>,
                                ];
                                unsafe {
                                    (_d
                                        .get_unchecked(
                                            typedata as usize,
                                        ))(
                                        ts,
                                        ord,
                                        ptr_loc,
                                        val_stored_loc,
                                        ptr_loc_of,
                                        val_store_of,
                                    )
                                }
                            }
                            ATOMIC::CAS {
                                typedata,
                                ptr_loc,
                                ptr_loc_of,
                                val_stored_loc,
                                val_store_of,
                                expected_loc,
                                expected_of,
                                ret_loc,
                                ret_of,
                                ord_success,
                                ord_failure,
                            } => {
                                let _d = &[
                                    call_cas::<u64>,
                                    call_cas::<u32>,
                                    call_cas::<u16>,
                                    call_cas::<u8>,
                                    call_cas::<i64>,
                                    call_cas::<i32>,
                                    call_cas::<i16>,
                                    call_cas::<i8>,
                                ];
                                unsafe {
                                    (_d
                                        .get_unchecked(
                                            typedata as usize,
                                        ))(
                                        ts,
                                        ptr_loc,
                                        ptr_loc_of,
                                        val_stored_loc,
                                        val_store_of,
                                        expected_loc,
                                        expected_of,
                                        ret_loc,
                                        ret_of,
                                        ord_success,
                                        ord_failure,
                                    )
                                }
                            }
                            ATOMIC::RMW {
                                typedata,
                                ptr_loc,
                                ptr_loc_of,
                                load_loc,
                                load_loc_of,
                                rhs_loc,
                                rhs_loc_of,
                                op,
                                ord,
                            } => {
                                let _d = &[
                                    call_rmw::<u64>,
                                    call_rmw::<u32>,
                                    call_rmw::<u16>,
                                    call_rmw::<u8>,
                                    call_rmw::<i64>,
                                    call_rmw::<i32>,
                                    call_rmw::<i16>,
                                    call_rmw::<i8>,
                                ];
                                unsafe {
                                    (_d
                                        .get_unchecked(
                                            typedata as usize,
                                        ))(
                                        ts,
                                        ord,
                                        ptr_loc,
                                        ptr_loc_of,
                                        load_loc,
                                        load_loc_of,
                                        rhs_loc,
                                        rhs_loc_of,
                                        op,
                                    )
                                }
                            }
                        }
                    }
                    trait Atomicable {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering);
                        fn a_load(pt: *mut Self, out: *mut Self, order: Ordering);
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        );
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        );
                    }
                    impl Atomicable for u64 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicU64>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicU64>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicU64>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicU64>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    impl Atomicable for u32 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicU32>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicU32>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicU32>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicU32>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    impl Atomicable for u16 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicU16>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicU16>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicU16>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicU16>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    impl Atomicable for u8 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicU8>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicU8>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicU8>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicU8>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    impl Atomicable for i64 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicI64>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicI64>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicI64>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicI64>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    impl Atomicable for i32 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicI32>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicI32>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicI32>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicI32>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    impl Atomicable for i16 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicI16>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicI16>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicI16>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicI16>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    impl Atomicable for i8 {
                        fn a_store(pt: *mut Self, value: Self, order: Ordering) {
                            unsafe {
                                <AtomicI8>::from_ptr(pt).store(value, order);
                            }
                        }
                        fn a_load(pt: *mut Self, ret: *mut Self, order: Ordering) {
                            unsafe {
                                *ret = <AtomicI8>::from_ptr(pt).load(order);
                            }
                        }
                        fn a_cas(
                            pt: *mut Self,
                            stored: *mut Self,
                            expected: *mut Self,
                            ret: *mut Self,
                            order1: Ordering,
                            order2: Ordering,
                        ) {
                            unsafe {
                                let stored = *stored;
                                let expected = *expected;
                                let [out, succ] = <AtomicI8>::from_ptr(pt)
                                    .compare_exchange_weak(expected, stored, order1, order2)
                                    .map_or_else(|e| [e, !0], |x| [x, 0]);
                                *ret = out;
                                *ret.add(1) = succ;
                            }
                        }
                        fn a_rmw(
                            pt: *mut Self,
                            out: *mut Self,
                            op: *mut Self,
                            rmwop: ATOMICRmwOp,
                            ord: Ordering,
                        ) {
                            unsafe {
                                let atomic = <AtomicI8>::from_ptr(pt);
                                *out = match rmwop {
                                    ATOMICRmwOp::Add => atomic.fetch_add(*op, ord),
                                    ATOMICRmwOp::Sub => atomic.fetch_sub(*op, ord),
                                    ATOMICRmwOp::And => atomic.fetch_and(*op, ord),
                                    ATOMICRmwOp::Nand => atomic.fetch_nand(*op, ord),
                                    ATOMICRmwOp::Or => atomic.fetch_or(*op, ord),
                                    ATOMICRmwOp::Xor => atomic.fetch_xor(*op, ord),
                                    ATOMICRmwOp::Xchg => atomic.swap(*op, ord),
                                    ATOMICRmwOp::Min => atomic.fetch_min(*op, ord),
                                    ATOMICRmwOp::Max => atomic.fetch_max(*op, ord),
                                };
                            }
                        }
                    }
                    #[allow(unused)]
                    fn call_store<T: Atomicable + Clone + Copy>(
                        taskstate: *mut VMTaskState,
                        ord: Ordering,
                        o1: u8,
                        o2: u8,
                        of1: u8,
                        of2: u8,
                    ) {
                        unsafe {
                            let o1 = (std::ptr::read_unaligned(
                                    unsafe {
                                        match o1 {
                                            0 => &raw mut (*taskstate).r1,
                                            1 => &raw mut (*taskstate).r2,
                                            2 => &raw mut (*taskstate).r3,
                                            3 => &raw mut (*taskstate).r4,
                                            4 => &raw mut (*taskstate).r5,
                                            5 => &raw mut (*taskstate).r6,
                                            6 => &raw mut (*taskstate).r7,
                                            7 => &raw mut (*taskstate).r8,
                                            8 => (*taskstate).scratchpad,
                                            9 => (*taskstate).largepad,
                                            #[allow(unused_unsafe)]
                                            10 => unsafe { (*taskstate).r2.selfref }
                                            #[allow(unreachable_patterns)]
                                            _ => ::core::panicking::panic("not implemented"),
                                        }
                                    }
                                        .add(of1 as _),
                                )
                                .pointer as *mut T);
                            let o2 = (unsafe {
                                match o2 {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut T)
                                .offset(of2 as _);
                            Atomicable::a_store(o1, *o2, ord)
                        };
                    }
                    #[allow(unused)]
                    fn call_load<T: Atomicable + Clone + Copy>(
                        taskstate: *mut VMTaskState,
                        ord: Ordering,
                        o1: u8,
                        o2: u8,
                        of1: u8,
                        of2: u8,
                    ) {
                        unsafe {
                            let o1 = (std::ptr::read_unaligned(
                                unsafe {
                                    match o1 {
                                        0 => &raw mut (*taskstate).r1,
                                        1 => &raw mut (*taskstate).r2,
                                        2 => &raw mut (*taskstate).r3,
                                        3 => &raw mut (*taskstate).r4,
                                        4 => &raw mut (*taskstate).r5,
                                        5 => &raw mut (*taskstate).r6,
                                        6 => &raw mut (*taskstate).r7,
                                        7 => &raw mut (*taskstate).r8,
                                        8 => (*taskstate).scratchpad,
                                        9 => (*taskstate).largepad,
                                        #[allow(unused_unsafe)]
                                        10 => unsafe { (*taskstate).r2.selfref }
                                        #[allow(unreachable_patterns)]
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                                    .add(of1 as _),
                            ))
                                .pointer as *mut T;
                            let o2 = (unsafe {
                                match o2 {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut T)
                                .add(of2 as _);
                            Atomicable::a_load(o1, o2, ord)
                        };
                    }
                    #[allow(unused)]
                    fn call_cas<T: Atomicable + Clone + Copy>(
                        taskstate: *mut VMTaskState,
                        ptr_loc: u8,
                        ptr_loc_of: u8,
                        val_stored_loc: u8,
                        val_store_of: u8,
                        expected_loc: u8,
                        expected_of: u8,
                        ret_loc: u8,
                        ret_of: u8,
                        ord: Ordering,
                        ord2: Ordering,
                    ) {
                        unsafe {
                            let o1 = (std::ptr::read_unaligned(
                                    unsafe {
                                        match ptr_loc {
                                            0 => &raw mut (*taskstate).r1,
                                            1 => &raw mut (*taskstate).r2,
                                            2 => &raw mut (*taskstate).r3,
                                            3 => &raw mut (*taskstate).r4,
                                            4 => &raw mut (*taskstate).r5,
                                            5 => &raw mut (*taskstate).r6,
                                            6 => &raw mut (*taskstate).r7,
                                            7 => &raw mut (*taskstate).r8,
                                            8 => (*taskstate).scratchpad,
                                            9 => (*taskstate).largepad,
                                            #[allow(unused_unsafe)]
                                            10 => unsafe { (*taskstate).r2.selfref }
                                            #[allow(unreachable_patterns)]
                                            _ => ::core::panicking::panic("not implemented"),
                                        }
                                    }
                                        .add(ptr_loc_of as _),
                                )
                                .pointer as *mut T);
                            let o2 = (unsafe {
                                match val_stored_loc {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut T)
                                .add(val_store_of as _);
                            let o3 = (unsafe {
                                match expected_loc {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut T)
                                .offset(expected_of as _);
                            let o4 = (unsafe {
                                match ret_loc {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut T)
                                .add(ret_of as _);
                            Atomicable::a_cas(o1, o2, o3, o4, ord, ord2)
                        };
                    }
                    #[allow(unused)]
                    fn call_rmw<T: Atomicable + Clone + Copy>(
                        taskstate: *mut VMTaskState,
                        ord: Ordering,
                        o1: u8,
                        of1: u8,
                        o2: u8,
                        of2: u8,
                        o3: u8,
                        of3: u8,
                        op: ATOMICRmwOp,
                    ) {
                        unsafe {
                            let ptr = (std::ptr::read_unaligned(
                                unsafe {
                                    match o1 {
                                        0 => &raw mut (*taskstate).r1,
                                        1 => &raw mut (*taskstate).r2,
                                        2 => &raw mut (*taskstate).r3,
                                        3 => &raw mut (*taskstate).r4,
                                        4 => &raw mut (*taskstate).r5,
                                        5 => &raw mut (*taskstate).r6,
                                        6 => &raw mut (*taskstate).r7,
                                        7 => &raw mut (*taskstate).r8,
                                        8 => (*taskstate).scratchpad,
                                        9 => (*taskstate).largepad,
                                        #[allow(unused_unsafe)]
                                        10 => unsafe { (*taskstate).r2.selfref }
                                        #[allow(unreachable_patterns)]
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                                    .add(of1 as _),
                            ))
                                .pointer as *mut T;
                            let load_to = (unsafe {
                                match o2 {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut T)
                                .add(of2 as _);
                            let rhs = (unsafe {
                                match o3 {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut T)
                                .add(of3 as _);
                            Atomicable::a_rmw(ptr, load_to, rhs, op, ord);
                        };
                    }
                }
                pub use atomic::*;
                mod cast {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::cast::{CAST, parse_cast},
                        },
                        resolve_location_src,
                    };
                    use sart::ctr::VMTaskState;
                    use std::ptr;
                    pub fn call_cast(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        unsafe {
                            let CAST {
                                offset_src: offset1,
                                offset_target: offset2,
                                src,
                                target,
                                type_initial: tag_initial,
                                type_final: tag_final,
                            } = parse_cast(pickle, unsafe { (*ws).arr });
                            let src1 = unsafe {
                                match src {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                match target {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            match tag_initial {
                                0 => {
                                    let s1 = (src1 as *mut u64).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                1 => {
                                    let s1 = (src1 as *mut u32).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                2 => {
                                    let s1 = (src1 as *mut u16).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                3 => {
                                    let s1 = (src1 as *mut u8).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                4 => {
                                    let s1 = (src1 as *mut i64).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                5 => {
                                    let s1 = (src1 as *mut i32).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                6 => {
                                    let s1 = (src1 as *mut i16).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                7 => {
                                    let s1 = (src1 as *mut i8).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                8 => {
                                    let s1 = (src1 as *mut f64).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        8 => {
                                            let target = (target as *mut f64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        9 => {
                                            let target = (target as *mut f32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                9 => {
                                    let s1 = (src1 as *mut f32).offset(offset1 as _);
                                    let src1 = ptr::read_unaligned(s1);
                                    match tag_final {
                                        0 => {
                                            let target = (target as *mut u64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        1 => {
                                            let target = (target as *mut u32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        2 => {
                                            let target = (target as *mut u16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        3 => {
                                            let target = (target as *mut u8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        4 => {
                                            let target = (target as *mut i64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        5 => {
                                            let target = (target as *mut i32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        6 => {
                                            let target = (target as *mut i16).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        7 => {
                                            let target = (target as *mut i8).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        8 => {
                                            let target = (target as *mut f64).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        9 => {
                                            let target = (target as *mut f32).offset(offset2 as _);
                                            ptr::write_unaligned(target, src1 as _);
                                        }
                                        _ => {
                                            ::core::panicking::panic_fmt(
                                                format_args!("Wrong tag_final"),
                                            );
                                        }
                                    }
                                }
                                _ => {
                                    ::core::panicking::panic_fmt(
                                        format_args!("Wrong tag_initial"),
                                    );
                                }
                            };
                        }
                    }
                }
                pub use cast::*;
                mod fp {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::fp::{VFP, parse_vfp},
                        },
                        resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::{
                        ops::{Add, Div, Mul, Sub},
                        ptr,
                    };
                    fn arithprelude(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        task: *mut VMTaskState,
                    ) -> (
                        u8,
                        u8,
                        u32,
                        *mut QuadPackedData,
                        *mut QuadPackedData,
                        *mut QuadPackedData,
                        i32,
                        i32,
                        i32,
                    ) {
                        let VFP {
                            instdef,
                            count,
                            datatype,
                            src1,
                            src2,
                            tgt,
                            of_src1,
                            of_src2,
                            of_tgt,
                        } = parse_vfp(pickle, unsafe { (*ws).arr });
                        let src1 = {
                            unsafe {
                                match src1 {
                                    0 => &raw mut (*task).r1,
                                    1 => &raw mut (*task).r2,
                                    2 => &raw mut (*task).r3,
                                    3 => &raw mut (*task).r4,
                                    4 => &raw mut (*task).r5,
                                    5 => &raw mut (*task).r6,
                                    6 => &raw mut (*task).r7,
                                    7 => &raw mut (*task).r8,
                                    8 => (*task).scratchpad,
                                    9 => (*task).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*task).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            }
                        };
                        let src2 = {
                            unsafe {
                                match src2 {
                                    0 => &raw mut (*task).r1,
                                    1 => &raw mut (*task).r2,
                                    2 => &raw mut (*task).r3,
                                    3 => &raw mut (*task).r4,
                                    4 => &raw mut (*task).r5,
                                    5 => &raw mut (*task).r6,
                                    6 => &raw mut (*task).r7,
                                    7 => &raw mut (*task).r8,
                                    8 => (*task).scratchpad,
                                    9 => (*task).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*task).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            }
                        };
                        let target = {
                            unsafe {
                                match tgt {
                                    0 => &raw mut (*task).r1,
                                    1 => &raw mut (*task).r2,
                                    2 => &raw mut (*task).r3,
                                    3 => &raw mut (*task).r4,
                                    4 => &raw mut (*task).r5,
                                    5 => &raw mut (*task).r6,
                                    6 => &raw mut (*task).r7,
                                    7 => &raw mut (*task).r8,
                                    8 => (*task).scratchpad,
                                    9 => (*task).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*task).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            }
                        };
                        (
                            instdef,
                            datatype,
                            count,
                            src1,
                            src2,
                            target,
                            of_src1,
                            of_src2,
                            of_tgt,
                        )
                    }
                    pub fn call_vaddf(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (
                            _,
                            fptype,
                            count,
                            src1,
                            src2,
                            target,
                            offset1,
                            offset2,
                            offset_target,
                        ) = arithprelude(pickle, ws, taskstate);
                        {
                            match fptype {
                                8 => {
                                    unsafe {
                                        let dest = (target as *mut f64).offset(offset_target as _);
                                        let src1 = (src1 as *mut f64).offset(offset1 as _);
                                        let src2 = (src2 as *mut f64).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.add(s2));
                                        }
                                    }
                                }
                                9 => {
                                    unsafe {
                                        let dest = (target as *mut f32).offset(offset_target as _);
                                        let src1 = (src1 as *mut f32).offset(offset1 as _);
                                        let src2 = (src2 as *mut f32).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.add(s2));
                                        }
                                    }
                                }
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
                            }
                        }
                    }
                    pub fn call_vsubf(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (
                            _,
                            fptype,
                            count,
                            src1,
                            src2,
                            target,
                            offset1,
                            offset2,
                            offset_target,
                        ) = arithprelude(pickle, ws, taskstate);
                        {
                            match fptype {
                                8 => {
                                    unsafe {
                                        let dest = (target as *mut f64).offset(offset_target as _);
                                        let src1 = (src1 as *mut f64).offset(offset1 as _);
                                        let src2 = (src2 as *mut f64).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.sub(s2));
                                        }
                                    }
                                }
                                9 => {
                                    unsafe {
                                        let dest = (target as *mut f32).offset(offset_target as _);
                                        let src1 = (src1 as *mut f32).offset(offset1 as _);
                                        let src2 = (src2 as *mut f32).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.sub(s2));
                                        }
                                    }
                                }
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
                            }
                        }
                    }
                    pub fn call_vmulf(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (
                            _,
                            fptype,
                            count,
                            src1,
                            src2,
                            target,
                            offset1,
                            offset2,
                            offset_target,
                        ) = arithprelude(pickle, ws, taskstate);
                        {
                            match fptype {
                                8 => {
                                    unsafe {
                                        let dest = (target as *mut f64).offset(offset_target as _);
                                        let src1 = (src1 as *mut f64).offset(offset1 as _);
                                        let src2 = (src2 as *mut f64).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.mul(s2));
                                        }
                                    }
                                }
                                9 => {
                                    unsafe {
                                        let dest = (target as *mut f32).offset(offset_target as _);
                                        let src1 = (src1 as *mut f32).offset(offset1 as _);
                                        let src2 = (src2 as *mut f32).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.mul(s2));
                                        }
                                    }
                                }
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
                            }
                        }
                    }
                    pub fn call_vdivf(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (
                            _,
                            fptype,
                            count,
                            src1,
                            src2,
                            target,
                            offset1,
                            offset2,
                            offset_target,
                        ) = arithprelude(pickle, ws, taskstate);
                        {
                            match fptype {
                                8 => {
                                    unsafe {
                                        let dest = (target as *mut f64).offset(offset_target as _);
                                        let src1 = (src1 as *mut f64).offset(offset1 as _);
                                        let src2 = (src2 as *mut f64).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.div(s2));
                                        }
                                    }
                                }
                                9 => {
                                    unsafe {
                                        let dest = (target as *mut f32).offset(offset_target as _);
                                        let src1 = (src1 as *mut f32).offset(offset1 as _);
                                        let src2 = (src2 as *mut f32).offset(offset2 as _);
                                        for i in 0..count {
                                            let t = dest.add(i as _);
                                            let s1 = ptr::read_unaligned(src1.add(i as _));
                                            let s2 = ptr::read_unaligned(src2.add(i as _));
                                            ptr::write_unaligned(t, s1.div(s2));
                                        }
                                    }
                                }
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
                            }
                        }
                    }
                }
                pub use fp::*;
                mod vbit {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
                    fn vbitop_and_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & b });
                            }
                        }
                    }
                    fn vbitop_and_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & b });
                            }
                        }
                    }
                    fn vbitop_and_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & b });
                            }
                        }
                    }
                    fn vbitop_and_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & b });
                            }
                        }
                    }
                    fn vbitop_or_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | b });
                            }
                        }
                    }
                    fn vbitop_or_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | b });
                            }
                        }
                    }
                    fn vbitop_or_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | b });
                            }
                        }
                    }
                    fn vbitop_or_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | b });
                            }
                        }
                    }
                    fn vbitop_xor_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ b });
                            }
                        }
                    }
                    fn vbitop_xor_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ b });
                            }
                        }
                    }
                    fn vbitop_xor_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ b });
                            }
                        }
                    }
                    fn vbitop_xor_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ b });
                            }
                        }
                    }
                    fn vbitop_not_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { !a });
                            }
                        }
                    }
                    fn vbitop_not_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { !a });
                            }
                        }
                    }
                    fn vbitop_not_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { !a });
                            }
                        }
                    }
                    fn vbitop_not_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { !a });
                            }
                        }
                    }
                    fn vbitop_or_not_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | !b });
                            }
                        }
                    }
                    fn vbitop_or_not_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | !b });
                            }
                        }
                    }
                    fn vbitop_or_not_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | !b });
                            }
                        }
                    }
                    fn vbitop_or_not_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a | !b });
                            }
                        }
                    }
                    fn vbitop_and_not_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & !b });
                            }
                        }
                    }
                    fn vbitop_and_not_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & !b });
                            }
                        }
                    }
                    fn vbitop_and_not_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & !b });
                            }
                        }
                    }
                    fn vbitop_and_not_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a & !b });
                            }
                        }
                    }
                    fn vbitop_xor_not_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ !b });
                            }
                        }
                    }
                    fn vbitop_xor_not_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ !b });
                            }
                        }
                    }
                    fn vbitop_xor_not_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ !b });
                            }
                        }
                    }
                    fn vbitop_xor_not_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a ^ !b });
                            }
                        }
                    }
                    fn vbitop_bitrev_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.reverse_bits() });
                            }
                        }
                    }
                    fn vbitop_bitrev_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.reverse_bits() });
                            }
                        }
                    }
                    fn vbitop_bitrev_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.reverse_bits() });
                            }
                        }
                    }
                    fn vbitop_bitrev_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.reverse_bits() });
                            }
                        }
                    }
                    fn vbitop_bswap_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.swap_bytes() });
                            }
                        }
                    }
                    fn vbitop_bswap_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.swap_bytes() });
                            }
                        }
                    }
                    fn vbitop_bswap_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.swap_bytes() });
                            }
                        }
                    }
                    fn vbitop_bswap_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let _b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { a.swap_bytes() });
                            }
                        }
                    }
                    const _DISPATCH: [fn(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ); 36] = [
                        vbitop_and_u64,
                        vbitop_and_u32,
                        vbitop_and_u16,
                        vbitop_and_u8,
                        vbitop_or_u64,
                        vbitop_or_u32,
                        vbitop_or_u16,
                        vbitop_or_u8,
                        vbitop_xor_u64,
                        vbitop_xor_u32,
                        vbitop_xor_u16,
                        vbitop_xor_u8,
                        vbitop_not_u64,
                        vbitop_not_u32,
                        vbitop_not_u16,
                        vbitop_not_u8,
                        vbitop_or_not_u64,
                        vbitop_or_not_u32,
                        vbitop_or_not_u16,
                        vbitop_or_not_u8,
                        vbitop_and_not_u64,
                        vbitop_and_not_u32,
                        vbitop_and_not_u16,
                        vbitop_and_not_u8,
                        vbitop_xor_not_u64,
                        vbitop_xor_not_u32,
                        vbitop_xor_not_u16,
                        vbitop_xor_not_u8,
                        vbitop_bitrev_u64,
                        vbitop_bitrev_u32,
                        vbitop_bitrev_u16,
                        vbitop_bitrev_u8,
                        vbitop_bswap_u64,
                        vbitop_bswap_u32,
                        vbitop_bswap_u16,
                        vbitop_bswap_u8,
                    ];
                    const TYPE_COUNT: u8 = 4;
                    #[inline(always)]
                    const fn calc_offset(op: u8, ty: u8) -> usize {
                        (op * TYPE_COUNT + ty) as _
                    }
                    pub fn call_vbit(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        ts: *mut VMTaskState,
                    ) {
                        unsafe {
                            let count = pickle.u3;
                            let op = count >> 4;
                            let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                            let width = (flags >> 14) as u8;
                            let count = {
                                let countdata = {
                                    #[allow(unused_unsafe)]
                                    <u32>::from_ne_bytes(unsafe {
                                        (&(*ws).arr)[0..4].try_into().unwrap_unchecked()
                                    })
                                };
                                countdata
                            };
                            let flags_src1 = (flags as u8) & 0x0F;
                            let flags_src2 = (flags as u8) >> 4 & 0x0F;
                            let flags_tg = (flags >> 12) as u8 & 0x0F;
                            let src1 = unsafe {
                                match flags_src1 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                match flags_src2 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let tg = unsafe {
                                match flags_tg {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let of_src1 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[4..8].try_into().unwrap_unchecked()
                                })
                            };
                            let of_src2 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[8..12].try_into().unwrap_unchecked()
                                })
                            };
                            let of_tg = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[12..16].try_into().unwrap_unchecked()
                                })
                            };
                            let offset = calc_offset(op, width);
                            (_DISPATCH
                                .get_unchecked(
                                    offset,
                                ))(src1, src2, tg, of_src1, of_src2, of_tg, count);
                        }
                    }
                }
                pub use vbit::*;
                mod vcnt {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::vminimax::{VCNT, parse_vcnt},
                        },
                        resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
                    fn vop_popcnt_u64(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.count_ones() as _ } });
                            }
                        }
                    }
                    fn vop_popcnt_u32(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.count_ones() as _ } });
                            }
                        }
                    }
                    fn vop_popcnt_u16(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.count_ones() as _ } });
                            }
                        }
                    }
                    fn vop_popcnt_u8(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.count_ones() as _ } });
                            }
                        }
                    }
                    fn vop_clz_u64(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_zeros() as _ } });
                            }
                        }
                    }
                    fn vop_clz_u32(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_zeros() as _ } });
                            }
                        }
                    }
                    fn vop_clz_u16(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_zeros() as _ } });
                            }
                        }
                    }
                    fn vop_clz_u8(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_zeros() as _ } });
                            }
                        }
                    }
                    fn vop_cls_u64(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_ones() as _ } });
                            }
                        }
                    }
                    fn vop_cls_u32(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_ones() as _ } });
                            }
                        }
                    }
                    fn vop_cls_u16(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_ones() as _ } });
                            }
                        }
                    }
                    fn vop_cls_u8(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.leading_ones() as _ } });
                            }
                        }
                    }
                    fn vop_ctz_u64(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.trailing_zeros() as _ } });
                            }
                        }
                    }
                    fn vop_ctz_u32(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.trailing_zeros() as _ } });
                            }
                        }
                    }
                    fn vop_ctz_u16(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.trailing_zeros() as _ } });
                            }
                        }
                    }
                    fn vop_ctz_u8(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.trailing_zeros() as _ } });
                            }
                        }
                    }
                    const _DISPATCH: [fn(
                        src1: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset3: i32,
                        count: u32,
                    ); 16] = [
                        vop_popcnt_u64,
                        vop_popcnt_u32,
                        vop_popcnt_u16,
                        vop_popcnt_u8,
                        vop_clz_u64,
                        vop_clz_u32,
                        vop_clz_u16,
                        vop_clz_u8,
                        vop_cls_u64,
                        vop_cls_u32,
                        vop_cls_u16,
                        vop_cls_u8,
                        vop_ctz_u64,
                        vop_ctz_u32,
                        vop_ctz_u16,
                        vop_ctz_u8,
                    ];
                    const TYPE_COUNT: u8 = 4;
                    #[inline(always)]
                    const fn calc_offset(op: u8, ty: u8) -> usize {
                        (op * TYPE_COUNT + ty) as _
                    }
                    pub fn call_vcnt(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        ts: *mut VMTaskState,
                    ) {
                        unsafe {
                            let VCNT {
                                op,
                                flags_src,
                                flags_target,
                                count,
                                of_src,
                                of_target,
                                typ,
                                ..
                            } = parse_vcnt(pickle, unsafe { (*ws).arr });
                            let src = unsafe {
                                match flags_src {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                match flags_target {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let offset = calc_offset(op, typ);
                            (_DISPATCH
                                .get_unchecked(
                                    offset,
                                ))(src, target, of_src as _, of_target as _, count);
                        }
                    }
                }
                pub use vcnt::*;
                mod vcopy {
                    use sart::ctr::VMTaskState;
                    use std::ptr;
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::corevm::{VCOPY, parse_vcopy},
                        },
                        resolve_location_src,
                    };
                    pub fn call_vcopy(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let VCOPY {
                            src,
                            target,
                            count,
                            src_offset,
                            target_offset,
                            ..
                        } = parse_vcopy(pickle, unsafe { (*ws).arr }.as_ref());
                        let src1 = unsafe {
                            (unsafe {
                                match src {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut u8)
                                .offset(src_offset as _)
                        };
                        let target = unsafe {
                            (unsafe {
                                match target {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            } as *mut u8)
                                .offset(target_offset as _)
                        };
                        unsafe { ptr::copy(src1, target, count.get(taskstate) as _) };
                    }
                }
                pub use vcopy::*;
                mod vfma {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
                    trait Fma {
                        /// Calculates `(self*a)+b` but rounding ONLY once
                        ///
                        /// Please note that FMA provides higher precision than simple MUL, ADD sequence
                        fn fma(&self, a: Self, b: Self) -> Self;
                    }
                    impl Fma for f32 {
                        fn fma(&self, a: Self, b: Self) -> Self {
                            self.mul_add(a, b)
                        }
                    }
                    impl Fma for f64 {
                        fn fma(&self, a: Self, b: Self) -> Self {
                            self.mul_add(a, b)
                        }
                    }
                    const _DISPATCH: [fn(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        tg: *mut QuadPackedData,
                        of1: i32,
                        of2: i32,
                        of3: i32,
                        oft: i32,
                        count: u32,
                    ); 2] = [vop_fma::<f64>, vop_fma::<f32>];
                    fn vop_fma<T>(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        tg: *mut QuadPackedData,
                        of1: i32,
                        of2: i32,
                        of3: i32,
                        oft: i32,
                        count: u32,
                    )
                    where
                        T: Fma,
                    {
                        unsafe {
                            let s1 = (src1 as *mut T).offset(of1 as _);
                            let s2 = (src2 as *mut T).offset(of2 as _);
                            let s3 = (src3 as *mut T).offset(of3 as _);
                            let t = (tg as *mut T).offset(oft as _);
                            for idx in 0..(count as usize) {
                                let s = ptr::read_unaligned(s1.add(idx));
                                let s_a = ptr::read_unaligned(s2.add(idx));
                                let s_b = ptr::read_unaligned(s3.add(idx));
                                let tg = t.add(idx);
                                ptr::write_unaligned(tg, s.fma(s_a, s_b));
                            }
                        }
                    }
                    #[inline(always)]
                    const fn calc_offset(ty: u8) -> usize {
                        ty as _
                    }
                    pub fn call_vfma(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        ts: *mut VMTaskState,
                    ) {
                        unsafe {
                            let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                            let floattype = pickle.u3 & 0x01;
                            let count = {
                                #[allow(unused_unsafe)]
                                <u32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[0..4].try_into().unwrap_unchecked()
                                })
                            };
                            let flags_src1 = (flags >> 12) as u8 & 0x0F;
                            let flags_src2 = (flags >> 8) as u8 & 0x0F;
                            let flags_src3 = (flags >> 4) as u8 & 0x0F;
                            let flags_tg = flags as u8 & 0x0F;
                            let src1 = unsafe {
                                match flags_src1 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                match flags_src2 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src3 = unsafe {
                                match flags_src3 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let tg = unsafe {
                                match flags_tg {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let of_src1 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[4..8].try_into().unwrap_unchecked()
                                })
                            };
                            let of_src2 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[8..12].try_into().unwrap_unchecked()
                                })
                            };
                            let of_src3 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[12..16].try_into().unwrap_unchecked()
                                })
                            };
                            let of_tg = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[16..20].try_into().unwrap_unchecked()
                                })
                            };
                            let offset = calc_offset(floattype);
                            (_DISPATCH
                                .get_unchecked(
                                    offset,
                                ))(
                                src1,
                                src2,
                                src3,
                                tg,
                                of_src1,
                                of_src2,
                                of_src3,
                                of_tg,
                                count,
                            );
                        }
                    }
                }
                pub use vfma::*;
                mod vfop {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::{
                                cast::{VFCAST, parse_vfcast},
                                vfop::{
                                    FOP_CEIL, FOP_FLOOR, FOP_ROUND, FOP_SQRT, FOP_TRUNC, VFOP,
                                    parse_vfop,
                                },
                            },
                        },
                        resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
                    pub fn call_vfop(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let VFOP {
                            src,
                            target,
                            offset_src: offset1,
                            offset_target: offset2,
                            count,
                            subop,
                            typetag,
                        } = parse_vfop(pickle, unsafe { (*ws).arr }.as_ref());
                        let src1 = unsafe {
                            match src {
                                0 => &raw mut (*taskstate).r1,
                                1 => &raw mut (*taskstate).r2,
                                2 => &raw mut (*taskstate).r3,
                                3 => &raw mut (*taskstate).r4,
                                4 => &raw mut (*taskstate).r5,
                                5 => &raw mut (*taskstate).r6,
                                6 => &raw mut (*taskstate).r7,
                                7 => &raw mut (*taskstate).r8,
                                8 => (*taskstate).scratchpad,
                                9 => (*taskstate).largepad,
                                #[allow(unused_unsafe)]
                                10 => unsafe { (*taskstate).r2.selfref }
                                #[allow(unreachable_patterns)]
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target1 = unsafe {
                            match target {
                                0 => &raw mut (*taskstate).r1,
                                1 => &raw mut (*taskstate).r2,
                                2 => &raw mut (*taskstate).r3,
                                3 => &raw mut (*taskstate).r4,
                                4 => &raw mut (*taskstate).r5,
                                5 => &raw mut (*taskstate).r6,
                                6 => &raw mut (*taskstate).r7,
                                7 => &raw mut (*taskstate).r8,
                                8 => (*taskstate).scratchpad,
                                9 => (*taskstate).largepad,
                                #[allow(unused_unsafe)]
                                10 => unsafe { (*taskstate).r2.selfref }
                                #[allow(unreachable_patterns)]
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        match typetag {
                            8 => {
                                match subop {
                                    FOP_CEIL => {
                                        unsafe {
                                            let dest = (target1 as *mut f64).offset(offset2 as _);
                                            let src1 = (src1 as *mut f64).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.ceil());
                                            }
                                        }
                                    }
                                    FOP_FLOOR => {
                                        unsafe {
                                            let dest = (target1 as *mut f64).offset(offset2 as _);
                                            let src1 = (src1 as *mut f64).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.floor());
                                            }
                                        }
                                    }
                                    FOP_TRUNC => {
                                        unsafe {
                                            let dest = (target1 as *mut f64).offset(offset2 as _);
                                            let src1 = (src1 as *mut f64).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.trunc());
                                            }
                                        }
                                    }
                                    FOP_ROUND => {
                                        unsafe {
                                            let dest = (target1 as *mut f64).offset(offset2 as _);
                                            let src1 = (src1 as *mut f64).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.round());
                                            }
                                        }
                                    }
                                    FOP_SQRT => {
                                        unsafe {
                                            let dest = (target1 as *mut f64).offset(offset2 as _);
                                            let src1 = (src1 as *mut f64).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.sqrt());
                                            }
                                        }
                                    }
                                    _ => ::core::panicking::panic("explicit panic"),
                                }
                            }
                            9 => {
                                match subop {
                                    FOP_CEIL => {
                                        unsafe {
                                            let dest = (target1 as *mut f32).offset(offset2 as _);
                                            let src1 = (src1 as *mut f32).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.ceil());
                                            }
                                        }
                                    }
                                    FOP_FLOOR => {
                                        unsafe {
                                            let dest = (target1 as *mut f32).offset(offset2 as _);
                                            let src1 = (src1 as *mut f32).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.floor());
                                            }
                                        }
                                    }
                                    FOP_TRUNC => {
                                        unsafe {
                                            let dest = (target1 as *mut f32).offset(offset2 as _);
                                            let src1 = (src1 as *mut f32).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.trunc());
                                            }
                                        }
                                    }
                                    FOP_ROUND => {
                                        unsafe {
                                            let dest = (target1 as *mut f32).offset(offset2 as _);
                                            let src1 = (src1 as *mut f32).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.round());
                                            }
                                        }
                                    }
                                    FOP_SQRT => {
                                        unsafe {
                                            let dest = (target1 as *mut f32).offset(offset2 as _);
                                            let src1 = (src1 as *mut f32).offset(offset1 as _);
                                            for i in 0..count {
                                                let t = dest.add(i as _);
                                                let s1 = ptr::read_unaligned(src1.add(i as _));
                                                ptr::write_unaligned(t, s1.sqrt());
                                            }
                                        }
                                    }
                                    _ => ::core::panicking::panic("explicit panic"),
                                }
                            }
                            _ => ::core::panicking::panic("explicit panic"),
                        }
                    }
                    pub fn call_vfcast(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let VFCAST {
                            offset_src,
                            offset_target,
                            count,
                            src,
                            target,
                            type_initial,
                            type_final,
                        } = parse_vfcast(pickle, unsafe { (*ws).arr }.as_ref());
                        let f = match (type_initial, type_final) {
                            (8, 0) => as_cast::<f64, u64>,
                            (8, 1) => as_cast::<f64, u32>,
                            (8, 2) => as_cast::<f64, u16>,
                            (8, 3) => as_cast::<f64, u8>,
                            (8, 4) => as_cast::<f64, i64>,
                            (8, 5) => as_cast::<f64, i32>,
                            (8, 6) => as_cast::<f64, i16>,
                            (8, 7) => as_cast::<f64, i8>,
                            (9, 0) => as_cast::<f32, u64>,
                            (9, 1) => as_cast::<f32, u32>,
                            (9, 2) => as_cast::<f32, u16>,
                            (9, 3) => as_cast::<f32, u8>,
                            (9, 4) => as_cast::<f32, i64>,
                            (9, 5) => as_cast::<f32, i32>,
                            (9, 6) => as_cast::<f32, i16>,
                            (9, 7) => as_cast::<f32, i8>,
                            (0, 9) => as_cast::<u64, f32>,
                            (1, 9) => as_cast::<u32, f32>,
                            (2, 9) => as_cast::<u16, f32>,
                            (3, 9) => as_cast::<u8, f32>,
                            (4, 9) => as_cast::<i64, f32>,
                            (5, 9) => as_cast::<i32, f32>,
                            (6, 9) => as_cast::<i16, f32>,
                            (7, 9) => as_cast::<i8, f32>,
                            (0, 8) => as_cast::<u64, f64>,
                            (1, 8) => as_cast::<u32, f64>,
                            (2, 8) => as_cast::<u16, f64>,
                            (3, 8) => as_cast::<u8, f64>,
                            (4, 8) => as_cast::<i64, f64>,
                            (5, 8) => as_cast::<i32, f64>,
                            (6, 8) => as_cast::<i16, f64>,
                            (7, 8) => as_cast::<i8, f64>,
                            _ => {
                                ::core::panicking::panic(
                                    "internal error: entered unreachable code",
                                )
                            }
                        };
                        let src1 = unsafe {
                            match src {
                                0 => &raw mut (*taskstate).r1,
                                1 => &raw mut (*taskstate).r2,
                                2 => &raw mut (*taskstate).r3,
                                3 => &raw mut (*taskstate).r4,
                                4 => &raw mut (*taskstate).r5,
                                5 => &raw mut (*taskstate).r6,
                                6 => &raw mut (*taskstate).r7,
                                7 => &raw mut (*taskstate).r8,
                                8 => (*taskstate).scratchpad,
                                9 => (*taskstate).largepad,
                                #[allow(unused_unsafe)]
                                10 => unsafe { (*taskstate).r2.selfref }
                                #[allow(unreachable_patterns)]
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target1 = unsafe {
                            match target {
                                0 => &raw mut (*taskstate).r1,
                                1 => &raw mut (*taskstate).r2,
                                2 => &raw mut (*taskstate).r3,
                                3 => &raw mut (*taskstate).r4,
                                4 => &raw mut (*taskstate).r5,
                                5 => &raw mut (*taskstate).r6,
                                6 => &raw mut (*taskstate).r7,
                                7 => &raw mut (*taskstate).r8,
                                8 => (*taskstate).scratchpad,
                                9 => (*taskstate).largepad,
                                #[allow(unused_unsafe)]
                                10 => unsafe { (*taskstate).r2.selfref }
                                #[allow(unreachable_patterns)]
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        f(src1, target1, offset_src, offset_target, count);
                    }
                    trait CastTo<Target> {
                        fn castto(&self) -> Target;
                    }
                    impl CastTo<f32> for u8 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<u8> for f32 {
                        fn castto(&self) -> u8 {
                            *self as _
                        }
                    }
                    impl CastTo<f32> for u16 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<u16> for f32 {
                        fn castto(&self) -> u16 {
                            *self as _
                        }
                    }
                    impl CastTo<f32> for u32 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<u32> for f32 {
                        fn castto(&self) -> u32 {
                            *self as _
                        }
                    }
                    impl CastTo<f32> for u64 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<u64> for f32 {
                        fn castto(&self) -> u64 {
                            *self as _
                        }
                    }
                    impl CastTo<f32> for i8 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<i8> for f32 {
                        fn castto(&self) -> i8 {
                            *self as _
                        }
                    }
                    impl CastTo<f32> for i16 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<i16> for f32 {
                        fn castto(&self) -> i16 {
                            *self as _
                        }
                    }
                    impl CastTo<f32> for i32 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<i32> for f32 {
                        fn castto(&self) -> i32 {
                            *self as _
                        }
                    }
                    impl CastTo<f32> for i64 {
                        fn castto(&self) -> f32 {
                            *self as _
                        }
                    }
                    impl CastTo<i64> for f32 {
                        fn castto(&self) -> i64 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for u8 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<u8> for f64 {
                        fn castto(&self) -> u8 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for u16 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<u16> for f64 {
                        fn castto(&self) -> u16 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for u32 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<u32> for f64 {
                        fn castto(&self) -> u32 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for u64 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<u64> for f64 {
                        fn castto(&self) -> u64 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for i8 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<i8> for f64 {
                        fn castto(&self) -> i8 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for i16 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<i16> for f64 {
                        fn castto(&self) -> i16 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for i32 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<i32> for f64 {
                        fn castto(&self) -> i32 {
                            *self as _
                        }
                    }
                    impl CastTo<f64> for i64 {
                        fn castto(&self) -> f64 {
                            *self as _
                        }
                    }
                    impl CastTo<i64> for f64 {
                        fn castto(&self) -> i64 {
                            *self as _
                        }
                    }
                    fn as_cast<T, E>(
                        src1: *mut QuadPackedData,
                        target: *mut QuadPackedData,
                        offsetsrc: i32,
                        offsettgt: i32,
                        count: u32,
                    )
                    where
                        T: CastTo<E>,
                    {
                        unsafe {
                            let src1 = (src1 as *mut T).offset(offsetsrc as _);
                            let target = (target as *mut E).offset(offsettgt as _);
                            for c in 0..count {
                                let r = ptr::read_unaligned(src1.add(c as _));
                                ptr::write_unaligned(target.add(c as _), r.castto());
                            }
                        }
                    }
                }
                pub use vfop::*;
                mod vops {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve_location_src,
                    };
                    use sart::ctr::VMTaskState;
                    use std::{ops::Neg, ptr};
                    pub fn call_vneg(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (count, typetag, src1, target, offset1, offset2) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let typetag = (flags >> 12) as u8;
                            let countbit = ((flags >> 4) & 0x01) as u8;
                            let count_data = {
                                #[allow(unused_unsafe)]
                                <u32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[0..4].try_into().unwrap_unchecked()
                                })
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { (*taskstate).r1.u32 }
                            };
                            let offset1 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[4..8].try_into().unwrap_unchecked()
                                })
                            };
                            let offset2 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[8..12].try_into().unwrap_unchecked()
                                })
                            };
                            let src1 = {
                                let src = (flags >> 8 as u8) & 0x0F;
                                unsafe {
                                    match src {
                                        0 => &raw mut (*taskstate).r1,
                                        1 => &raw mut (*taskstate).r2,
                                        2 => &raw mut (*taskstate).r3,
                                        3 => &raw mut (*taskstate).r4,
                                        4 => &raw mut (*taskstate).r5,
                                        5 => &raw mut (*taskstate).r6,
                                        6 => &raw mut (*taskstate).r7,
                                        7 => &raw mut (*taskstate).r8,
                                        8 => (*taskstate).scratchpad,
                                        9 => (*taskstate).largepad,
                                        #[allow(unused_unsafe)]
                                        10 => unsafe { (*taskstate).r2.selfref }
                                        #[allow(unreachable_patterns)]
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                            };
                            let target = {
                                let src = ((flags >> 4) as u8) & 0x0F;
                                unsafe {
                                    match src {
                                        0 => &raw mut (*taskstate).r1,
                                        1 => &raw mut (*taskstate).r2,
                                        2 => &raw mut (*taskstate).r3,
                                        3 => &raw mut (*taskstate).r4,
                                        4 => &raw mut (*taskstate).r5,
                                        5 => &raw mut (*taskstate).r6,
                                        6 => &raw mut (*taskstate).r7,
                                        7 => &raw mut (*taskstate).r8,
                                        8 => (*taskstate).scratchpad,
                                        9 => (*taskstate).largepad,
                                        #[allow(unused_unsafe)]
                                        10 => unsafe { (*taskstate).r2.selfref }
                                        #[allow(unreachable_patterns)]
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                            };
                            (count, typetag, src1, target, offset1, offset2)
                        };
                        match typetag {
                            4 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i64)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i64).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_neg());
                                    }
                                }
                            }
                            5 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i32)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i32).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_neg());
                                    }
                                }
                            }
                            6 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i16)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i16).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_neg());
                                    }
                                }
                            }
                            7 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i8)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i8).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_neg());
                                    }
                                }
                            }
                            8 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut f64)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut f64).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.neg());
                                    }
                                }
                            }
                            9 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut f32)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut f32).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.neg());
                                    }
                                }
                            }
                            _ => {
                                ::core::panicking::panic_fmt(
                                    format_args!("Invalid type to neg"),
                                );
                            }
                        }
                    }
                    pub fn call_vabs(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (count, typetag, src1, target, offset1, offset2) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let typetag = (flags >> 12) as u8;
                            let countbit = ((flags >> 4) & 0x01) as u8;
                            let count_data = {
                                #[allow(unused_unsafe)]
                                <u32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[0..4].try_into().unwrap_unchecked()
                                })
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { (*taskstate).r1.u32 }
                            };
                            let offset1 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[4..8].try_into().unwrap_unchecked()
                                })
                            };
                            let offset2 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[8..12].try_into().unwrap_unchecked()
                                })
                            };
                            let src1 = {
                                let src = (flags >> 8 as u8) & 0x0F;
                                unsafe {
                                    match src {
                                        0 => &raw mut (*taskstate).r1,
                                        1 => &raw mut (*taskstate).r2,
                                        2 => &raw mut (*taskstate).r3,
                                        3 => &raw mut (*taskstate).r4,
                                        4 => &raw mut (*taskstate).r5,
                                        5 => &raw mut (*taskstate).r6,
                                        6 => &raw mut (*taskstate).r7,
                                        7 => &raw mut (*taskstate).r8,
                                        8 => (*taskstate).scratchpad,
                                        9 => (*taskstate).largepad,
                                        #[allow(unused_unsafe)]
                                        10 => unsafe { (*taskstate).r2.selfref }
                                        #[allow(unreachable_patterns)]
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                            };
                            let target = {
                                let src = ((flags >> 4) as u8) & 0x0F;
                                unsafe {
                                    match src {
                                        0 => &raw mut (*taskstate).r1,
                                        1 => &raw mut (*taskstate).r2,
                                        2 => &raw mut (*taskstate).r3,
                                        3 => &raw mut (*taskstate).r4,
                                        4 => &raw mut (*taskstate).r5,
                                        5 => &raw mut (*taskstate).r6,
                                        6 => &raw mut (*taskstate).r7,
                                        7 => &raw mut (*taskstate).r8,
                                        8 => (*taskstate).scratchpad,
                                        9 => (*taskstate).largepad,
                                        #[allow(unused_unsafe)]
                                        10 => unsafe { (*taskstate).r2.selfref }
                                        #[allow(unreachable_patterns)]
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                            };
                            (count, typetag, src1, target, offset1, offset2)
                        };
                        match typetag {
                            4 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i64)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i64).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_abs());
                                    }
                                }
                            }
                            5 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i32)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i32).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_abs());
                                    }
                                }
                            }
                            6 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i16)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i16).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_abs());
                                    }
                                }
                            }
                            7 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut i8)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut i8).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.strict_abs());
                                    }
                                }
                            }
                            8 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut f64)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut f64).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.abs());
                                    }
                                }
                            }
                            9 => {
                                unsafe {
                                    for i in 0..count {
                                        let t = (target as *mut f32)
                                            .offset(offset2 as _)
                                            .add(i as _);
                                        let s1 = ptr::read_unaligned(
                                            (src1 as *mut f32).offset(offset1 as _).add(i as _),
                                        );
                                        ptr::write_unaligned(t, s1.abs());
                                    }
                                }
                            }
                            _ => {
                                ::core::panicking::panic_fmt(
                                    format_args!("Invalid type to neg"),
                                );
                            }
                        }
                    }
                }
                pub use vops::*;
                mod vrot {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
                    fn vbitop_rotl_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u64).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotl_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u32).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotl_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u16).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotl_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u8).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotl_i64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i64).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i64).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotl_i32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i32).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i32).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotl_i16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i16).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i16).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotl_i8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i8).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i8).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_left(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u64).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u32).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u16).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut u8).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_i64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i64).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i64).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i64 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_i32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i32).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i32).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i32 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_i16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i16).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i16).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i16 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    fn vbitop_rotr_i8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i8).offset(offset1 as _);
                            let b = ptr::read_unaligned(
                                (src2 as *mut i8).offset(offset2 as _),
                            );
                            let t1 = (src3 as *mut i8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i8 = ptr::read_unaligned(s1.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(
                                    t2,
                                    {
                                        {
                                            let w = (std::mem::size_of_val(&a) * 8) as i64;
                                            let amt = (b as i64).rem_euclid(w) as u32;
                                            a.rotate_right(amt)
                                        }
                                    },
                                );
                            }
                        }
                    }
                    const _DISPATCH: [fn(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ); 16] = [
                        vbitop_rotl_u64,
                        vbitop_rotl_u32,
                        vbitop_rotl_u16,
                        vbitop_rotl_u8,
                        vbitop_rotl_i64,
                        vbitop_rotl_i32,
                        vbitop_rotl_i16,
                        vbitop_rotl_i8,
                        vbitop_rotr_u64,
                        vbitop_rotr_u32,
                        vbitop_rotr_u16,
                        vbitop_rotr_u8,
                        vbitop_rotr_i64,
                        vbitop_rotr_i32,
                        vbitop_rotr_i16,
                        vbitop_rotr_i8,
                    ];
                    const TYPE_COUNT: u8 = 8;
                    #[inline(always)]
                    const fn calc_offset(op: u8, ty: u8) -> usize {
                        (op * TYPE_COUNT + ty) as _
                    }
                    pub fn call_vrot(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        ts: *mut VMTaskState,
                    ) {
                        unsafe {
                            let rot = pickle.u3;
                            let op = rot & 0x01;
                            let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                            let typ = (flags >> 12) as u8;
                            let count = {
                                let countdata = {
                                    #[allow(unused_unsafe)]
                                    <u32>::from_ne_bytes(unsafe {
                                        (&(*ws).arr)[0..4].try_into().unwrap_unchecked()
                                    })
                                };
                                countdata
                            };
                            let flags_src1 = (flags as u8) & 0x0F;
                            let flags_src2 = (flags as u8) >> 4 & 0x0F;
                            let flags_tg = (flags >> 12) as u8 & 0x0F;
                            let src1 = unsafe {
                                match flags_src1 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                match flags_src2 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let tg = unsafe {
                                match flags_tg {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let of_src1 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[4..8].try_into().unwrap_unchecked()
                                })
                            };
                            let of_src2 = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[8..12].try_into().unwrap_unchecked()
                                })
                            };
                            let of_tg = {
                                #[allow(unused_unsafe)]
                                <i32>::from_ne_bytes(unsafe {
                                    (&(*ws).arr)[12..16].try_into().unwrap_unchecked()
                                })
                            };
                            let offset = calc_offset(op, typ);
                            (_DISPATCH
                                .get_unchecked(
                                    offset,
                                ))(src1, src2, tg, of_src1, of_src2, of_tg, count);
                        }
                    }
                }
                pub use vrot::*;
                mod vminimax {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::vminimax::{VMINIMAX, parse_vminimax},
                        },
                        resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
                    fn vop_min_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).add(offset1 as _);
                            let s2 = (src2 as *mut u64).add(offset2 as _);
                            let t1 = (src3 as *mut u64).add(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).add(offset1 as _);
                            let s2 = (src2 as *mut u32).add(offset2 as _);
                            let t1 = (src3 as *mut u32).add(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).add(offset1 as _);
                            let s2 = (src2 as *mut u16).add(offset2 as _);
                            let t1 = (src3 as *mut u16).add(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).add(offset1 as _);
                            let s2 = (src2 as *mut u8).add(offset2 as _);
                            let t1 = (src3 as *mut u8).add(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_i64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i64).add(offset1 as _);
                            let s2 = (src2 as *mut i64).add(offset2 as _);
                            let t1 = (src3 as *mut i64).add(offset3 as _);
                            for idx in 0..count {
                                let a: i64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_i32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i32).add(offset1 as _);
                            let s2 = (src2 as *mut i32).add(offset2 as _);
                            let t1 = (src3 as *mut i32).add(offset3 as _);
                            for idx in 0..count {
                                let a: i32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_i16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i16).add(offset1 as _);
                            let s2 = (src2 as *mut i16).add(offset2 as _);
                            let t1 = (src3 as *mut i16).add(offset3 as _);
                            for idx in 0..count {
                                let a: i16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_i8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i8).add(offset1 as _);
                            let s2 = (src2 as *mut i8).add(offset2 as _);
                            let t1 = (src3 as *mut i8).add(offset3 as _);
                            for idx in 0..count {
                                let a: i8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_f64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut f64).add(offset1 as _);
                            let s2 = (src2 as *mut f64).add(offset2 as _);
                            let t1 = (src3 as *mut f64).add(offset3 as _);
                            for idx in 0..count {
                                let a: f64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: f64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_min_f32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut f32).add(offset1 as _);
                            let s2 = (src2 as *mut f32).add(offset2 as _);
                            let t1 = (src3 as *mut f32).add(offset3 as _);
                            for idx in 0..count {
                                let a: f32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: f32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.min(b) } });
                            }
                        }
                    }
                    fn vop_max_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).add(offset1 as _);
                            let s2 = (src2 as *mut u64).add(offset2 as _);
                            let t1 = (src3 as *mut u64).add(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).add(offset1 as _);
                            let s2 = (src2 as *mut u32).add(offset2 as _);
                            let t1 = (src3 as *mut u32).add(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).add(offset1 as _);
                            let s2 = (src2 as *mut u16).add(offset2 as _);
                            let t1 = (src3 as *mut u16).add(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).add(offset1 as _);
                            let s2 = (src2 as *mut u8).add(offset2 as _);
                            let t1 = (src3 as *mut u8).add(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: u8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_i64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i64).add(offset1 as _);
                            let s2 = (src2 as *mut i64).add(offset2 as _);
                            let t1 = (src3 as *mut i64).add(offset3 as _);
                            for idx in 0..count {
                                let a: i64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_i32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i32).add(offset1 as _);
                            let s2 = (src2 as *mut i32).add(offset2 as _);
                            let t1 = (src3 as *mut i32).add(offset3 as _);
                            for idx in 0..count {
                                let a: i32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_i16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i16).add(offset1 as _);
                            let s2 = (src2 as *mut i16).add(offset2 as _);
                            let t1 = (src3 as *mut i16).add(offset3 as _);
                            for idx in 0..count {
                                let a: i16 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i16 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_i8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i8).add(offset1 as _);
                            let s2 = (src2 as *mut i8).add(offset2 as _);
                            let t1 = (src3 as *mut i8).add(offset3 as _);
                            for idx in 0..count {
                                let a: i8 = ptr::read_unaligned(s1.add(idx as _));
                                let b: i8 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_f64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut f64).add(offset1 as _);
                            let s2 = (src2 as *mut f64).add(offset2 as _);
                            let t1 = (src3 as *mut f64).add(offset3 as _);
                            for idx in 0..count {
                                let a: f64 = ptr::read_unaligned(s1.add(idx as _));
                                let b: f64 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    fn vop_max_f32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut f32).add(offset1 as _);
                            let s2 = (src2 as *mut f32).add(offset2 as _);
                            let t1 = (src3 as *mut f32).add(offset3 as _);
                            for idx in 0..count {
                                let a: f32 = ptr::read_unaligned(s1.add(idx as _));
                                let b: f32 = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.max(b) } });
                            }
                        }
                    }
                    const _DISPATCH: [fn(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: u8,
                        offset2: u8,
                        offset3: u8,
                        count: u32,
                    ); 20] = [
                        vop_min_u64,
                        vop_min_u32,
                        vop_min_u16,
                        vop_min_u8,
                        vop_min_i64,
                        vop_min_i32,
                        vop_min_i16,
                        vop_min_i8,
                        vop_min_f64,
                        vop_min_f32,
                        vop_max_u64,
                        vop_max_u32,
                        vop_max_u16,
                        vop_max_u8,
                        vop_max_i64,
                        vop_max_i32,
                        vop_max_i16,
                        vop_max_i8,
                        vop_max_f64,
                        vop_max_f32,
                    ];
                    const TYPE_COUNT: u8 = 10;
                    #[inline(always)]
                    const fn calc_offset(op: u8, ty: u8) -> usize {
                        (op * TYPE_COUNT + ty) as _
                    }
                    pub fn call_vminimax(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        ts: *mut VMTaskState,
                    ) {
                        unsafe {
                            let VMINIMAX {
                                op,
                                flags_src1,
                                flags_src2,
                                flags_target,
                                count,
                                of_src1,
                                of_src2,
                                of_target,
                                typ,
                                ..
                            } = parse_vminimax(pickle, (*ws).arr);
                            let src1 = unsafe {
                                match flags_src1 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                match flags_src2 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let tg = unsafe {
                                match flags_target {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let offset = calc_offset(op, typ);
                            (_DISPATCH
                                .get_unchecked(
                                    offset,
                                ))(src1, src2, tg, of_src1, of_src2, of_target, count);
                        }
                    }
                }
                pub use vminimax::*;
                mod vsh {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::vsh::{VSH, parse_vsh},
                        },
                        resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
                    fn vop_shl_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shl_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shl_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shl_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shl_i64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i64).offset(offset1 as _);
                            let s2 = (src2 as *mut i64).offset(offset2 as _);
                            let s2 = s2 as *mut u64;
                            if !(size_of::<i64>() == size_of::<u64>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i64>() == size_of::<u64>()",
                                )
                            }
                            if !(align_of::<i64>() == align_of::<u64>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i64>() == align_of::<u64>()",
                                )
                            }
                            let t1 = (src3 as *mut i64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i64 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shl_i32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i32).offset(offset1 as _);
                            let s2 = (src2 as *mut i32).offset(offset2 as _);
                            let s2 = s2 as *mut u32;
                            if !(size_of::<i32>() == size_of::<u32>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i32>() == size_of::<u32>()",
                                )
                            }
                            if !(align_of::<i32>() == align_of::<u32>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i32>() == align_of::<u32>()",
                                )
                            }
                            let t1 = (src3 as *mut i32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i32 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shl_i16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i16).offset(offset1 as _);
                            let s2 = (src2 as *mut i16).offset(offset2 as _);
                            let s2 = s2 as *mut u16;
                            if !(size_of::<i16>() == size_of::<u16>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i16>() == size_of::<u16>()",
                                )
                            }
                            if !(align_of::<i16>() == align_of::<u16>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i16>() == align_of::<u16>()",
                                )
                            }
                            let t1 = (src3 as *mut i16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i16 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shl_i8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i8).offset(offset1 as _);
                            let s2 = (src2 as *mut i8).offset(offset2 as _);
                            let s2 = s2 as *mut u8;
                            if !(size_of::<i8>() == size_of::<u8>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i8>() == size_of::<u8>()",
                                )
                            }
                            if !(align_of::<i8>() == align_of::<u8>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i8>() == align_of::<u8>()",
                                )
                            }
                            let t1 = (src3 as *mut i8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i8 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shl(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_u64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u64).offset(offset1 as _);
                            let s2 = (src2 as *mut u64).offset(offset2 as _);
                            let t1 = (src3 as *mut u64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u64 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_u32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u32).offset(offset1 as _);
                            let s2 = (src2 as *mut u32).offset(offset2 as _);
                            let t1 = (src3 as *mut u32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u32 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_u16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u16).offset(offset1 as _);
                            let s2 = (src2 as *mut u16).offset(offset2 as _);
                            let t1 = (src3 as *mut u16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u16 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_u8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut u8).offset(offset1 as _);
                            let s2 = (src2 as *mut u8).offset(offset2 as _);
                            let t1 = (src3 as *mut u8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: u8 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_i64(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i64).offset(offset1 as _);
                            let s2 = (src2 as *mut i64).offset(offset2 as _);
                            let s2 = s2 as *mut u64;
                            if !(size_of::<i64>() == size_of::<u64>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i64>() == size_of::<u64>()",
                                )
                            }
                            if !(align_of::<i64>() == align_of::<u64>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i64>() == align_of::<u64>()",
                                )
                            }
                            let t1 = (src3 as *mut i64).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i64 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_i32(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i32).offset(offset1 as _);
                            let s2 = (src2 as *mut i32).offset(offset2 as _);
                            let s2 = s2 as *mut u32;
                            if !(size_of::<i32>() == size_of::<u32>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i32>() == size_of::<u32>()",
                                )
                            }
                            if !(align_of::<i32>() == align_of::<u32>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i32>() == align_of::<u32>()",
                                )
                            }
                            let t1 = (src3 as *mut i32).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i32 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_i16(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i16).offset(offset1 as _);
                            let s2 = (src2 as *mut i16).offset(offset2 as _);
                            let s2 = s2 as *mut u16;
                            if !(size_of::<i16>() == size_of::<u16>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i16>() == size_of::<u16>()",
                                )
                            }
                            if !(align_of::<i16>() == align_of::<u16>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i16>() == align_of::<u16>()",
                                )
                            }
                            let t1 = (src3 as *mut i16).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i16 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    fn vop_shr_i8(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ) {
                        unsafe {
                            let s1 = (src1 as *mut i8).offset(offset1 as _);
                            let s2 = (src2 as *mut i8).offset(offset2 as _);
                            let s2 = s2 as *mut u8;
                            if !(size_of::<i8>() == size_of::<u8>()) {
                                ::core::panicking::panic(
                                    "assertion failed: size_of::<i8>() == size_of::<u8>()",
                                )
                            }
                            if !(align_of::<i8>() == align_of::<u8>()) {
                                ::core::panicking::panic(
                                    "assertion failed: align_of::<i8>() == align_of::<u8>()",
                                )
                            }
                            let t1 = (src3 as *mut i8).offset(offset3 as _);
                            for idx in 0..count {
                                let a: i8 = ptr::read_unaligned(s1.add(idx as _));
                                let b = ptr::read_unaligned(s2.add(idx as _));
                                let t2 = t1.add(idx as _);
                                ptr::write_unaligned(t2, { { a.wrapping_shr(b as _) } });
                            }
                        }
                    }
                    const _DISPATCH: [fn(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ); 16] = [
                        vop_shl_u64,
                        vop_shl_u32,
                        vop_shl_u16,
                        vop_shl_u8,
                        vop_shl_i64,
                        vop_shl_i32,
                        vop_shl_i16,
                        vop_shl_i8,
                        vop_shr_u64,
                        vop_shr_u32,
                        vop_shr_u16,
                        vop_shr_u8,
                        vop_shr_i64,
                        vop_shr_i32,
                        vop_shr_i16,
                        vop_shr_i8,
                    ];
                    const TYPE_COUNT: u8 = 8;
                    #[inline(always)]
                    const fn calc_offset(op: u8, ty: u8) -> usize {
                        (op * TYPE_COUNT + ty) as _
                    }
                    pub fn call_vsh(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        ts: *mut VMTaskState,
                    ) {
                        unsafe {
                            let VSH {
                                op,
                                flags_src1,
                                flags_src2,
                                flags_target,
                                count,
                                of_src1,
                                of_src2,
                                of_target,
                                typ,
                            } = parse_vsh(pickle, &(&(*ws).arr));
                            let src1 = unsafe {
                                match flags_src1 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                match flags_src2 {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let tg = unsafe {
                                match flags_target {
                                    0 => &raw mut (*ts).r1,
                                    1 => &raw mut (*ts).r2,
                                    2 => &raw mut (*ts).r3,
                                    3 => &raw mut (*ts).r4,
                                    4 => &raw mut (*ts).r5,
                                    5 => &raw mut (*ts).r6,
                                    6 => &raw mut (*ts).r7,
                                    7 => &raw mut (*ts).r8,
                                    8 => (*ts).scratchpad,
                                    9 => (*ts).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*ts).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let offset = calc_offset(op, typ);
                            (_DISPATCH
                                .get_unchecked(
                                    offset,
                                ))(
                                src1,
                                src2,
                                tg,
                                of_src1 as _,
                                of_src2 as _,
                                of_target as _,
                                count,
                            );
                        }
                    }
                }
                pub use vsh::*;
                mod vau {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::au::{ARITH, DIVLIKE, parse_arith, parse_divlike},
                        },
                        ints::{IIntImpl, WideningMul},
                        resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr::{self, addr_of_mut};
                    #[inline(always)]
                    fn arithprelude(
                        ws: *mut WorkingSet,
                        task: *mut VMTaskState,
                    ) -> (
                        u16,
                        u8,
                        u32,
                        *mut QuadPackedData,
                        *mut QuadPackedData,
                        *mut QuadPackedData,
                        i32,
                        i32,
                        i32,
                    ) {
                        let ARITH {
                            datatype,
                            count,
                            instdefined,
                            src1,
                            of_src1,
                            src2,
                            of_src2,
                            tgt,
                            of_tgt,
                        } = parse_arith(unsafe { (*ws).arr });
                        let src1 = {
                            unsafe {
                                match src1 {
                                    0 => &raw mut (*task).r1,
                                    1 => &raw mut (*task).r2,
                                    2 => &raw mut (*task).r3,
                                    3 => &raw mut (*task).r4,
                                    4 => &raw mut (*task).r5,
                                    5 => &raw mut (*task).r6,
                                    6 => &raw mut (*task).r7,
                                    7 => &raw mut (*task).r8,
                                    8 => (*task).scratchpad,
                                    9 => (*task).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*task).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            }
                        };
                        let src2 = {
                            unsafe {
                                match src2 {
                                    0 => &raw mut (*task).r1,
                                    1 => &raw mut (*task).r2,
                                    2 => &raw mut (*task).r3,
                                    3 => &raw mut (*task).r4,
                                    4 => &raw mut (*task).r5,
                                    5 => &raw mut (*task).r6,
                                    6 => &raw mut (*task).r7,
                                    7 => &raw mut (*task).r8,
                                    8 => (*task).scratchpad,
                                    9 => (*task).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*task).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            }
                        };
                        let target = {
                            unsafe {
                                match tgt {
                                    0 => &raw mut (*task).r1,
                                    1 => &raw mut (*task).r2,
                                    2 => &raw mut (*task).r3,
                                    3 => &raw mut (*task).r4,
                                    4 => &raw mut (*task).r5,
                                    5 => &raw mut (*task).r6,
                                    6 => &raw mut (*task).r7,
                                    7 => &raw mut (*task).r8,
                                    8 => (*task).scratchpad,
                                    9 => (*task).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*task).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            }
                        };
                        (
                            instdefined,
                            datatype,
                            count,
                            src1,
                            src2,
                            target,
                            of_src1,
                            of_src2,
                            of_tgt,
                        )
                    }
                    #[inline(always)]
                    pub fn call_vadd(
                        _: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (
                            instdefined,
                            typetag,
                            count,
                            src1,
                            src2,
                            target,
                            t1,
                            t2,
                            t3,
                        ) = arithprelude(ws, taskstate);
                        let carry = (instdefined >> 15) == 1;
                        let saturate = (instdefined >> 14 & 0b01) == 1;
                        if true {
                            if !!(carry && saturate) {
                                ::core::panicking::panic(
                                    "assertion failed: !(carry && saturate)",
                                )
                            }
                        }
                        if true {
                            if !(count != 0) {
                                ::core::panicking::panic("assertion failed: count != 0")
                            }
                        }
                        if true {
                            if !!(carry && count != 1) {
                                ::core::panicking::panic(
                                    "assertion failed: !(carry && count != 1)",
                                )
                            }
                        }
                        unsafe {
                            match (carry, saturate, typetag) {
                                (true, _, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let t = (target as *mut u64).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u64).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u64).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u64,
                                                ) != 0;
                                                let output = (s1).carrying_add(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u64),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let t = (target as *mut u32).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u32).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u32).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u32,
                                                ) != 0;
                                                let output = (s1).carrying_add(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u32),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let t = (target as *mut u16).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u16).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u16).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u16,
                                                ) != 0;
                                                let output = (s1).carrying_add(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u16),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let t = (target as *mut u8).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u8).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u8).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u8,
                                                ) != 0;
                                                let output = (s1).carrying_add(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u8),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let t = (target as *mut i64).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i64).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i64).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i64,
                                                ) != 0;
                                                let output = (s1).carryadd(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i64),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let t = (target as *mut i32).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i32).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i32).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i32,
                                                ) != 0;
                                                let output = (s1).carryadd(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i32),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let t = (target as *mut i16).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i16).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i16).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i16,
                                                ) != 0;
                                                let output = (s1).carryadd(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i16),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let t = (target as *mut i8).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i8).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i8).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i8,
                                                ) != 0;
                                                let output = (s1).carryadd(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i8),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        _ => ::core::panicking::panic("not yet implemented"),
                                    }
                                }
                                (_, true, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let target = (target as *mut u64).offset(t3 as _);
                                                let s1_ = (src1 as *mut u64).offset(t1 as _);
                                                let s2_ = (src2 as *mut u64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let target = (target as *mut u32).offset(t3 as _);
                                                let s1_ = (src1 as *mut u32).offset(t1 as _);
                                                let s2_ = (src2 as *mut u32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let target = (target as *mut u16).offset(t3 as _);
                                                let s1_ = (src1 as *mut u16).offset(t1 as _);
                                                let s2_ = (src2 as *mut u16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let target = (target as *mut u8).offset(t3 as _);
                                                let s1_ = (src1 as *mut u8).offset(t1 as _);
                                                let s2_ = (src2 as *mut u8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let target = (target as *mut i64).offset(t3 as _);
                                                let s1_ = (src1 as *mut i64).offset(t1 as _);
                                                let s2_ = (src2 as *mut i64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let target = (target as *mut i32).offset(t3 as _);
                                                let s1_ = (src1 as *mut i32).offset(t1 as _);
                                                let s2_ = (src2 as *mut i32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let target = (target as *mut i16).offset(t3 as _);
                                                let s1_ = (src1 as *mut i16).offset(t1 as _);
                                                let s2_ = (src2 as *mut i16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let target = (target as *mut i8).offset(t3 as _);
                                                let s1_ = (src1 as *mut i8).offset(t1 as _);
                                                let s2_ = (src2 as *mut i8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_add(s2));
                                                }
                                            }
                                        }
                                        _ => ::core::panicking::panic("explicit panic"),
                                    }
                                }
                                (_, _, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let target = (target as *mut u64).offset(t3 as _);
                                                let s1_ = (src1 as *mut u64).offset(t1 as _);
                                                let s2_ = (src2 as *mut u64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let target = (target as *mut u32).offset(t3 as _);
                                                let s1_ = (src1 as *mut u32).offset(t1 as _);
                                                let s2_ = (src2 as *mut u32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let target = (target as *mut u16).offset(t3 as _);
                                                let s1_ = (src1 as *mut u16).offset(t1 as _);
                                                let s2_ = (src2 as *mut u16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let target = (target as *mut u8).offset(t3 as _);
                                                let s1_ = (src1 as *mut u8).offset(t1 as _);
                                                let s2_ = (src2 as *mut u8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let target = (target as *mut i64).offset(t3 as _);
                                                let s1_ = (src1 as *mut i64).offset(t1 as _);
                                                let s2_ = (src2 as *mut i64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let target = (target as *mut i32).offset(t3 as _);
                                                let s1_ = (src1 as *mut i32).offset(t1 as _);
                                                let s2_ = (src2 as *mut i32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let target = (target as *mut i16).offset(t3 as _);
                                                let s1_ = (src1 as *mut i16).offset(t1 as _);
                                                let s2_ = (src2 as *mut i16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let target = (target as *mut i8).offset(t3 as _);
                                                let s1_ = (src1 as *mut i8).offset(t1 as _);
                                                let s2_ = (src2 as *mut i8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_add(s2));
                                                }
                                            }
                                        }
                                        _ => ::core::panicking::panic("explicit panic"),
                                    }
                                }
                            }
                        }
                    }
                    #[inline(always)]
                    pub fn call_vsub(
                        _: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (
                            instdefined,
                            typetag,
                            count,
                            src1,
                            src2,
                            target,
                            t1,
                            t2,
                            t3,
                        ) = arithprelude(ws, taskstate);
                        let carry = (instdefined >> 15) == 1;
                        let saturate = (instdefined >> 14 & 0b01) == 1;
                        if true {
                            if !!(carry && saturate) {
                                ::core::panicking::panic(
                                    "assertion failed: !(carry && saturate)",
                                )
                            }
                        }
                        if true {
                            if !(count != 0) {
                                ::core::panicking::panic("assertion failed: count != 0")
                            }
                        }
                        if true {
                            if !!(carry && count != 1) {
                                ::core::panicking::panic(
                                    "assertion failed: !(carry && count != 1)",
                                )
                            }
                        }
                        unsafe {
                            match (carry, saturate, typetag) {
                                (true, _, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let t = (target as *mut u64).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u64).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u64).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u64,
                                                ) != 0;
                                                let output = (s1).borrowing_sub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u64),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let t = (target as *mut u32).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u32).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u32).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u32,
                                                ) != 0;
                                                let output = (s1).borrowing_sub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u32),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let t = (target as *mut u16).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u16).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u16).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u16,
                                                ) != 0;
                                                let output = (s1).borrowing_sub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u16),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let t = (target as *mut u8).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut u8).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut u8).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut u8,
                                                ) != 0;
                                                let output = (s1).borrowing_sub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut u8),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let t = (target as *mut i64).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i64).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i64).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i64,
                                                ) != 0;
                                                let output = (s1).borrowsub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i64),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let t = (target as *mut i32).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i32).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i32).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i32,
                                                ) != 0;
                                                let output = (s1).borrowsub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i32),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let t = (target as *mut i16).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i16).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i16).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i16,
                                                ) != 0;
                                                let output = (s1).borrowsub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i16),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let t = (target as *mut i8).offset(t3 as _);
                                                let s1 = ptr::read_unaligned(
                                                    (src1 as *mut i8).offset(t1 as _),
                                                );
                                                let s2 = ptr::read_unaligned(
                                                    (src2 as *mut i8).offset(t2 as _),
                                                );
                                                let carry = ptr::read_unaligned(
                                                    &raw mut (*taskstate).r5 as *mut i8,
                                                ) != 0;
                                                let output = (s1).borrowsub(s2, carry);
                                                ptr::write_unaligned(t, output.0);
                                                ptr::write_unaligned(
                                                    (&raw mut (*taskstate).r5 as *mut i8),
                                                    if output.1 { !0 } else { 0 },
                                                );
                                            }
                                        }
                                        _ => ::core::panicking::panic("not yet implemented"),
                                    }
                                }
                                (_, true, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let target = (target as *mut u64).offset(t3 as _);
                                                let s1_ = (src1 as *mut u64).offset(t1 as _);
                                                let s2_ = (src2 as *mut u64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let target = (target as *mut u32).offset(t3 as _);
                                                let s1_ = (src1 as *mut u32).offset(t1 as _);
                                                let s2_ = (src2 as *mut u32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let target = (target as *mut u16).offset(t3 as _);
                                                let s1_ = (src1 as *mut u16).offset(t1 as _);
                                                let s2_ = (src2 as *mut u16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let target = (target as *mut u8).offset(t3 as _);
                                                let s1_ = (src1 as *mut u8).offset(t1 as _);
                                                let s2_ = (src2 as *mut u8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let target = (target as *mut i64).offset(t3 as _);
                                                let s1_ = (src1 as *mut i64).offset(t1 as _);
                                                let s2_ = (src2 as *mut i64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let target = (target as *mut i32).offset(t3 as _);
                                                let s1_ = (src1 as *mut i32).offset(t1 as _);
                                                let s2_ = (src2 as *mut i32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let target = (target as *mut i16).offset(t3 as _);
                                                let s1_ = (src1 as *mut i16).offset(t1 as _);
                                                let s2_ = (src2 as *mut i16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let target = (target as *mut i8).offset(t3 as _);
                                                let s1_ = (src1 as *mut i8).offset(t1 as _);
                                                let s2_ = (src2 as *mut i8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.saturating_sub(s2));
                                                }
                                            }
                                        }
                                        _ => ::core::panicking::panic("explicit panic"),
                                    }
                                }
                                (_, _, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let target = (target as *mut u64).offset(t3 as _);
                                                let s1_ = (src1 as *mut u64).offset(t1 as _);
                                                let s2_ = (src2 as *mut u64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let target = (target as *mut u32).offset(t3 as _);
                                                let s1_ = (src1 as *mut u32).offset(t1 as _);
                                                let s2_ = (src2 as *mut u32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let target = (target as *mut u16).offset(t3 as _);
                                                let s1_ = (src1 as *mut u16).offset(t1 as _);
                                                let s2_ = (src2 as *mut u16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let target = (target as *mut u8).offset(t3 as _);
                                                let s1_ = (src1 as *mut u8).offset(t1 as _);
                                                let s2_ = (src2 as *mut u8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let target = (target as *mut i64).offset(t3 as _);
                                                let s1_ = (src1 as *mut i64).offset(t1 as _);
                                                let s2_ = (src2 as *mut i64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let target = (target as *mut i32).offset(t3 as _);
                                                let s1_ = (src1 as *mut i32).offset(t1 as _);
                                                let s2_ = (src2 as *mut i32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let target = (target as *mut i16).offset(t3 as _);
                                                let s1_ = (src1 as *mut i16).offset(t1 as _);
                                                let s2_ = (src2 as *mut i16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let target = (target as *mut i8).offset(t3 as _);
                                                let s1_ = (src1 as *mut i8).offset(t1 as _);
                                                let s2_ = (src2 as *mut i8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_sub(s2));
                                                }
                                            }
                                        }
                                        _ => ::core::panicking::panic("explicit panic"),
                                    }
                                }
                            }
                        }
                    }
                    #[inline(always)]
                    pub fn call_vmul(
                        _: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (
                            instdefined,
                            typetag,
                            count,
                            src1,
                            src2,
                            target,
                            t1,
                            t2,
                            t3,
                        ) = arithprelude(ws, taskstate);
                        let eflags = (instdefined >> 14) as u8;
                        let wide = (eflags & 0x03) == 1;
                        let lowbits = (eflags & 0x01) == 0;
                        if true {
                            if !(count != 0) {
                                ::core::panicking::panic("assertion failed: count != 0")
                            }
                        }
                        unsafe {
                            match (wide, lowbits, typetag) {
                                (true, _, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let target = (target as *mut u64).offset(t3 as _);
                                                let s1_ = (src1 as *mut u64).offset(t1 as _);
                                                let s2_ = (src2 as *mut u64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let target = (target as *mut u32).offset(t3 as _);
                                                let s1_ = (src1 as *mut u32).offset(t1 as _);
                                                let s2_ = (src2 as *mut u32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let target = (target as *mut u16).offset(t3 as _);
                                                let s1_ = (src1 as *mut u16).offset(t1 as _);
                                                let s2_ = (src2 as *mut u16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let target = (target as *mut u8).offset(t3 as _);
                                                let s1_ = (src1 as *mut u8).offset(t1 as _);
                                                let s2_ = (src2 as *mut u8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let target = (target as *mut i64).offset(t3 as _);
                                                let s1_ = (src1 as *mut i64).offset(t1 as _);
                                                let s2_ = (src2 as *mut i64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let target = (target as *mut i32).offset(t3 as _);
                                                let s1_ = (src1 as *mut i32).offset(t1 as _);
                                                let s2_ = (src2 as *mut i32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let target = (target as *mut i16).offset(t3 as _);
                                                let s1_ = (src1 as *mut i16).offset(t1 as _);
                                                let s2_ = (src2 as *mut i16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let target = (target as *mut i8).offset(t3 as _);
                                                let s1_ = (src1 as *mut i8).offset(t1 as _);
                                                let s2_ = (src2 as *mut i8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t_1 = target.add(2 * (i as usize));
                                                    let t_2 = target.add(2 * (i as usize) + 1);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (a, b) = (s1).mul_widen(s2);
                                                    ptr::write_unaligned(t_1, a as _);
                                                    ptr::write_unaligned(t_2, b);
                                                }
                                            }
                                        }
                                        _ => ::core::panicking::panic("not yet implemented"),
                                    }
                                }
                                (_, true, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let target = (target as *mut u64).offset(t3 as _);
                                                let s1_ = (src1 as *mut u64).offset(t1 as _);
                                                let s2_ = (src2 as *mut u64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let target = (target as *mut u32).offset(t3 as _);
                                                let s1_ = (src1 as *mut u32).offset(t1 as _);
                                                let s2_ = (src2 as *mut u32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let target = (target as *mut u16).offset(t3 as _);
                                                let s1_ = (src1 as *mut u16).offset(t1 as _);
                                                let s2_ = (src2 as *mut u16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let target = (target as *mut u8).offset(t3 as _);
                                                let s1_ = (src1 as *mut u8).offset(t1 as _);
                                                let s2_ = (src2 as *mut u8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let target = (target as *mut i64).offset(t3 as _);
                                                let s1_ = (src1 as *mut i64).offset(t1 as _);
                                                let s2_ = (src2 as *mut i64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let target = (target as *mut i32).offset(t3 as _);
                                                let s1_ = (src1 as *mut i32).offset(t1 as _);
                                                let s2_ = (src2 as *mut i32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let target = (target as *mut i16).offset(t3 as _);
                                                let s1_ = (src1 as *mut i16).offset(t1 as _);
                                                let s2_ = (src2 as *mut i16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let target = (target as *mut i8).offset(t3 as _);
                                                let s1_ = (src1 as *mut i8).offset(t1 as _);
                                                let s2_ = (src2 as *mut i8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    ptr::write_unaligned(t, s1.wrapping_mul(s2));
                                                }
                                            }
                                        }
                                        _ => ::core::panicking::panic("explicit panic"),
                                    }
                                }
                                (_, _, tag) => {
                                    match tag {
                                        0 => {
                                            unsafe {
                                                let target = (target as *mut u64).offset(t3 as _);
                                                let s1_ = (src1 as *mut u64).offset(t1 as _);
                                                let s2_ = (src2 as *mut u64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        1 => {
                                            unsafe {
                                                let target = (target as *mut u32).offset(t3 as _);
                                                let s1_ = (src1 as *mut u32).offset(t1 as _);
                                                let s2_ = (src2 as *mut u32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        2 => {
                                            unsafe {
                                                let target = (target as *mut u16).offset(t3 as _);
                                                let s1_ = (src1 as *mut u16).offset(t1 as _);
                                                let s2_ = (src2 as *mut u16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        3 => {
                                            unsafe {
                                                let target = (target as *mut u8).offset(t3 as _);
                                                let s1_ = (src1 as *mut u8).offset(t1 as _);
                                                let s2_ = (src2 as *mut u8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        4 => {
                                            unsafe {
                                                let target = (target as *mut i64).offset(t3 as _);
                                                let s1_ = (src1 as *mut i64).offset(t1 as _);
                                                let s2_ = (src2 as *mut i64).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        5 => {
                                            unsafe {
                                                let target = (target as *mut i32).offset(t3 as _);
                                                let s1_ = (src1 as *mut i32).offset(t1 as _);
                                                let s2_ = (src2 as *mut i32).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        6 => {
                                            unsafe {
                                                let target = (target as *mut i16).offset(t3 as _);
                                                let s1_ = (src1 as *mut i16).offset(t1 as _);
                                                let s2_ = (src2 as *mut i16).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        7 => {
                                            unsafe {
                                                let target = (target as *mut i8).offset(t3 as _);
                                                let s1_ = (src1 as *mut i8).offset(t1 as _);
                                                let s2_ = (src2 as *mut i8).offset(t2 as _);
                                                for i in 0..count {
                                                    let t = target.add(i as _);
                                                    let s1 = ptr::read_unaligned(s1_.add(i as _));
                                                    let s2 = ptr::read_unaligned(s2_.add(i as _));
                                                    let (_, b) = s1.mul_widen(s2);
                                                    ptr::write_unaligned(t, b);
                                                }
                                            }
                                        }
                                        _ => ::core::panicking::panic("explicit panic"),
                                    }
                                }
                            }
                        }
                    }
                    pub fn divlike(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        task: *mut VMTaskState,
                    ) -> (
                        u8,
                        *mut QuadPackedData,
                        *mut QuadPackedData,
                        *mut QuadPackedData,
                        i32,
                        i32,
                        i32,
                    ) {
                        let DIVLIKE {
                            datatype,
                            src1,
                            src2,
                            tgt,
                            of_src1,
                            of_src2,
                            of_tgt,
                        } = parse_divlike(pickle, unsafe { (*ws).arr });
                        let src1 = unsafe {
                            match src1 {
                                0 => &raw mut (*task).r1,
                                1 => &raw mut (*task).r2,
                                2 => &raw mut (*task).r3,
                                3 => &raw mut (*task).r4,
                                4 => &raw mut (*task).r5,
                                5 => &raw mut (*task).r6,
                                6 => &raw mut (*task).r7,
                                7 => &raw mut (*task).r8,
                                8 => (*task).scratchpad,
                                9 => (*task).largepad,
                                #[allow(unused_unsafe)]
                                10 => unsafe { (*task).r2.selfref }
                                #[allow(unreachable_patterns)]
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let src2 = unsafe {
                            match src2 {
                                0 => &raw mut (*task).r1,
                                1 => &raw mut (*task).r2,
                                2 => &raw mut (*task).r3,
                                3 => &raw mut (*task).r4,
                                4 => &raw mut (*task).r5,
                                5 => &raw mut (*task).r6,
                                6 => &raw mut (*task).r7,
                                7 => &raw mut (*task).r8,
                                8 => (*task).scratchpad,
                                9 => (*task).largepad,
                                #[allow(unused_unsafe)]
                                10 => unsafe { (*task).r2.selfref }
                                #[allow(unreachable_patterns)]
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target = unsafe {
                            match tgt {
                                0 => &raw mut (*task).r1,
                                1 => &raw mut (*task).r2,
                                2 => &raw mut (*task).r3,
                                3 => &raw mut (*task).r4,
                                4 => &raw mut (*task).r5,
                                5 => &raw mut (*task).r6,
                                6 => &raw mut (*task).r7,
                                7 => &raw mut (*task).r8,
                                8 => (*task).scratchpad,
                                9 => (*task).largepad,
                                #[allow(unused_unsafe)]
                                10 => unsafe { (*task).r2.selfref }
                                #[allow(unreachable_patterns)]
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        (datatype, src1, src2, target, of_src1, of_src2, of_tgt)
                    }
                    #[inline(always)]
                    pub fn call_div(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (typetag, src1, src2, target, t1, t2, t3) = divlike(
                            pickle,
                            ws,
                            taskstate,
                        );
                        let count = 1;
                        match typetag {
                            0 => {
                                unsafe {
                                    let target = (target as *mut u64).offset(t3 as _);
                                    let s1_ = (src1 as *mut u64).offset(t1 as _);
                                    let s2_ = (src2 as *mut u64).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            1 => {
                                unsafe {
                                    let target = (target as *mut u32).offset(t3 as _);
                                    let s1_ = (src1 as *mut u32).offset(t1 as _);
                                    let s2_ = (src2 as *mut u32).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            2 => {
                                unsafe {
                                    let target = (target as *mut u16).offset(t3 as _);
                                    let s1_ = (src1 as *mut u16).offset(t1 as _);
                                    let s2_ = (src2 as *mut u16).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            3 => {
                                unsafe {
                                    let target = (target as *mut u8).offset(t3 as _);
                                    let s1_ = (src1 as *mut u8).offset(t1 as _);
                                    let s2_ = (src2 as *mut u8).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            4 => {
                                unsafe {
                                    let target = (target as *mut i64).offset(t3 as _);
                                    let s1_ = (src1 as *mut i64).offset(t1 as _);
                                    let s2_ = (src2 as *mut i64).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            5 => {
                                unsafe {
                                    let target = (target as *mut i32).offset(t3 as _);
                                    let s1_ = (src1 as *mut i32).offset(t1 as _);
                                    let s2_ = (src2 as *mut i32).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            6 => {
                                unsafe {
                                    let target = (target as *mut i16).offset(t3 as _);
                                    let s1_ = (src1 as *mut i16).offset(t1 as _);
                                    let s2_ = (src2 as *mut i16).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            7 => {
                                unsafe {
                                    let target = (target as *mut i8).offset(t3 as _);
                                    let s1_ = (src1 as *mut i8).offset(t1 as _);
                                    let s2_ = (src2 as *mut i8).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_div(s2));
                                    }
                                }
                            }
                            _ => ::core::panicking::panic("explicit panic"),
                        }
                    }
                    #[inline(always)]
                    pub fn call_rem(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        let (typetag, src1, src2, target, t1, t2, t3) = divlike(
                            pickle,
                            ws,
                            taskstate,
                        );
                        let count = 1;
                        match typetag {
                            0 => {
                                unsafe {
                                    let target = (target as *mut u64).offset(t3 as _);
                                    let s1_ = (src1 as *mut u64).offset(t1 as _);
                                    let s2_ = (src2 as *mut u64).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            1 => {
                                unsafe {
                                    let target = (target as *mut u32).offset(t3 as _);
                                    let s1_ = (src1 as *mut u32).offset(t1 as _);
                                    let s2_ = (src2 as *mut u32).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            2 => {
                                unsafe {
                                    let target = (target as *mut u16).offset(t3 as _);
                                    let s1_ = (src1 as *mut u16).offset(t1 as _);
                                    let s2_ = (src2 as *mut u16).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            3 => {
                                unsafe {
                                    let target = (target as *mut u8).offset(t3 as _);
                                    let s1_ = (src1 as *mut u8).offset(t1 as _);
                                    let s2_ = (src2 as *mut u8).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            4 => {
                                unsafe {
                                    let target = (target as *mut i64).offset(t3 as _);
                                    let s1_ = (src1 as *mut i64).offset(t1 as _);
                                    let s2_ = (src2 as *mut i64).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            5 => {
                                unsafe {
                                    let target = (target as *mut i32).offset(t3 as _);
                                    let s1_ = (src1 as *mut i32).offset(t1 as _);
                                    let s2_ = (src2 as *mut i32).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            6 => {
                                unsafe {
                                    let target = (target as *mut i16).offset(t3 as _);
                                    let s1_ = (src1 as *mut i16).offset(t1 as _);
                                    let s2_ = (src2 as *mut i16).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            7 => {
                                unsafe {
                                    let target = (target as *mut i8).offset(t3 as _);
                                    let s1_ = (src1 as *mut i8).offset(t1 as _);
                                    let s2_ = (src2 as *mut i8).offset(t2 as _);
                                    for i in 0..count {
                                        let t = target.add(i as _);
                                        let s1 = ptr::read_unaligned(s1_.add(i as _));
                                        let s2 = ptr::read_unaligned(s2_.add(i as _));
                                        ptr::write_unaligned(t, s1.strict_rem(s2));
                                    }
                                }
                            }
                            _ => ::core::panicking::panic("explicit panic"),
                        }
                    }
                }
                pub use vau::*;
                pub fn call_scratch(
                    pickle: &PickleInstruction,
                    ws: *mut WorkingSet,
                    taskstate: *mut VMTaskState,
                ) {
                    let scratch = parse_scratch(pickle, unsafe { (*ws).arr }.as_ref());
                    match scratch {
                        SCRATCH::Allocate { size_reg, align_reg } => {
                            unsafe {
                                let size = unsafe {
                                    match size_reg {
                                        0 => (*taskstate).r1,
                                        1 => (*taskstate).r2,
                                        2 => (*taskstate).r3,
                                        3 => (*taskstate).r4,
                                        4 => (*taskstate).r5,
                                        5 => (*taskstate).r6,
                                        6 => (*taskstate).r7,
                                        7 => (*taskstate).r8,
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                                    .u64 as usize;
                                let align = unsafe {
                                    match align_reg {
                                        0 => (*taskstate).r1,
                                        1 => (*taskstate).r2,
                                        2 => (*taskstate).r3,
                                        3 => (*taskstate).r4,
                                        4 => (*taskstate).r5,
                                        5 => (*taskstate).r6,
                                        6 => (*taskstate).r7,
                                        7 => (*taskstate).r8,
                                        _ => ::core::panicking::panic("not implemented"),
                                    }
                                }
                                    .u64 as usize;
                                if true {
                                    if !(*taskstate).largepad.is_null() {
                                        ::core::panicking::panic(
                                            "assertion failed: (*taskstate).largepad.is_null()",
                                        )
                                    }
                                }
                                if true {
                                    if !(align == 0 || align.is_power_of_two()) {
                                        ::core::panicking::panic(
                                            "assertion failed: align == 0 || align.is_power_of_two()",
                                        )
                                    }
                                }
                                (*taskstate).largepad = (*ws).allocate(size, align);
                            }
                        }
                        SCRATCH::DropClassic => {
                            unsafe {
                                let pt = (*taskstate).largepad;
                                (*taskstate).largepad = null_mut();
                                (*ws).free(pt);
                            }
                        }
                        SCRATCH::DropAligned => {
                            unsafe {
                                let pt = (*taskstate).largepad;
                                (*taskstate).largepad = null_mut();
                                (*ws).salloc_free(pt);
                            }
                        }
                    }
                }
            }
            pub use almu::*;
            mod threading {
                use std::{
                    iter, mem::{transmute, zeroed},
                    ptr::{self, addr_of_mut, null_mut},
                };
                use sart::structures::ffi::{
                    LFFITypeMap, VReg,
                    libffi_sys::{
                        FFI_TYPE_STRUCT, ffi_abi_FFI_DEFAULT_ABI, ffi_call, ffi_cif,
                        ffi_prep_cif, ffi_type, ffi_type_uint8, ffi_type_uint16,
                        ffi_type_uint32, ffi_type_uint64, ffi_type_void,
                    },
                };
                use sart::{
                    ctr::{CVMTaskState, VMTaskState},
                    structures::ffi::{COut, CallSig},
                };
                use crate::{
                    CODE_CACHE, FNCALL_DISPATCH, SymbolMapTable, SymbolMapTableInfo,
                    ThreadSafe, VM,
                    acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
                    arrcastint, resolve_location_src,
                };
                mod spawn {
                    use std::{
                        ffi::c_void, ptr::{self, null_mut},
                        thread::spawn,
                    };
                    use sart::ctr::VMTaskState;
                    use crate::{
                        GLOBAL_RUNTIME, ThreadSafe, VM,
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                            reader::spawn::{SPAWN, parse_spawn},
                        },
                        resolve_location_src,
                    };
                    pub extern "C" fn savm_spawn(
                        taskstate: *mut VMTaskState,
                        section: u64,
                        launch_async: bool,
                        return_hwnd: bool,
                    ) -> *mut c_void {
                        unsafe {
                            let safe_taskstate = ThreadSafe(taskstate);
                            let vm = ThreadSafe((*taskstate).engine_or_pt.pt as *mut VM);
                            if launch_async {
                                let tokiort = GLOBAL_RUNTIME
                                    .spawn(async move {
                                        let _vm = vm;
                                        let _taskstate = safe_taskstate;
                                        {
                                            ::core::panicking::panic_fmt(
                                                format_args!(
                                                    "not yet implemented: {0}",
                                                    format_args!("Add Async fncall"),
                                                ),
                                            );
                                        };
                                    });
                                if return_hwnd {
                                    let rtptr = Box::into_raw(Box::new(tokiort));
                                    return rtptr as _;
                                }
                            } else {
                                let stdrt = spawn(move || {
                                    let vm = vm;
                                    let taskstate = safe_taskstate;
                                    let [r7, r8] = (*vm.0).fncall(section, taskstate.0);
                                    (r7.u64, r8.u64)
                                });
                                if return_hwnd {
                                    let rtptr = Box::into_raw(Box::new(stdrt));
                                    return rtptr as _;
                                }
                            };
                        };
                        null_mut()
                    }
                    pub fn call_spawn(
                        pickle: &PickleInstruction,
                        ws: *mut WorkingSet,
                        taskstate: *mut VMTaskState,
                    ) {
                        unsafe {
                            let SPAWN {
                                launch_as_async,
                                out_loc,
                                return_hwnd,
                                section,
                            } = parse_spawn(pickle, (*ws).arr.as_ref());
                            let hwnd = unsafe {
                                match out_loc {
                                    0 => &raw mut (*taskstate).r1,
                                    1 => &raw mut (*taskstate).r2,
                                    2 => &raw mut (*taskstate).r3,
                                    3 => &raw mut (*taskstate).r4,
                                    4 => &raw mut (*taskstate).r5,
                                    5 => &raw mut (*taskstate).r6,
                                    6 => &raw mut (*taskstate).r7,
                                    7 => &raw mut (*taskstate).r8,
                                    8 => (*taskstate).scratchpad,
                                    9 => (*taskstate).largepad,
                                    #[allow(unused_unsafe)]
                                    10 => unsafe { (*taskstate).r2.selfref }
                                    #[allow(unreachable_patterns)]
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let newhwnd = savm_spawn(
                                taskstate,
                                section,
                                launch_as_async,
                                return_hwnd,
                            );
                            if !newhwnd.is_null() {
                                ptr::write(hwnd as *mut *mut c_void, newhwnd);
                            }
                        }
                    }
                }
                pub use spawn::*;
                struct NativeAsyncExecutor;
                pub const EXEC: ::std::thread::LocalKey<NativeAsyncExecutor> = {
                    #[inline]
                    fn __rust_std_internal_init_fn() -> NativeAsyncExecutor {
                        NativeAsyncExecutor
                    }
                    unsafe {
                        ::std::thread::LocalKey::new(const {
                            if ::std::mem::needs_drop::<NativeAsyncExecutor>() {
                                |__rust_std_internal_init| {
                                    #[thread_local]
                                    static __RUST_STD_INTERNAL_VAL: ::std::thread::local_impl::LazyStorage<
                                        NativeAsyncExecutor,
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
                                        NativeAsyncExecutor,
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
                pub extern "C" fn ffi_synccall_sectionid(
                    taskstate: *mut VMTaskState,
                    sectionid: u64,
                ) {
                    unsafe {
                        let vm = (*taskstate).engine_or_pt.pt as *const _ as *const VM;
                        let [r7, r8] = (*vm).fncall(sectionid, taskstate);
                        (*taskstate).r7 = r7;
                        (*taskstate).r8 = r8;
                    }
                }
                pub extern "C" fn ffi_libcall_sectionid(
                    taskstate: *mut VMTaskState,
                    sectionid: u64,
                ) {
                    {
                        let (v, cdecl) = FNCALL_DISPATCH
                            .get()
                            .unwrap()
                            .get(&sectionid)
                            .unwrap();
                        run_cdecl(v.0, cdecl, taskstate)
                    }
                }
                pub fn call_synccall(
                    _: &PickleInstruction,
                    ws: *mut WorkingSet,
                    taskstate: *mut VMTaskState,
                ) {
                    let sectionid = {
                        #[allow(unused_unsafe)]
                        <u64>::from_ne_bytes(unsafe {
                            (&(*ws).arr)[0..8].try_into().unwrap_unchecked()
                        })
                    };
                    unsafe {
                        let vm = (*taskstate).engine_or_pt.pt as *const _ as *const VM;
                        let tskptr = taskstate as *mut _;
                        let dispatch = || {
                            let [r7, r8] = (*vm).fncall(sectionid, taskstate);
                            (*taskstate).r7 = r7;
                            (*taskstate).r8 = r8;
                        };
                        if CODE_CACHE.contains_key(&sectionid) {
                            return dispatch();
                        }
                        return match (*vm).resolve.as_ref().learn_data(sectionid) {
                            SymbolMapTableInfo::MixedSizedBytecode => dispatch(),
                            SymbolMapTableInfo::NativePointer => {
                                FNCALL_DISPATCH
                                    .get()
                                    .map_or_else(
                                        || {
                                            match (*vm).resolve.as_ref().resolve_data(sectionid) {
                                                SymbolMapTable::NativePointer { fnptr, cdecl } => {
                                                    run_cdecl(fnptr, &cdecl, tskptr)
                                                }
                                                _ => {
                                                    ::core::panicking::panic(
                                                        "internal error: entered unreachable code",
                                                    )
                                                }
                                            }
                                        },
                                        |x| {
                                            let (fnptr, cdecl) = x
                                                .get(&sectionid)
                                                .expect(
                                                    "SaVM Error - Uncached Library Call in cached area. This is a error with SaVM and no amount of bytecode patching can rectify it",
                                                );
                                            run_cdecl(fnptr.0, cdecl, tskptr)
                                        },
                                    );
                            }
                        };
                    }
                }
                static mut BITS128_ELEMENTS: [*mut ffi_type; 3] = {
                    [&raw mut ffi_type_uint64, &raw mut ffi_type_uint64, null_mut()]
                };
                static FFI_TYPE_BITS128: ThreadSafe<ffi_type> = ThreadSafe(ffi_type {
                    size: 0,
                    alignment: 0,
                    type_: FFI_TYPE_STRUCT as u16,
                    elements: { &raw mut BITS128_ELEMENTS as *mut _ },
                });
                fn run_cdecl(
                    fnptr: *const (),
                    cdecl: &CallSig,
                    taskstate: *mut VMTaskState,
                ) {
                    match cdecl {
                        CallSig::SaFFI(_) => {
                            unsafe {
                                let fcall: extern "C" fn(*mut CVMTaskState) = transmute(
                                    fnptr,
                                );
                                fcall(taskstate as _);
                            }
                        }
                        CallSig::CDef(cdef) => {
                            unsafe {
                                let mut bits128 = FFI_TYPE_BITS128.0;
                                let out_bytes = cdef.out.width();
                                let output = match cdef.out {
                                    COut::Void => &raw mut ffi_type_void,
                                    COut::Bits8 => &raw mut ffi_type_uint8,
                                    COut::Bits16 => &raw mut ffi_type_uint16,
                                    COut::Bits32 => &raw mut ffi_type_uint32,
                                    COut::Bits64 => &raw mut ffi_type_uint64,
                                    COut::Bits128 => &raw mut bits128,
                                };
                                let mut lffis: [LFFITypeMap; 32] = zeroed();
                                let mut types = [null_mut(); 33];
                                cdef.inargs
                                    .iter()
                                    .zip(lffis.iter_mut())
                                    .for_each(|(x, slot)| x.vtype.as_lffitype(slot));
                                lffis
                                    .iter_mut()
                                    .map(|x| &mut x.lffitype as *mut _)
                                    .chain(iter::once(null_mut()))
                                    .zip(types.iter_mut())
                                    .for_each(|(ffi, ty)| {
                                        *ty = ffi;
                                    });
                                let mut cif = ffi_cif { ..Default::default() };
                                ffi_prep_cif(
                                    &mut cif,
                                    ffi_abi_FFI_DEFAULT_ABI,
                                    cdef.inargs.len() as u32,
                                    output,
                                    types.as_mut_ptr(),
                                );
                                let mut stores = [0u64; 32];
                                let mut inargs = [null_mut(); 32];
                                cdef.inargs
                                    .iter()
                                    .map(|mval| {
                                        let outval = match mval.vreg {
                                            VReg::R1 => 0,
                                            VReg::R2 => 1,
                                            VReg::R3 => 2,
                                            VReg::R4 => 3,
                                            VReg::R5 => 4,
                                            VReg::R6 => 5,
                                            VReg::R7 => 6,
                                            VReg::R8 => 7,
                                            VReg::Scratchpad => 8,
                                            VReg::Largepad => 9,
                                            VReg::LoadFromPtrInR2 => 10,
                                        };
                                        let ts = &mut *taskstate;
                                        let locreslv = unsafe {
                                            match outval {
                                                0 => &raw mut (*ts).r1,
                                                1 => &raw mut (*ts).r2,
                                                2 => &raw mut (*ts).r3,
                                                3 => &raw mut (*ts).r4,
                                                4 => &raw mut (*ts).r5,
                                                5 => &raw mut (*ts).r6,
                                                6 => &raw mut (*ts).r7,
                                                7 => &raw mut (*ts).r8,
                                                8 => (*ts).scratchpad,
                                                9 => (*ts).largepad,
                                                #[allow(unused_unsafe)]
                                                10 => unsafe { (*ts).r2.selfref }
                                                #[allow(unreachable_patterns)]
                                                _ => ::core::panicking::panic("not implemented"),
                                            }
                                        };
                                        ((*locreslv).u64, mval.regof, mval.vtype)
                                    })
                                    .zip(stores.iter_mut())
                                    .zip(inargs.iter_mut())
                                    .for_each(|(((storeval, regof, vtype), store), inarg)| {
                                        *store = storeval;
                                        let ptr = vtype.ptr(store as *mut _ as _, regof);
                                        *inarg = ptr;
                                    });
                                let mut ret_fullsize = [0u64; 2];
                                ffi_call(
                                    &mut cif,
                                    Some(transmute(fnptr)),
                                    ret_fullsize.as_mut_ptr() as _,
                                    inargs.as_mut_ptr(),
                                );
                                let r7 = &raw mut (*taskstate).r7;
                                ptr::copy_nonoverlapping(
                                    ret_fullsize.as_ptr() as *const u8,
                                    r7 as *mut u8,
                                    out_bytes,
                                );
                            }
                        }
                        CallSig::SaFFIAsyncO(_) | CallSig::SaFFIAsyncQ(_) => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "not implemented: {0}",
                                    format_args!("synccall was ran with ASYNC library"),
                                ),
                            );
                        }
                    }
                }
                pub fn call_asynccall(
                    _pickle: &PickleInstruction,
                    _ws: *mut WorkingSet,
                    _taskstate: *mut VMTaskState,
                ) {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "not implemented: {0}",
                                format_args!("Synccall-asyncall will be implemented later!"),
                            ),
                        );
                    }
                }
                pub fn call_task(
                    _pickle: &PickleInstruction,
                    _ws: *mut WorkingSet,
                    _taskstate: *mut VMTaskState,
                ) {
                    {
                        ::core::panicking::panic_fmt(
                            format_args!(
                                "not implemented: {0}",
                                format_args!("Task will be implemented later!"),
                            ),
                        );
                    }
                }
            }
            pub use threading::*;
            use sart::{
                ctr::{AggressiveMatrixExtension, VMTaskState},
                salloc, structures::QuadPackedData,
            };
            use crate::acaot::pickle::def::{
                PICKLE_DISPATCH_TABLE, PICKLE_OPCODE_JIF, PICKLE_OPCODE_JMP,
                PICKLE_OPCODE_MARK, PICKLE_OPCODE_VADD, PICKLE_OPCODE_VCMP,
                PickleInstruction,
            };
            pub const SIZE_128KB: usize = 128 * 1024 / size_of::<QuadPackedData>();
            pub struct WorkingSet {
                pub arr: &'static [u8],
                pub largepad: *mut QuadPackedData,
                pub largepad_cursor: usize,
                pub relocmap: Arc<HashMap<u64, usize, ahash::RandomState>>,
                pub ame: *mut AggressiveMatrixExtension,
                pub ame_free: bool,
                pub jmp: (u64, usize),
            }
            impl WorkingSet {
                pub fn getame(&mut self) -> *mut AggressiveMatrixExtension {
                    let allocame = || unsafe {
                        salloc::aligned_malloc(
                            size_of::<AggressiveMatrixExtension>(),
                            align_of::<AggressiveMatrixExtension>(),
                        ) as *mut AggressiveMatrixExtension
                    };
                    if self.ame_free {
                        self.ame_free = false;
                        if self.ame.is_null() {
                            self.ame = allocame();
                        }
                        return self.ame;
                    }
                    allocame()
                }
                pub fn freeame(&mut self, ame: *mut AggressiveMatrixExtension) {
                    if self.ame == ame {
                        self.ame_free = true;
                        return;
                    }
                    unsafe { salloc::aligned_free(ame as _) };
                }
                pub fn allocate(
                    &mut self,
                    size: usize,
                    align: usize,
                ) -> *mut QuadPackedData {
                    if align != 0 {
                        return unsafe {
                            sart::salloc::aligned_malloc(
                                size * size_of::<QuadPackedData>(),
                                align,
                            ) as _
                        };
                    }
                    let req_size = size as usize + 1;
                    let Some(new_cursor) = self.largepad_cursor.checked_add(req_size)
                    else {
                        return self.sallocate_fallback(req_size);
                    };
                    if new_cursor > SIZE_128KB {
                        return self.sallocate_fallback(req_size);
                    }
                    unsafe {
                        let newptr = self.largepad.add(self.largepad_cursor);
                        *newptr = QuadPackedData {
                            u64: req_size as _,
                        };
                        self.largepad_cursor = new_cursor;
                        return newptr.add(1);
                    }
                }
                fn sallocate_fallback(&self, req_size: usize) -> *mut QuadPackedData {
                    unsafe {
                        let out = sart::salloc::aligned_malloc(
                            req_size * size_of::<QuadPackedData>(),
                            align_of::<QuadPackedData>(),
                        ) as *mut QuadPackedData;
                        if out.is_null() {
                            return std::ptr::null_mut();
                        }
                        (*out).u64 = 0;
                        out.add(1)
                    }
                }
                pub fn salloc_free(&self, ptr: *mut QuadPackedData) {
                    unsafe {
                        sart::salloc::aligned_free(ptr as _);
                    }
                }
                pub fn free(&mut self, ptr: *mut QuadPackedData) {
                    unsafe {
                        let header = ptr.wrapping_sub(1);
                        let length_of_ptr = (*header).u64;
                        if length_of_ptr == 0 {
                            return self.salloc_free(ptr.wrapping_sub(1));
                        }
                        self.largepad_cursor = self
                            .largepad_cursor
                            .wrapping_sub(length_of_ptr as usize);
                    }
                }
            }
            pub type ResolveFn = fn(
                pickle: &PickleInstruction,
                ws: *mut WorkingSet,
                taskstate: *mut VMTaskState,
            ) -> ();
            #[inline(always)]
            pub fn call_hint(
                pickle: &PickleInstruction,
                ws: *mut WorkingSet,
                taskstate: *mut VMTaskState,
            ) {
                unsafe {
                    let instruction = pickle.u1;
                    let total_wsput = pickle.u2 as usize;
                    let bytes = pickle.u3 as usize;
                    let pic = { (*taskstate).curline_or_resume.usi };
                    (*ws).arr = {
                        std::slice::from_raw_parts(
                            ((*taskstate).engine_or_pt.pt as *const PickleInstruction)
                                .add(pic + 1) as *const u8,
                            bytes,
                        )
                    };
                    (*taskstate).curline_or_resume.usi = pic + total_wsput + 1;
                    {
                        let pkl = &*((*taskstate).engine_or_pt.pt
                            as *const PickleInstruction)
                            .add((*taskstate).curline_or_resume.usi);
                        if true {
                            if !(pkl.opcode == instruction) {
                                ::core::panicking::panic(
                                    "assertion failed: pkl.opcode == instruction",
                                )
                            }
                        }
                        match instruction {
                            PICKLE_OPCODE_MARK => call_mark(pkl, ws, taskstate),
                            PICKLE_OPCODE_JMP => call_jmp(pkl, ws, taskstate),
                            PICKLE_OPCODE_JIF => call_jif(pkl, ws, taskstate),
                            PICKLE_OPCODE_VCMP => call_vcmp(pkl, ws, taskstate),
                            PICKLE_OPCODE_VADD => call_vadd(pkl, ws, taskstate),
                            _ => {
                                return PICKLE_DISPATCH_TABLE
                                    .get_unchecked(instruction as usize)(pkl, ws, taskstate);
                            }
                        }
                    }
                }
            }
            #[inline(always)]
            pub fn call_mark(
                _pickle: &PickleInstruction,
                _ws: *mut WorkingSet,
                _taskstate: *mut VMTaskState,
            ) {}
            #[inline(always)]
            pub fn call_ws_put(
                _pickle: &PickleInstruction,
                _ws: *mut WorkingSet,
                _taskstate: *mut VMTaskState,
            ) {
                {
                    ::core::panicking::panic_fmt(
                        format_args!("WS_PUT is not to be called"),
                    );
                };
            }
            #[inline(always)]
            pub fn call_mov(
                pickle: &PickleInstruction,
                _ws: *mut WorkingSet,
                taskstate: *mut VMTaskState,
            ) {
                let source = pickle.u1;
                let target = pickle.u2;
                if source == target {
                    cold_path();
                    match source {
                        12 => {
                            unsafe {
                                (*taskstate).r1.selfref = (*taskstate).largepad;
                            }
                        }
                        13 => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "not yet implemented: {0}",
                                    format_args!("RW Global State isn\'t yet implemented"),
                                ),
                            );
                        }
                        _ => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "source == target but special ids don\'t match",
                                ),
                            );
                        }
                    }
                } else {
                    unsafe {
                        let rsrc = unsafe {
                            match source {
                                0 => (*taskstate).r1,
                                1 => (*taskstate).r2,
                                2 => (*taskstate).r3,
                                3 => (*taskstate).r4,
                                4 => (*taskstate).r5,
                                5 => (*taskstate).r6,
                                6 => (*taskstate).r7,
                                7 => (*taskstate).r8,
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let ptarget = unsafe {
                            match target {
                                0 => &raw mut (*taskstate).r1,
                                1 => &raw mut (*taskstate).r2,
                                2 => &raw mut (*taskstate).r3,
                                3 => &raw mut (*taskstate).r4,
                                4 => &raw mut (*taskstate).r5,
                                5 => &raw mut (*taskstate).r6,
                                6 => &raw mut (*taskstate).r7,
                                7 => &raw mut (*taskstate).r8,
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        *ptarget = rsrc;
                    };
                }
            }
            #[inline(always)]
            pub fn call_reg(
                pickle: &PickleInstruction,
                ws: *mut WorkingSet,
                taskstate: *mut VMTaskState,
            ) {
                let reg = pickle.u1;
                let mut filled = [0u8; 8];
                unsafe { filled[0..8].copy_from_slice(&(&(*ws).arr)[0..8]) };
                let data = u64::from_ne_bytes(filled);
                unsafe {
                    *unsafe {
                        match reg {
                            0 => &raw mut (*taskstate).r1,
                            1 => &raw mut (*taskstate).r2,
                            2 => &raw mut (*taskstate).r3,
                            3 => &raw mut (*taskstate).r4,
                            4 => &raw mut (*taskstate).r5,
                            5 => &raw mut (*taskstate).r6,
                            6 => &raw mut (*taskstate).r7,
                            7 => &raw mut (*taskstate).r8,
                            _ => ::core::panicking::panic("not implemented"),
                        }
                    } = QuadPackedData { u64: data }
                };
            }
            #[inline(always)]
            pub fn call_jmp(
                pickle: &PickleInstruction,
                ws: *mut WorkingSet,
                taskstate: *mut VMTaskState,
            ) {
                let mut filled = [0u8; 8];
                filled[0..6].copy_from_slice(unsafe { &(&(*ws).arr)[0..6] });
                filled[6..8].copy_from_slice(&[pickle.u1, pickle.u2]);
                let data = u64::from_ne_bytes(filled);
                unsafe {
                    if (*ws).jmp.0 == data {
                        (*taskstate).curline_or_resume.usi = (*ws).jmp.1;
                        return;
                    }
                    let cr = *(*ws).relocmap.get(&data).unwrap_unchecked();
                    (*ws).jmp = (data, cr);
                    (*taskstate).curline_or_resume.usi = cr;
                }
            }
            #[inline(always)]
            pub fn call_jif(
                pickle: &PickleInstruction,
                ws: *mut WorkingSet,
                taskstate: *mut VMTaskState,
            ) {
                let intent = pickle.u1;
                let relocation_src = pickle.u2;
                let width = pickle.u3;
                let offset = i32::from_ne_bytes(unsafe {
                    (&(*ws).arr)[0..4].try_into().unwrap_unchecked()
                });
                let marker = u64::from_ne_bytes(unsafe {
                    (&(*ws).arr)[4..12].try_into().unwrap_unchecked()
                });
                let not_zero = unsafe {
                    let src = unsafe {
                        match relocation_src {
                            0 => &raw mut (*taskstate).r1,
                            1 => &raw mut (*taskstate).r2,
                            2 => &raw mut (*taskstate).r3,
                            3 => &raw mut (*taskstate).r4,
                            4 => &raw mut (*taskstate).r5,
                            5 => &raw mut (*taskstate).r6,
                            6 => &raw mut (*taskstate).r7,
                            7 => &raw mut (*taskstate).r8,
                            8 => (*taskstate).scratchpad,
                            9 => (*taskstate).largepad,
                            #[allow(unused_unsafe)]
                            10 => unsafe { (*taskstate).r2.selfref }
                            #[allow(unreachable_patterns)]
                            _ => ::core::panicking::panic("not implemented"),
                        }
                    } as *mut u8;
                    match width {
                        0 => {
                            std::ptr::read_unaligned(
                                (src as *mut u64).offset(offset as _),
                            ) != 0
                        }
                        1 => {
                            std::ptr::read_unaligned(
                                (src as *mut u32).offset(offset as _),
                            ) != 0
                        }
                        2 => {
                            std::ptr::read_unaligned(
                                (src as *mut u16).offset(offset as _),
                            ) != 0
                        }
                        3 => {
                            std::ptr::read_unaligned(
                                (src as *mut u8).offset(offset as _),
                            ) != 0
                        }
                        _ => {
                            ::core::panicking::panic_fmt(format_args!("Invalid width"));
                        }
                    }
                };
                unsafe {
                    if (intent == 0 && !not_zero) || (intent != 0 && not_zero) {
                        if (*ws).jmp.0 == marker {
                            (*taskstate).curline_or_resume.usi = (*ws).jmp.1;
                            return;
                        }
                        let cr = *(*ws).relocmap.get(&marker).unwrap_unchecked();
                        (*ws).jmp = (marker, cr);
                        (*taskstate).curline_or_resume.usi = cr;
                    }
                }
            }
            #[inline(always)]
            pub fn call_vcmp(
                pickle: &PickleInstruction,
                ws: *mut WorkingSet,
                taskstate: *mut VMTaskState,
            ) {
                let op = pickle.u1;
                let width = pickle.u2;
                let srcflags = {
                    #[allow(unused_unsafe)]
                    <u16>::from_ne_bytes(unsafe {
                        (&(*ws).arr)[0..2].try_into().unwrap_unchecked()
                    })
                };
                let _src1 = (srcflags >> 12) as u8 & 0xF;
                let _src2 = ((srcflags >> 8) & 0xF) as u8;
                let _target = ((srcflags >> 4) & 0xF) as u8;
                let count = {
                    #[allow(unused_unsafe)]
                    <u32>::from_ne_bytes(unsafe {
                        (&(*ws).arr)[2..6].try_into().unwrap_unchecked()
                    })
                };
                let offset1 = {
                    #[allow(unused_unsafe)]
                    <i32>::from_ne_bytes(unsafe {
                        (&(*ws).arr)[6..10].try_into().unwrap_unchecked()
                    })
                };
                let offset2 = {
                    #[allow(unused_unsafe)]
                    <i32>::from_ne_bytes(unsafe {
                        (&(*ws).arr)[10..14].try_into().unwrap_unchecked()
                    })
                };
                let offset3 = {
                    #[allow(unused_unsafe)]
                    <i32>::from_ne_bytes(unsafe {
                        (&(*ws).arr)[14..18].try_into().unwrap_unchecked()
                    })
                };
                let src1 = {
                    unsafe {
                        match _src1 {
                            0 => &raw mut (*taskstate).r1,
                            1 => &raw mut (*taskstate).r2,
                            2 => &raw mut (*taskstate).r3,
                            3 => &raw mut (*taskstate).r4,
                            4 => &raw mut (*taskstate).r5,
                            5 => &raw mut (*taskstate).r6,
                            6 => &raw mut (*taskstate).r7,
                            7 => &raw mut (*taskstate).r8,
                            8 => (*taskstate).scratchpad,
                            9 => (*taskstate).largepad,
                            #[allow(unused_unsafe)]
                            10 => unsafe { (*taskstate).r2.selfref }
                            #[allow(unreachable_patterns)]
                            _ => ::core::panicking::panic("not implemented"),
                        }
                    }
                };
                let src2 = {
                    unsafe {
                        match _src2 {
                            0 => &raw mut (*taskstate).r1,
                            1 => &raw mut (*taskstate).r2,
                            2 => &raw mut (*taskstate).r3,
                            3 => &raw mut (*taskstate).r4,
                            4 => &raw mut (*taskstate).r5,
                            5 => &raw mut (*taskstate).r6,
                            6 => &raw mut (*taskstate).r7,
                            7 => &raw mut (*taskstate).r8,
                            8 => (*taskstate).scratchpad,
                            9 => (*taskstate).largepad,
                            #[allow(unused_unsafe)]
                            10 => unsafe { (*taskstate).r2.selfref }
                            #[allow(unreachable_patterns)]
                            _ => ::core::panicking::panic("not implemented"),
                        }
                    }
                };
                let target = {
                    unsafe {
                        match _target {
                            0 => &raw mut (*taskstate).r1,
                            1 => &raw mut (*taskstate).r2,
                            2 => &raw mut (*taskstate).r3,
                            3 => &raw mut (*taskstate).r4,
                            4 => &raw mut (*taskstate).r5,
                            5 => &raw mut (*taskstate).r6,
                            6 => &raw mut (*taskstate).r7,
                            7 => &raw mut (*taskstate).r8,
                            8 => (*taskstate).scratchpad,
                            9 => (*taskstate).largepad,
                            #[allow(unused_unsafe)]
                            10 => unsafe { (*taskstate).r2.selfref }
                            #[allow(unreachable_patterns)]
                            _ => ::core::panicking::panic("not implemented"),
                        }
                    }
                };
                let successval = if count > 1 { !0u64 } else { 1u64 };
                let innercmp: unsafe fn(
                    u8,
                    *const QuadPackedData,
                    *const QuadPackedData,
                    *mut QuadPackedData,
                    u64,
                    i32,
                    i32,
                    i32,
                ) = if op <= 9 {
                    let is_signed = [2, 4, 6, 8].iter().any(|o| op == *o);
                    match (is_signed, width) {
                        (true, 0) => vcmp_inner::<i64>,
                        (true, 1) => vcmp_inner::<i32>,
                        (true, 2) => vcmp_inner::<i16>,
                        (true, 3) => vcmp_inner::<i8>,
                        (false, 0) => vcmp_inner::<u64>,
                        (false, 1) => vcmp_inner::<u32>,
                        (false, 2) => vcmp_inner::<u16>,
                        (false, 3) => vcmp_inner::<u8>,
                        _ => ::core::panicking::panic("explicit panic"),
                    }
                } else {
                    match width {
                        1 => vcmp_f_inner::<f32, i32>,
                        0 => vcmp_f_inner::<f64, i64>,
                        _ => ::core::panicking::panic("explicit panic"),
                    }
                };
                unsafe {
                    for additive in 0..count {
                        innercmp(
                            op,
                            src1.add(additive as _),
                            src2.add(additive as _),
                            target.add(additive as _),
                            successval,
                            offset1,
                            offset2,
                            offset3,
                        );
                    }
                }
            }
            unsafe fn vcmp_inner<T>(
                op: u8,
                s1: *const QuadPackedData,
                s2: *const QuadPackedData,
                t: *mut QuadPackedData,
                success: u64,
                offset1: i32,
                offset2: i32,
                offset3: i32,
            )
            where
                T: Copy + PartialEq + PartialOrd + 'static,
            {
                unsafe {
                    let v1 = read_unaligned((s1 as *mut T).offset(offset1 as _));
                    let v2 = read_unaligned((s2 as *mut T).offset(offset2 as _));
                    let cond = match op {
                        0 => v1 == v2,
                        1 => v1 != v2,
                        2 | 3 => v1 < v2,
                        4 | 5 => v1 <= v2,
                        6 | 7 => v1 > v2,
                        8 | 9 => v1 >= v2,
                        _ => false,
                    };
                    let val = if cond { transmute_copy(&success) } else { zeroed() };
                    (t as *mut T).offset(offset3 as _).write_unaligned(val);
                }
            }
            trait Float {
                fn nan(&self) -> bool;
            }
            impl Float for f32 {
                fn nan(&self) -> bool {
                    self.is_nan()
                }
            }
            impl Float for f64 {
                fn nan(&self) -> bool {
                    self.is_nan()
                }
            }
            unsafe fn vcmp_f_inner<T, E>(
                op: u8,
                s1: *const QuadPackedData,
                s2: *const QuadPackedData,
                t: *mut QuadPackedData,
                success: u64,
                offset1: i32,
                offset2: i32,
                offset3: i32,
            )
            where
                T: Copy + Float + PartialEq + PartialOrd + 'static,
            {
                unsafe {
                    if !(size_of::<T>() == size_of::<E>()) {
                        ::core::panicking::panic(
                            "assertion failed: size_of::<T>() == size_of::<E>()",
                        )
                    }
                    if !(align_of::<T>() == align_of::<E>()) {
                        ::core::panicking::panic(
                            "assertion failed: align_of::<T>() == align_of::<E>()",
                        )
                    }
                    let v1 = read_unaligned((s1 as *mut T).offset(offset1 as _));
                    let v2 = read_unaligned((s2 as *mut T).offset(offset2 as _));
                    let un = v1.nan() || v2.nan();
                    let eq = v1 == v2;
                    let lt = v1 < v2;
                    let gt = v1 > v2;
                    let cond = match op {
                        10 => eq || lt || gt,
                        11 => un,
                        12 => eq,
                        13 => un || lt || gt,
                        14 => lt || gt,
                        15 => un || eq,
                        16 => lt,
                        17 => lt || eq,
                        18 => gt,
                        19 => gt || eq,
                        20 => un || lt,
                        21 => un || lt || eq,
                        22 => un || gt,
                        23 => un || gt || eq,
                        _ => false,
                    };
                    let val = if cond { transmute_copy(&success) } else { zeroed() };
                    ((t as *mut T).offset(offset3 as _) as *mut E).write_unaligned(val);
                }
            }
        }
        pub struct PickleWorker<T: Seek + Read> {
            pub bytecode: T,
            pub out: Vec<PickleInstruction>,
            pub libcalls: HashSet<u64>,
            pub jump: HashMap<u64, usize, ahash::RandomState>,
        }
        trait Extract: Read + Sized {
            fn extract<const N: usize>(&mut self) -> [u8; N] {
                self.extract_result::<N>().unwrap()
            }
            fn extract_result<const N: usize>(&mut self) -> std::io::Result<[u8; N]> {
                let mut my_array = [0u8; N];
                self.read_exact(&mut my_array)?;
                Ok(my_array)
            }
        }
        trait ToNE {
            fn swap_if_be(self) -> Self;
        }
        impl<const N: usize> ToNE for [u8; N] {
            #[allow(unused)]
            fn swap_if_be(mut self) -> Self {
                self
            }
        }
        impl<T: Read + Sized> Extract for T {}
        impl<T: Seek + Read> PickleWorker<T> {
            pub fn pass1(&mut self) {
                while let Ok([opcode]) = self.bytecode.extract_result::<1>() {
                    match opcode {
                        INSTRUCTION_MOV => self.handle_mov(),
                        INSTRUCTION_REG => self.handle_reg(),
                        INSTRUCTION_MARK => self.handle_mark(),
                        INSTRUCTION_JMP => self.handle_jmp(),
                        INSTRUCTION_JIF => self.handle_jif(),
                        INSTRUCTION_VCMP => self.handle_vcmp(),
                        INSTRUCTION_SCRATCH => self.handle_scratch(),
                        INSTRUCTION_VCOPY => self.handle_vcopy(),
                        INSTRUCTION_VADD => self.handle_vop(PICKLE_OPCODE_VADD),
                        INSTRUCTION_VADDF => self.handle_vopf(PICKLE_OPCODE_VADDF),
                        INSTRUCTION_VSUB => self.handle_vop(PICKLE_OPCODE_VSUB),
                        INSTRUCTION_VSUBF => self.handle_vopf(PICKLE_OPCODE_VSUBF),
                        INSTRUCTION_VMUL => self.handle_vop(PICKLE_OPCODE_VMUL),
                        INSTRUCTION_VMULF => self.handle_vopf(PICKLE_OPCODE_VMULF),
                        INSTRUCTION_VDIVF => self.handle_vopf(PICKLE_OPCODE_VDIVF),
                        INSTRUCTION_DIV => self.handle_div_like(PICKLE_OPCODE_DIV),
                        INSTRUCTION_REM => self.handle_div_like(PICKLE_OPCODE_REM),
                        INSTRUCTION_CAST => self.handle_cast(),
                        INSTRUCTION_VFCAST => self.handle_vdata_op(PICKLE_OPCODE_VFCAST),
                        INSTRUCTION_VNEG => self.handle_vdata_op(PICKLE_OPCODE_VNEG),
                        INSTRUCTION_VABS => self.handle_vdata_op(PICKLE_OPCODE_VABS),
                        INSTRUCTION_VFOP => self.handle_vdata_op(PICKLE_OPCODE_VFOP),
                        INSTRUCTION_VBIT => self.handle_vbit_op(PICKLE_OPCODE_VBIT),
                        INSTRUCTION_VROT => self.handle_vrot(),
                        INSTRUCTION_VSH => self.handle_vsh(),
                        INSTRUCTION_VCNT => self.handle_vcnt(),
                        INSTRUCTION_VMINIMAX => self.handle_vminimax(),
                        INSTRUCTION_VFMA => self.handle_vfma(),
                        INSTRUCTION_SYNCCALL => self.handle_synccall(),
                        INSTRUCTION_ASYNCCALL => self.handle_asynccall(),
                        INSTRUCTION_SPAWN => self.handle_spawn(),
                        INSTRUCTION_TASK => self.handle_task(),
                        INSTRUCTION_ATOMIC => self.handle_atomic(),
                        _e => {
                            ::core::panicking::panic_fmt(
                                format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("INST: {0}", _e),
                                ),
                            );
                        }
                    }
                }
            }
            fn handle_atomic(&mut self) {
                let opcode = PICKLE_OPCODE_ATOMIC;
                let flags_offset_v0_v1 = self.bytecode.extract::<4>();
                let mut cp = [0; 6];
                cp[0..1].copy_from_slice(&[flags_offset_v0_v1[3]]);
                cp[1..3].copy_from_slice(&self.bytecode.extract::<2>());
                cp[3..5].copy_from_slice(&self.bytecode.extract::<2>().swap_if_be());
                self.emit_copy_bytes(opcode, cp);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags_offset_v0_v1[0],
                        u2: flags_offset_v0_v1[1],
                        u3: flags_offset_v0_v1[2],
                    });
            }
            fn handle_task(&mut self) {
                let opcode = PICKLE_OPCODE_TASK;
                let [op] = self.bytecode.extract::<1>().swap_if_be();
                let marker = self.bytecode.extract::<8>().swap_if_be();
                self.emit_copy_bytes(opcode, marker);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: op >> 4,
                        u2: op & 0x0F,
                        u3: 0,
                    });
            }
            fn handle_spawn(&mut self) {
                let opcode = PICKLE_OPCODE_SPAWN;
                let sectionid = self.bytecode.extract::<8>().swap_if_be();
                let [o0] = self.bytecode.extract::<1>().swap_if_be();
                self.emit_copy_bytes(opcode, sectionid);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: o0,
                        u2: 0,
                        u3: 0,
                    });
            }
            fn handle_asynccall(&mut self) {
                let opcode = PICKLE_OPCODE_ASYNCCALL;
                let sectionid = self.bytecode.extract::<8>().swap_if_be();
                let marker = self.bytecode.extract::<8>().swap_if_be();
                self.libcalls.insert(u64::from_ne_bytes(sectionid));
                let mut copy = [0u8; 16];
                copy[0..8].copy_from_slice(&sectionid);
                copy[8..16].copy_from_slice(&marker);
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: 0,
                        u2: 0,
                        u3: 0,
                    });
            }
            fn handle_synccall(&mut self) {
                let opcode = PICKLE_OPCODE_SYNCCALL;
                let [regignore] = self.bytecode.extract::<1>();
                let sectionid = self.bytecode.extract::<8>().swap_if_be();
                self.libcalls.insert(u64::from_ne_bytes(sectionid));
                self.emit_copy_bytes(opcode, sectionid);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: regignore,
                        u2: 0,
                        u3: 0,
                    });
            }
            fn handle_vfma(&mut self) {
                let opcode = PICKLE_OPCODE_VFMA;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let [memflags] = self.bytecode.extract::<1>();
                let mut copy = [0u8; 20];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[16..20].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: memflags,
                    });
            }
            fn handle_vminimax(&mut self) {
                let opcode = PICKLE_OPCODE_VMINIMAX;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let [maxbit] = self.bytecode.extract::<1>();
                let mut copy = [0u8; 8];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..5].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                copy[5..6].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                copy[6..7].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: maxbit,
                    });
            }
            fn handle_vcnt(&mut self) {
                let opcode = PICKLE_OPCODE_VCNT;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let mut copy = [0u8; 6];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..5].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                copy[5..6].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: 0,
                    });
            }
            fn handle_vsh(&mut self) {
                let opcode = PICKLE_OPCODE_VSH;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let mut copy = [0u8; 8];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..5].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                copy[5..6].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                copy[6..7].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: 0,
                    });
            }
            fn handle_vrot(&mut self) {
                let opcode = PICKLE_OPCODE_VROT;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let [rotation] = self.bytecode.extract::<1>();
                let mut copy = [0u8; 16];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: rotation,
                    });
            }
            fn handle_vbit_op(&mut self, opcode: u8) {
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let [count] = self.bytecode.extract::<1>();
                let mut copy = [0u8; 16];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: count,
                    });
            }
            fn handle_vdata_op(&mut self, opcode: u8) {
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let mut copy = [0u8; 12];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: 0,
                    });
            }
            fn handle_cast(&mut self) {
                let opcode = PICKLE_OPCODE_CAST;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let mut copy = [0u8; 8];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: 0,
                    });
            }
            fn handle_div_like(&mut self, opcode: u8) {
                let [args1, args2] = self.bytecode.extract::<2>().swap_if_be();
                let mut copy: [u8; 12] = [0; 12];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: args1,
                        u2: args2,
                        u3: 0,
                    });
            }
            fn handle_vopf(&mut self, opcode: u8) {
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let mut copy: [u8; 16] = [0; 16];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: flags1,
                        u2: flags2,
                        u3: 0,
                    });
            }
            fn handle_vop(&mut self, opcode: u8) {
                let mut copy: [u8; 20] = [0; 20];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                let target1 = self.bytecode.extract::<4>().swap_if_be();
                copy[16..20].copy_from_slice(&target1);
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: 0,
                        u2: 0,
                        u3: 0,
                    });
            }
            fn handle_vcopy(&mut self) {
                let [mflags, src_flags] = self.bytecode.extract::<2>();
                let mut copy = [0u8; 12];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(PICKLE_OPCODE_VCOPY, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_VCOPY,
                        u1: mflags,
                        u2: src_flags,
                        u3: 0,
                    });
            }
            fn handle_scratch(&mut self) {
                let data = self.bytecode.extract::<2>().swap_if_be();
                let data = u16::from_ne_bytes(data);
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_SCRATCH,
                        u1: (data >> 14) as _,
                        u2: ((data >> 8) as u8) & 0x3F,
                        u3: (data & 0xFF) as u8,
                    });
            }
            fn handle_vcmp(&mut self) {
                let [r0] = self.bytecode.extract::<1>();
                let operation = r0 & 0xFF;
                let mut total: [u8; 18] = [0; 18];
                total[0..2].copy_from_slice(&self.bytecode.extract::<2>().swap_if_be());
                total[2..6].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                total[6..10].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                total[10..14]
                    .copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                total[14..18]
                    .copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(PICKLE_OPCODE_VCMP, total);
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_VCMP,
                        u1: operation & 0x1F,
                        u2: operation >> 5,
                        u3: 0,
                    });
            }
            fn handle_jif(&mut self) {
                let [cond] = self.bytecode.extract_result::<1>().unwrap();
                let offset = i32::from_le_bytes(
                        self.bytecode.extract_result::<4>().unwrap(),
                    )
                    .to_ne_bytes();
                let marker = u64::from_le_bytes(
                        self.bytecode.extract_result::<8>().unwrap(),
                    )
                    .to_ne_bytes();
                let mut combined_array: [u8; 12] = [0; 12];
                combined_array[..4].copy_from_slice(&offset);
                combined_array[4..].copy_from_slice(&marker);
                self.emit_copy_bytes(PICKLE_OPCODE_JIF, combined_array);
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_JIF,
                        u1: cond >> 7,
                        u2: cond & 0x0F,
                        u3: (cond >> 5) & 0x03,
                    });
            }
            fn handle_jmp(&mut self) {
                let data = u64::from_le_bytes(
                        self.bytecode.extract_result::<8>().unwrap(),
                    )
                    .to_ne_bytes();
                self.emit_copy_bytes::<
                        6,
                    >(PICKLE_OPCODE_JMP, data[0..6].try_into().unwrap());
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_JMP,
                        u1: data[6],
                        u2: data[7],
                        u3: 0,
                    });
            }
            fn handle_mark(&mut self) {
                let marker = u64::from_le_bytes(
                    self.bytecode.extract_result::<8>().unwrap(),
                );
                let data = marker.to_ne_bytes();
                self.emit_copy_bytes(PICKLE_OPCODE_MARK, data);
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_MARK,
                        u1: 0,
                        u2: 0,
                        u3: 0,
                    });
                self.jump.insert(marker, self.out.len() - 1);
            }
            fn handle_reg(&mut self) {
                let [register] = self.bytecode.extract_result().expect("");
                let data_ne: [u8; 8] = u64::from_le_bytes(
                        self.bytecode.extract_result::<8>().expect(""),
                    )
                    .to_ne_bytes();
                self.emit_copy_bytes(PICKLE_OPCODE_REG, data_ne);
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_REG,
                        u1: register,
                        u2: 0,
                        u3: 0,
                    });
            }
            fn emit_copy_bytes<const N: usize>(&mut self, opcode: u8, data: [u8; N]) {
                if true {
                    if !(N % 2 == 0) {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("Payload must be word-aligned"),
                            );
                        }
                    }
                }
                if true {
                    if !(N <= 255) {
                        {
                            ::core::panicking::panic_fmt(
                                format_args!("Payload size {0} exceeds u8 capacity", N),
                            );
                        }
                    }
                }
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_HINT,
                        u1: opcode,
                        u2: (N / 4) as u8 + ((N % 4) / 2) as u8,
                        u3: N as u8,
                    });
                let mut chunks_4 = data.chunks_exact(4);
                for chunk in chunks_4.by_ref() {
                    self.out
                        .push(PickleInstruction {
                            opcode: chunk[0],
                            u1: chunk[1],
                            u2: chunk[2],
                            u3: chunk[3],
                        });
                }
                let chunks_2 = chunks_4.remainder().chunks_exact(2);
                for chunk in chunks_2 {
                    self.out
                        .push(PickleInstruction {
                            opcode: chunk[0],
                            u1: chunk[1],
                            u2: 0,
                            u3: 0,
                        });
                }
            }
            fn handle_mov(&mut self) {
                let [registers] = self.bytecode.extract_result().expect("");
                let source = registers >> 4;
                let target = registers & 0x0F;
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_MOV,
                        u1: source,
                        u2: target,
                        u3: 0,
                    });
            }
        }
    }
    pub enum LocSrc {
        VCopyNoAlias,
        VCopyOverlapping,
        VMScratchAction,
        VMSectionDispatch,
        VMLibcallSection,
        VMSpawn,
        NativeLibCall(u64),
        SaLibCall(u64),
        CLIRLibCall(ClirLC),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for LocSrc {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                LocSrc::VCopyNoAlias => {
                    ::core::fmt::Formatter::write_str(f, "VCopyNoAlias")
                }
                LocSrc::VCopyOverlapping => {
                    ::core::fmt::Formatter::write_str(f, "VCopyOverlapping")
                }
                LocSrc::VMScratchAction => {
                    ::core::fmt::Formatter::write_str(f, "VMScratchAction")
                }
                LocSrc::VMSectionDispatch => {
                    ::core::fmt::Formatter::write_str(f, "VMSectionDispatch")
                }
                LocSrc::VMLibcallSection => {
                    ::core::fmt::Formatter::write_str(f, "VMLibcallSection")
                }
                LocSrc::VMSpawn => ::core::fmt::Formatter::write_str(f, "VMSpawn"),
                LocSrc::NativeLibCall(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "NativeLibCall",
                        &__self_0,
                    )
                }
                LocSrc::SaLibCall(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "SaLibCall",
                        &__self_0,
                    )
                }
                LocSrc::CLIRLibCall(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CLIRLibCall",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    #[doc(hidden)]
    unsafe impl ::core::clone::TrivialClone for LocSrc {}
    #[automatically_derived]
    impl ::core::clone::Clone for LocSrc {
        #[inline]
        fn clone(&self) -> LocSrc {
            let _: ::core::clone::AssertParamIsClone<u64>;
            let _: ::core::clone::AssertParamIsClone<ClirLC>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for LocSrc {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for LocSrc {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for LocSrc {
        #[inline]
        fn eq(&self, other: &LocSrc) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (
                        LocSrc::NativeLibCall(__self_0),
                        LocSrc::NativeLibCall(__arg1_0),
                    ) => __self_0 == __arg1_0,
                    (LocSrc::SaLibCall(__self_0), LocSrc::SaLibCall(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    (LocSrc::CLIRLibCall(__self_0), LocSrc::CLIRLibCall(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for LocSrc {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_fields_are_eq(&self) {
            let _: ::core::cmp::AssertParamIsEq<u64>;
            let _: ::core::cmp::AssertParamIsEq<ClirLC>;
        }
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for LocSrc {
        #[inline]
        fn partial_cmp(
            &self,
            other: &LocSrc,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            match (self, other) {
                (LocSrc::NativeLibCall(__self_0), LocSrc::NativeLibCall(__arg1_0)) => {
                    ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0)
                }
                (LocSrc::SaLibCall(__self_0), LocSrc::SaLibCall(__arg1_0)) => {
                    ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0)
                }
                (LocSrc::CLIRLibCall(__self_0), LocSrc::CLIRLibCall(__arg1_0)) => {
                    ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0)
                }
                _ => ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr),
            }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Ord for LocSrc {
        #[inline]
        fn cmp(&self, other: &LocSrc) -> ::core::cmp::Ordering {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            match ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr) {
                ::core::cmp::Ordering::Equal => {
                    match (self, other) {
                        (
                            LocSrc::NativeLibCall(__self_0),
                            LocSrc::NativeLibCall(__arg1_0),
                        ) => ::core::cmp::Ord::cmp(__self_0, __arg1_0),
                        (LocSrc::SaLibCall(__self_0), LocSrc::SaLibCall(__arg1_0)) => {
                            ::core::cmp::Ord::cmp(__self_0, __arg1_0)
                        }
                        (
                            LocSrc::CLIRLibCall(__self_0),
                            LocSrc::CLIRLibCall(__arg1_0),
                        ) => ::core::cmp::Ord::cmp(__self_0, __arg1_0),
                        _ => ::core::cmp::Ordering::Equal,
                    }
                }
                cmp => cmp,
            }
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for LocSrc {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_discr, state);
            match self {
                LocSrc::NativeLibCall(__self_0) => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                LocSrc::SaLibCall(__self_0) => ::core::hash::Hash::hash(__self_0, state),
                LocSrc::CLIRLibCall(__self_0) => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                _ => {}
            }
        }
    }
    #[doc(hidden)]
    #[allow(
        non_upper_case_globals,
        unused_attributes,
        unused_qualifications,
        clippy::absolute_paths,
    )]
    const _: () = {
        #[allow(unused_extern_crates, clippy::useless_attribute)]
        extern crate serde as _serde;
        #[automatically_derived]
        impl _serde::Serialize for LocSrc {
            fn serialize<__S>(
                &self,
                __serializer: __S,
            ) -> _serde::__private228::Result<__S::Ok, __S::Error>
            where
                __S: _serde::Serializer,
            {
                match *self {
                    LocSrc::VCopyNoAlias => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "LocSrc",
                            0u32,
                            "VCopyNoAlias",
                        )
                    }
                    LocSrc::VCopyOverlapping => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "LocSrc",
                            1u32,
                            "VCopyOverlapping",
                        )
                    }
                    LocSrc::VMScratchAction => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "LocSrc",
                            2u32,
                            "VMScratchAction",
                        )
                    }
                    LocSrc::VMSectionDispatch => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "LocSrc",
                            3u32,
                            "VMSectionDispatch",
                        )
                    }
                    LocSrc::VMLibcallSection => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "LocSrc",
                            4u32,
                            "VMLibcallSection",
                        )
                    }
                    LocSrc::VMSpawn => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "LocSrc",
                            5u32,
                            "VMSpawn",
                        )
                    }
                    LocSrc::NativeLibCall(ref __field0) => {
                        _serde::Serializer::serialize_newtype_variant(
                            __serializer,
                            "LocSrc",
                            6u32,
                            "NativeLibCall",
                            __field0,
                        )
                    }
                    LocSrc::SaLibCall(ref __field0) => {
                        _serde::Serializer::serialize_newtype_variant(
                            __serializer,
                            "LocSrc",
                            7u32,
                            "SaLibCall",
                            __field0,
                        )
                    }
                    LocSrc::CLIRLibCall(ref __field0) => {
                        _serde::Serializer::serialize_newtype_variant(
                            __serializer,
                            "LocSrc",
                            8u32,
                            "CLIRLibCall",
                            __field0,
                        )
                    }
                }
            }
        }
    };
    #[doc(hidden)]
    #[allow(
        non_upper_case_globals,
        unused_attributes,
        unused_qualifications,
        clippy::absolute_paths,
    )]
    const _: () = {
        #[allow(unused_extern_crates, clippy::useless_attribute)]
        extern crate serde as _serde;
        #[automatically_derived]
        impl<'de> _serde::Deserialize<'de> for LocSrc {
            fn deserialize<__D>(
                __deserializer: __D,
            ) -> _serde::__private228::Result<Self, __D::Error>
            where
                __D: _serde::Deserializer<'de>,
            {
                #[allow(non_camel_case_types)]
                #[doc(hidden)]
                enum __Field {
                    __field0,
                    __field1,
                    __field2,
                    __field3,
                    __field4,
                    __field5,
                    __field6,
                    __field7,
                    __field8,
                }
                #[doc(hidden)]
                struct __FieldVisitor;
                #[automatically_derived]
                impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                    type Value = __Field;
                    fn expecting(
                        &self,
                        __formatter: &mut _serde::__private228::Formatter,
                    ) -> _serde::__private228::fmt::Result {
                        _serde::__private228::Formatter::write_str(
                            __formatter,
                            "variant identifier",
                        )
                    }
                    fn visit_u64<__E>(
                        self,
                        __value: u64,
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            0u64 => _serde::__private228::Ok(__Field::__field0),
                            1u64 => _serde::__private228::Ok(__Field::__field1),
                            2u64 => _serde::__private228::Ok(__Field::__field2),
                            3u64 => _serde::__private228::Ok(__Field::__field3),
                            4u64 => _serde::__private228::Ok(__Field::__field4),
                            5u64 => _serde::__private228::Ok(__Field::__field5),
                            6u64 => _serde::__private228::Ok(__Field::__field6),
                            7u64 => _serde::__private228::Ok(__Field::__field7),
                            8u64 => _serde::__private228::Ok(__Field::__field8),
                            _ => {
                                _serde::__private228::Err(
                                    _serde::de::Error::invalid_value(
                                        _serde::de::Unexpected::Unsigned(__value),
                                        &"variant index 0 <= i < 9",
                                    ),
                                )
                            }
                        }
                    }
                    fn visit_str<__E>(
                        self,
                        __value: &str,
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            "VCopyNoAlias" => _serde::__private228::Ok(__Field::__field0),
                            "VCopyOverlapping" => {
                                _serde::__private228::Ok(__Field::__field1)
                            }
                            "VMScratchAction" => {
                                _serde::__private228::Ok(__Field::__field2)
                            }
                            "VMSectionDispatch" => {
                                _serde::__private228::Ok(__Field::__field3)
                            }
                            "VMLibcallSection" => {
                                _serde::__private228::Ok(__Field::__field4)
                            }
                            "VMSpawn" => _serde::__private228::Ok(__Field::__field5),
                            "NativeLibCall" => {
                                _serde::__private228::Ok(__Field::__field6)
                            }
                            "SaLibCall" => _serde::__private228::Ok(__Field::__field7),
                            "CLIRLibCall" => _serde::__private228::Ok(__Field::__field8),
                            _ => {
                                _serde::__private228::Err(
                                    _serde::de::Error::unknown_variant(__value, VARIANTS),
                                )
                            }
                        }
                    }
                    fn visit_bytes<__E>(
                        self,
                        __value: &[u8],
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            b"VCopyNoAlias" => {
                                _serde::__private228::Ok(__Field::__field0)
                            }
                            b"VCopyOverlapping" => {
                                _serde::__private228::Ok(__Field::__field1)
                            }
                            b"VMScratchAction" => {
                                _serde::__private228::Ok(__Field::__field2)
                            }
                            b"VMSectionDispatch" => {
                                _serde::__private228::Ok(__Field::__field3)
                            }
                            b"VMLibcallSection" => {
                                _serde::__private228::Ok(__Field::__field4)
                            }
                            b"VMSpawn" => _serde::__private228::Ok(__Field::__field5),
                            b"NativeLibCall" => {
                                _serde::__private228::Ok(__Field::__field6)
                            }
                            b"SaLibCall" => _serde::__private228::Ok(__Field::__field7),
                            b"CLIRLibCall" => _serde::__private228::Ok(__Field::__field8),
                            _ => {
                                let __value = &_serde::__private228::from_utf8_lossy(
                                    __value,
                                );
                                _serde::__private228::Err(
                                    _serde::de::Error::unknown_variant(__value, VARIANTS),
                                )
                            }
                        }
                    }
                }
                #[automatically_derived]
                impl<'de> _serde::Deserialize<'de> for __Field {
                    #[inline]
                    fn deserialize<__D>(
                        __deserializer: __D,
                    ) -> _serde::__private228::Result<Self, __D::Error>
                    where
                        __D: _serde::Deserializer<'de>,
                    {
                        _serde::Deserializer::deserialize_identifier(
                            __deserializer,
                            __FieldVisitor,
                        )
                    }
                }
                #[doc(hidden)]
                struct __Visitor<'de> {
                    marker: _serde::__private228::PhantomData<LocSrc>,
                    lifetime: _serde::__private228::PhantomData<&'de ()>,
                }
                #[automatically_derived]
                impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                    type Value = LocSrc;
                    fn expecting(
                        &self,
                        __formatter: &mut _serde::__private228::Formatter,
                    ) -> _serde::__private228::fmt::Result {
                        _serde::__private228::Formatter::write_str(
                            __formatter,
                            "enum LocSrc",
                        )
                    }
                    fn visit_enum<__A>(
                        self,
                        __data: __A,
                    ) -> _serde::__private228::Result<Self::Value, __A::Error>
                    where
                        __A: _serde::de::EnumAccess<'de>,
                    {
                        match _serde::de::EnumAccess::variant(__data)? {
                            (__Field::__field0, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(LocSrc::VCopyNoAlias)
                            }
                            (__Field::__field1, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(LocSrc::VCopyOverlapping)
                            }
                            (__Field::__field2, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(LocSrc::VMScratchAction)
                            }
                            (__Field::__field3, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(LocSrc::VMSectionDispatch)
                            }
                            (__Field::__field4, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(LocSrc::VMLibcallSection)
                            }
                            (__Field::__field5, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(LocSrc::VMSpawn)
                            }
                            (__Field::__field6, __variant) => {
                                _serde::__private228::Result::map(
                                    _serde::de::VariantAccess::newtype_variant::<
                                        u64,
                                    >(__variant),
                                    LocSrc::NativeLibCall,
                                )
                            }
                            (__Field::__field7, __variant) => {
                                _serde::__private228::Result::map(
                                    _serde::de::VariantAccess::newtype_variant::<
                                        u64,
                                    >(__variant),
                                    LocSrc::SaLibCall,
                                )
                            }
                            (__Field::__field8, __variant) => {
                                _serde::__private228::Result::map(
                                    _serde::de::VariantAccess::newtype_variant::<
                                        ClirLC,
                                    >(__variant),
                                    LocSrc::CLIRLibCall,
                                )
                            }
                        }
                    }
                }
                #[doc(hidden)]
                const VARIANTS: &'static [&'static str] = &[
                    "VCopyNoAlias",
                    "VCopyOverlapping",
                    "VMScratchAction",
                    "VMSectionDispatch",
                    "VMLibcallSection",
                    "VMSpawn",
                    "NativeLibCall",
                    "SaLibCall",
                    "CLIRLibCall",
                ];
                _serde::Deserializer::deserialize_enum(
                    __deserializer,
                    "LocSrc",
                    VARIANTS,
                    __Visitor {
                        marker: _serde::__private228::PhantomData::<LocSrc>,
                        lifetime: _serde::__private228::PhantomData,
                    },
                )
            }
        }
    };
    pub enum ClirLC {
        Ceil32,
        Ceil64,
        Floor32,
        Floor64,
        Fma32,
        Fma64,
        Trunc32,
        Trunc64,
        Nearest32,
        Nearest64,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ClirLC {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    ClirLC::Ceil32 => "Ceil32",
                    ClirLC::Ceil64 => "Ceil64",
                    ClirLC::Floor32 => "Floor32",
                    ClirLC::Floor64 => "Floor64",
                    ClirLC::Fma32 => "Fma32",
                    ClirLC::Fma64 => "Fma64",
                    ClirLC::Trunc32 => "Trunc32",
                    ClirLC::Trunc64 => "Trunc64",
                    ClirLC::Nearest32 => "Nearest32",
                    ClirLC::Nearest64 => "Nearest64",
                },
            )
        }
    }
    #[automatically_derived]
    #[doc(hidden)]
    unsafe impl ::core::clone::TrivialClone for ClirLC {}
    #[automatically_derived]
    impl ::core::clone::Clone for ClirLC {
        #[inline]
        fn clone(&self) -> ClirLC {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for ClirLC {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for ClirLC {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for ClirLC {
        #[inline]
        fn eq(&self, other: &ClirLC) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for ClirLC {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_fields_are_eq(&self) {}
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for ClirLC {
        #[inline]
        fn partial_cmp(
            &self,
            other: &ClirLC,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr)
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Ord for ClirLC {
        #[inline]
        fn cmp(&self, other: &ClirLC) -> ::core::cmp::Ordering {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr)
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for ClirLC {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_discr, state)
        }
    }
    #[doc(hidden)]
    #[allow(
        non_upper_case_globals,
        unused_attributes,
        unused_qualifications,
        clippy::absolute_paths,
    )]
    const _: () = {
        #[allow(unused_extern_crates, clippy::useless_attribute)]
        extern crate serde as _serde;
        #[automatically_derived]
        impl _serde::Serialize for ClirLC {
            fn serialize<__S>(
                &self,
                __serializer: __S,
            ) -> _serde::__private228::Result<__S::Ok, __S::Error>
            where
                __S: _serde::Serializer,
            {
                match *self {
                    ClirLC::Ceil32 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            0u32,
                            "Ceil32",
                        )
                    }
                    ClirLC::Ceil64 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            1u32,
                            "Ceil64",
                        )
                    }
                    ClirLC::Floor32 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            2u32,
                            "Floor32",
                        )
                    }
                    ClirLC::Floor64 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            3u32,
                            "Floor64",
                        )
                    }
                    ClirLC::Fma32 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            4u32,
                            "Fma32",
                        )
                    }
                    ClirLC::Fma64 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            5u32,
                            "Fma64",
                        )
                    }
                    ClirLC::Trunc32 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            6u32,
                            "Trunc32",
                        )
                    }
                    ClirLC::Trunc64 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            7u32,
                            "Trunc64",
                        )
                    }
                    ClirLC::Nearest32 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            8u32,
                            "Nearest32",
                        )
                    }
                    ClirLC::Nearest64 => {
                        _serde::Serializer::serialize_unit_variant(
                            __serializer,
                            "ClirLC",
                            9u32,
                            "Nearest64",
                        )
                    }
                }
            }
        }
    };
    #[doc(hidden)]
    #[allow(
        non_upper_case_globals,
        unused_attributes,
        unused_qualifications,
        clippy::absolute_paths,
    )]
    const _: () = {
        #[allow(unused_extern_crates, clippy::useless_attribute)]
        extern crate serde as _serde;
        #[automatically_derived]
        impl<'de> _serde::Deserialize<'de> for ClirLC {
            fn deserialize<__D>(
                __deserializer: __D,
            ) -> _serde::__private228::Result<Self, __D::Error>
            where
                __D: _serde::Deserializer<'de>,
            {
                #[allow(non_camel_case_types)]
                #[doc(hidden)]
                enum __Field {
                    __field0,
                    __field1,
                    __field2,
                    __field3,
                    __field4,
                    __field5,
                    __field6,
                    __field7,
                    __field8,
                    __field9,
                }
                #[doc(hidden)]
                struct __FieldVisitor;
                #[automatically_derived]
                impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                    type Value = __Field;
                    fn expecting(
                        &self,
                        __formatter: &mut _serde::__private228::Formatter,
                    ) -> _serde::__private228::fmt::Result {
                        _serde::__private228::Formatter::write_str(
                            __formatter,
                            "variant identifier",
                        )
                    }
                    fn visit_u64<__E>(
                        self,
                        __value: u64,
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            0u64 => _serde::__private228::Ok(__Field::__field0),
                            1u64 => _serde::__private228::Ok(__Field::__field1),
                            2u64 => _serde::__private228::Ok(__Field::__field2),
                            3u64 => _serde::__private228::Ok(__Field::__field3),
                            4u64 => _serde::__private228::Ok(__Field::__field4),
                            5u64 => _serde::__private228::Ok(__Field::__field5),
                            6u64 => _serde::__private228::Ok(__Field::__field6),
                            7u64 => _serde::__private228::Ok(__Field::__field7),
                            8u64 => _serde::__private228::Ok(__Field::__field8),
                            9u64 => _serde::__private228::Ok(__Field::__field9),
                            _ => {
                                _serde::__private228::Err(
                                    _serde::de::Error::invalid_value(
                                        _serde::de::Unexpected::Unsigned(__value),
                                        &"variant index 0 <= i < 10",
                                    ),
                                )
                            }
                        }
                    }
                    fn visit_str<__E>(
                        self,
                        __value: &str,
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            "Ceil32" => _serde::__private228::Ok(__Field::__field0),
                            "Ceil64" => _serde::__private228::Ok(__Field::__field1),
                            "Floor32" => _serde::__private228::Ok(__Field::__field2),
                            "Floor64" => _serde::__private228::Ok(__Field::__field3),
                            "Fma32" => _serde::__private228::Ok(__Field::__field4),
                            "Fma64" => _serde::__private228::Ok(__Field::__field5),
                            "Trunc32" => _serde::__private228::Ok(__Field::__field6),
                            "Trunc64" => _serde::__private228::Ok(__Field::__field7),
                            "Nearest32" => _serde::__private228::Ok(__Field::__field8),
                            "Nearest64" => _serde::__private228::Ok(__Field::__field9),
                            _ => {
                                _serde::__private228::Err(
                                    _serde::de::Error::unknown_variant(__value, VARIANTS),
                                )
                            }
                        }
                    }
                    fn visit_bytes<__E>(
                        self,
                        __value: &[u8],
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            b"Ceil32" => _serde::__private228::Ok(__Field::__field0),
                            b"Ceil64" => _serde::__private228::Ok(__Field::__field1),
                            b"Floor32" => _serde::__private228::Ok(__Field::__field2),
                            b"Floor64" => _serde::__private228::Ok(__Field::__field3),
                            b"Fma32" => _serde::__private228::Ok(__Field::__field4),
                            b"Fma64" => _serde::__private228::Ok(__Field::__field5),
                            b"Trunc32" => _serde::__private228::Ok(__Field::__field6),
                            b"Trunc64" => _serde::__private228::Ok(__Field::__field7),
                            b"Nearest32" => _serde::__private228::Ok(__Field::__field8),
                            b"Nearest64" => _serde::__private228::Ok(__Field::__field9),
                            _ => {
                                let __value = &_serde::__private228::from_utf8_lossy(
                                    __value,
                                );
                                _serde::__private228::Err(
                                    _serde::de::Error::unknown_variant(__value, VARIANTS),
                                )
                            }
                        }
                    }
                }
                #[automatically_derived]
                impl<'de> _serde::Deserialize<'de> for __Field {
                    #[inline]
                    fn deserialize<__D>(
                        __deserializer: __D,
                    ) -> _serde::__private228::Result<Self, __D::Error>
                    where
                        __D: _serde::Deserializer<'de>,
                    {
                        _serde::Deserializer::deserialize_identifier(
                            __deserializer,
                            __FieldVisitor,
                        )
                    }
                }
                #[doc(hidden)]
                struct __Visitor<'de> {
                    marker: _serde::__private228::PhantomData<ClirLC>,
                    lifetime: _serde::__private228::PhantomData<&'de ()>,
                }
                #[automatically_derived]
                impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                    type Value = ClirLC;
                    fn expecting(
                        &self,
                        __formatter: &mut _serde::__private228::Formatter,
                    ) -> _serde::__private228::fmt::Result {
                        _serde::__private228::Formatter::write_str(
                            __formatter,
                            "enum ClirLC",
                        )
                    }
                    fn visit_enum<__A>(
                        self,
                        __data: __A,
                    ) -> _serde::__private228::Result<Self::Value, __A::Error>
                    where
                        __A: _serde::de::EnumAccess<'de>,
                    {
                        match _serde::de::EnumAccess::variant(__data)? {
                            (__Field::__field0, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Ceil32)
                            }
                            (__Field::__field1, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Ceil64)
                            }
                            (__Field::__field2, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Floor32)
                            }
                            (__Field::__field3, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Floor64)
                            }
                            (__Field::__field4, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Fma32)
                            }
                            (__Field::__field5, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Fma64)
                            }
                            (__Field::__field6, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Trunc32)
                            }
                            (__Field::__field7, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Trunc64)
                            }
                            (__Field::__field8, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Nearest32)
                            }
                            (__Field::__field9, __variant) => {
                                _serde::de::VariantAccess::unit_variant(__variant)?;
                                _serde::__private228::Ok(ClirLC::Nearest64)
                            }
                        }
                    }
                }
                #[doc(hidden)]
                const VARIANTS: &'static [&'static str] = &[
                    "Ceil32",
                    "Ceil64",
                    "Floor32",
                    "Floor64",
                    "Fma32",
                    "Fma64",
                    "Trunc32",
                    "Trunc64",
                    "Nearest32",
                    "Nearest64",
                ];
                _serde::Deserializer::deserialize_enum(
                    __deserializer,
                    "ClirLC",
                    VARIANTS,
                    __Visitor {
                        marker: _serde::__private228::PhantomData::<ClirLC>,
                        lifetime: _serde::__private228::PhantomData,
                    },
                )
            }
        }
    };
    pub enum SigStore {
        VCopyCommon,
        VMScratch,
        SaVMLibcallDispatch,
        JITCall,
        SaFFICall,
        SaFFICallAsyncQ,
        SaFFICallAsyncO,
        VMSpawn,
        LibDefined(u64),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for SigStore {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                SigStore::VCopyCommon => {
                    ::core::fmt::Formatter::write_str(f, "VCopyCommon")
                }
                SigStore::VMScratch => ::core::fmt::Formatter::write_str(f, "VMScratch"),
                SigStore::SaVMLibcallDispatch => {
                    ::core::fmt::Formatter::write_str(f, "SaVMLibcallDispatch")
                }
                SigStore::JITCall => ::core::fmt::Formatter::write_str(f, "JITCall"),
                SigStore::SaFFICall => ::core::fmt::Formatter::write_str(f, "SaFFICall"),
                SigStore::SaFFICallAsyncQ => {
                    ::core::fmt::Formatter::write_str(f, "SaFFICallAsyncQ")
                }
                SigStore::SaFFICallAsyncO => {
                    ::core::fmt::Formatter::write_str(f, "SaFFICallAsyncO")
                }
                SigStore::VMSpawn => ::core::fmt::Formatter::write_str(f, "VMSpawn"),
                SigStore::LibDefined(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LibDefined",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    #[doc(hidden)]
    unsafe impl ::core::clone::TrivialClone for SigStore {}
    #[automatically_derived]
    impl ::core::clone::Clone for SigStore {
        #[inline]
        fn clone(&self) -> SigStore {
            let _: ::core::clone::AssertParamIsClone<u64>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for SigStore {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for SigStore {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for SigStore {
        #[inline]
        fn eq(&self, other: &SigStore) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (SigStore::LibDefined(__self_0), SigStore::LibDefined(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Eq for SigStore {
        #[inline]
        #[doc(hidden)]
        #[coverage(off)]
        fn assert_fields_are_eq(&self) {
            let _: ::core::cmp::AssertParamIsEq<u64>;
        }
    }
    #[automatically_derived]
    impl ::core::cmp::PartialOrd for SigStore {
        #[inline]
        fn partial_cmp(
            &self,
            other: &SigStore,
        ) -> ::core::option::Option<::core::cmp::Ordering> {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            match (self, other) {
                (SigStore::LibDefined(__self_0), SigStore::LibDefined(__arg1_0)) => {
                    ::core::cmp::PartialOrd::partial_cmp(__self_0, __arg1_0)
                }
                _ => ::core::cmp::PartialOrd::partial_cmp(&__self_discr, &__arg1_discr),
            }
        }
    }
    #[automatically_derived]
    impl ::core::cmp::Ord for SigStore {
        #[inline]
        fn cmp(&self, other: &SigStore) -> ::core::cmp::Ordering {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            match ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr) {
                ::core::cmp::Ordering::Equal => {
                    match (self, other) {
                        (
                            SigStore::LibDefined(__self_0),
                            SigStore::LibDefined(__arg1_0),
                        ) => ::core::cmp::Ord::cmp(__self_0, __arg1_0),
                        _ => ::core::cmp::Ordering::Equal,
                    }
                }
                cmp => cmp,
            }
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for SigStore {
        #[inline]
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_discr, state);
            match self {
                SigStore::LibDefined(__self_0) => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                _ => {}
            }
        }
    }
    pub struct JITReloc {
        pub addend: i64,
        pub loc: LocSrc,
        pub offset: u32,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for JITReloc {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "JITReloc",
                "addend",
                &self.addend,
                "loc",
                &self.loc,
                "offset",
                &&self.offset,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for JITReloc {
        #[inline]
        fn clone(&self) -> JITReloc {
            JITReloc {
                addend: ::core::clone::Clone::clone(&self.addend),
                loc: ::core::clone::Clone::clone(&self.loc),
                offset: ::core::clone::Clone::clone(&self.offset),
            }
        }
    }
    #[doc(hidden)]
    #[allow(
        non_upper_case_globals,
        unused_attributes,
        unused_qualifications,
        clippy::absolute_paths,
    )]
    const _: () = {
        #[allow(unused_extern_crates, clippy::useless_attribute)]
        extern crate serde as _serde;
        #[automatically_derived]
        impl _serde::Serialize for JITReloc {
            fn serialize<__S>(
                &self,
                __serializer: __S,
            ) -> _serde::__private228::Result<__S::Ok, __S::Error>
            where
                __S: _serde::Serializer,
            {
                let mut __serde_state = _serde::Serializer::serialize_struct(
                    __serializer,
                    "JITReloc",
                    false as usize + 1 + 1 + 1,
                )?;
                _serde::ser::SerializeStruct::serialize_field(
                    &mut __serde_state,
                    "addend",
                    &self.addend,
                )?;
                _serde::ser::SerializeStruct::serialize_field(
                    &mut __serde_state,
                    "loc",
                    &self.loc,
                )?;
                _serde::ser::SerializeStruct::serialize_field(
                    &mut __serde_state,
                    "offset",
                    &self.offset,
                )?;
                _serde::ser::SerializeStruct::end(__serde_state)
            }
        }
    };
    #[doc(hidden)]
    #[allow(
        non_upper_case_globals,
        unused_attributes,
        unused_qualifications,
        clippy::absolute_paths,
    )]
    const _: () = {
        #[allow(unused_extern_crates, clippy::useless_attribute)]
        extern crate serde as _serde;
        #[automatically_derived]
        impl<'de> _serde::Deserialize<'de> for JITReloc {
            fn deserialize<__D>(
                __deserializer: __D,
            ) -> _serde::__private228::Result<Self, __D::Error>
            where
                __D: _serde::Deserializer<'de>,
            {
                #[allow(non_camel_case_types)]
                #[doc(hidden)]
                enum __Field {
                    __field0,
                    __field1,
                    __field2,
                    __ignore,
                }
                #[doc(hidden)]
                struct __FieldVisitor;
                #[automatically_derived]
                impl<'de> _serde::de::Visitor<'de> for __FieldVisitor {
                    type Value = __Field;
                    fn expecting(
                        &self,
                        __formatter: &mut _serde::__private228::Formatter,
                    ) -> _serde::__private228::fmt::Result {
                        _serde::__private228::Formatter::write_str(
                            __formatter,
                            "field identifier",
                        )
                    }
                    fn visit_u64<__E>(
                        self,
                        __value: u64,
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            0u64 => _serde::__private228::Ok(__Field::__field0),
                            1u64 => _serde::__private228::Ok(__Field::__field1),
                            2u64 => _serde::__private228::Ok(__Field::__field2),
                            _ => _serde::__private228::Ok(__Field::__ignore),
                        }
                    }
                    fn visit_str<__E>(
                        self,
                        __value: &str,
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            "addend" => _serde::__private228::Ok(__Field::__field0),
                            "loc" => _serde::__private228::Ok(__Field::__field1),
                            "offset" => _serde::__private228::Ok(__Field::__field2),
                            _ => _serde::__private228::Ok(__Field::__ignore),
                        }
                    }
                    fn visit_bytes<__E>(
                        self,
                        __value: &[u8],
                    ) -> _serde::__private228::Result<Self::Value, __E>
                    where
                        __E: _serde::de::Error,
                    {
                        match __value {
                            b"addend" => _serde::__private228::Ok(__Field::__field0),
                            b"loc" => _serde::__private228::Ok(__Field::__field1),
                            b"offset" => _serde::__private228::Ok(__Field::__field2),
                            _ => _serde::__private228::Ok(__Field::__ignore),
                        }
                    }
                }
                #[automatically_derived]
                impl<'de> _serde::Deserialize<'de> for __Field {
                    #[inline]
                    fn deserialize<__D>(
                        __deserializer: __D,
                    ) -> _serde::__private228::Result<Self, __D::Error>
                    where
                        __D: _serde::Deserializer<'de>,
                    {
                        _serde::Deserializer::deserialize_identifier(
                            __deserializer,
                            __FieldVisitor,
                        )
                    }
                }
                #[doc(hidden)]
                struct __Visitor<'de> {
                    marker: _serde::__private228::PhantomData<JITReloc>,
                    lifetime: _serde::__private228::PhantomData<&'de ()>,
                }
                #[automatically_derived]
                impl<'de> _serde::de::Visitor<'de> for __Visitor<'de> {
                    type Value = JITReloc;
                    fn expecting(
                        &self,
                        __formatter: &mut _serde::__private228::Formatter,
                    ) -> _serde::__private228::fmt::Result {
                        _serde::__private228::Formatter::write_str(
                            __formatter,
                            "struct JITReloc",
                        )
                    }
                    #[inline]
                    fn visit_seq<__A>(
                        self,
                        mut __seq: __A,
                    ) -> _serde::__private228::Result<Self::Value, __A::Error>
                    where
                        __A: _serde::de::SeqAccess<'de>,
                    {
                        let __field0 = match _serde::de::SeqAccess::next_element::<
                            i64,
                        >(&mut __seq)? {
                            _serde::__private228::Some(__value) => __value,
                            _serde::__private228::None => {
                                return _serde::__private228::Err(
                                    _serde::de::Error::invalid_length(
                                        0usize,
                                        &"struct JITReloc with 3 elements",
                                    ),
                                );
                            }
                        };
                        let __field1 = match _serde::de::SeqAccess::next_element::<
                            LocSrc,
                        >(&mut __seq)? {
                            _serde::__private228::Some(__value) => __value,
                            _serde::__private228::None => {
                                return _serde::__private228::Err(
                                    _serde::de::Error::invalid_length(
                                        1usize,
                                        &"struct JITReloc with 3 elements",
                                    ),
                                );
                            }
                        };
                        let __field2 = match _serde::de::SeqAccess::next_element::<
                            u32,
                        >(&mut __seq)? {
                            _serde::__private228::Some(__value) => __value,
                            _serde::__private228::None => {
                                return _serde::__private228::Err(
                                    _serde::de::Error::invalid_length(
                                        2usize,
                                        &"struct JITReloc with 3 elements",
                                    ),
                                );
                            }
                        };
                        _serde::__private228::Ok(JITReloc {
                            addend: __field0,
                            loc: __field1,
                            offset: __field2,
                        })
                    }
                    #[inline]
                    fn visit_map<__A>(
                        self,
                        mut __map: __A,
                    ) -> _serde::__private228::Result<Self::Value, __A::Error>
                    where
                        __A: _serde::de::MapAccess<'de>,
                    {
                        let mut __field0: _serde::__private228::Option<i64> = _serde::__private228::None;
                        let mut __field1: _serde::__private228::Option<LocSrc> = _serde::__private228::None;
                        let mut __field2: _serde::__private228::Option<u32> = _serde::__private228::None;
                        while let _serde::__private228::Some(__key) = _serde::de::MapAccess::next_key::<
                            __Field,
                        >(&mut __map)? {
                            match __key {
                                __Field::__field0 => {
                                    if _serde::__private228::Option::is_some(&__field0) {
                                        return _serde::__private228::Err(
                                            <__A::Error as _serde::de::Error>::duplicate_field("addend"),
                                        );
                                    }
                                    __field0 = _serde::__private228::Some(
                                        _serde::de::MapAccess::next_value::<i64>(&mut __map)?,
                                    );
                                }
                                __Field::__field1 => {
                                    if _serde::__private228::Option::is_some(&__field1) {
                                        return _serde::__private228::Err(
                                            <__A::Error as _serde::de::Error>::duplicate_field("loc"),
                                        );
                                    }
                                    __field1 = _serde::__private228::Some(
                                        _serde::de::MapAccess::next_value::<LocSrc>(&mut __map)?,
                                    );
                                }
                                __Field::__field2 => {
                                    if _serde::__private228::Option::is_some(&__field2) {
                                        return _serde::__private228::Err(
                                            <__A::Error as _serde::de::Error>::duplicate_field("offset"),
                                        );
                                    }
                                    __field2 = _serde::__private228::Some(
                                        _serde::de::MapAccess::next_value::<u32>(&mut __map)?,
                                    );
                                }
                                _ => {
                                    let _ = _serde::de::MapAccess::next_value::<
                                        _serde::de::IgnoredAny,
                                    >(&mut __map)?;
                                }
                            }
                        }
                        let __field0 = match __field0 {
                            _serde::__private228::Some(__field0) => __field0,
                            _serde::__private228::None => {
                                _serde::__private228::de::missing_field("addend")?
                            }
                        };
                        let __field1 = match __field1 {
                            _serde::__private228::Some(__field1) => __field1,
                            _serde::__private228::None => {
                                _serde::__private228::de::missing_field("loc")?
                            }
                        };
                        let __field2 = match __field2 {
                            _serde::__private228::Some(__field2) => __field2,
                            _serde::__private228::None => {
                                _serde::__private228::de::missing_field("offset")?
                            }
                        };
                        _serde::__private228::Ok(JITReloc {
                            addend: __field0,
                            loc: __field1,
                            offset: __field2,
                        })
                    }
                }
                #[doc(hidden)]
                const FIELDS: &'static [&'static str] = &["addend", "loc", "offset"];
                _serde::Deserializer::deserialize_struct(
                    __deserializer,
                    "JITReloc",
                    FIELDS,
                    __Visitor {
                        marker: _serde::__private228::PhantomData::<JITReloc>,
                        lifetime: _serde::__private228::PhantomData,
                    },
                )
            }
        }
    };
}
pub mod ints {
    pub mod iint {
        pub trait IIntImpl: Sized + Copy {
            fn carryadd(self, rhs: Self, carry: bool) -> (Self, bool);
            fn borrowsub(self, rhs: Self, borrow: bool) -> (Self, bool);
        }
        impl IIntImpl for i8 {
            #[inline]
            fn carryadd(self, rhs: Self, carry: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_add(rhs);
                let (r2, o2) = r1.overflowing_add(carry as Self);
                (r2, o1 != o2)
            }
            #[inline]
            fn borrowsub(self, rhs: Self, borrow: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_sub(rhs);
                let (r2, o2) = r1.overflowing_sub(borrow as Self);
                (r2, o1 != o2)
            }
        }
        impl IIntImpl for i16 {
            #[inline]
            fn carryadd(self, rhs: Self, carry: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_add(rhs);
                let (r2, o2) = r1.overflowing_add(carry as Self);
                (r2, o1 != o2)
            }
            #[inline]
            fn borrowsub(self, rhs: Self, borrow: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_sub(rhs);
                let (r2, o2) = r1.overflowing_sub(borrow as Self);
                (r2, o1 != o2)
            }
        }
        impl IIntImpl for i32 {
            #[inline]
            fn carryadd(self, rhs: Self, carry: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_add(rhs);
                let (r2, o2) = r1.overflowing_add(carry as Self);
                (r2, o1 != o2)
            }
            #[inline]
            fn borrowsub(self, rhs: Self, borrow: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_sub(rhs);
                let (r2, o2) = r1.overflowing_sub(borrow as Self);
                (r2, o1 != o2)
            }
        }
        impl IIntImpl for i64 {
            #[inline]
            fn carryadd(self, rhs: Self, carry: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_add(rhs);
                let (r2, o2) = r1.overflowing_add(carry as Self);
                (r2, o1 != o2)
            }
            #[inline]
            fn borrowsub(self, rhs: Self, borrow: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_sub(rhs);
                let (r2, o2) = r1.overflowing_sub(borrow as Self);
                (r2, o1 != o2)
            }
        }
        impl IIntImpl for i128 {
            #[inline]
            fn carryadd(self, rhs: Self, carry: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_add(rhs);
                let (r2, o2) = r1.overflowing_add(carry as Self);
                (r2, o1 != o2)
            }
            #[inline]
            fn borrowsub(self, rhs: Self, borrow: bool) -> (Self, bool) {
                let (r1, o1) = self.overflowing_sub(rhs);
                let (r2, o2) = r1.overflowing_sub(borrow as Self);
                (r2, o1 != o2)
            }
        }
    }
    pub mod mul {
        pub trait WideningMul: Sized + Copy {
            /// Returns (low, high)
            fn mul_widen(self, b: Self) -> (Self, Self);
        }
        impl WideningMul for u8 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as u16) * (b as u16);
                (output as u8, ((output as u16) >> 8) as u8)
            }
        }
        impl WideningMul for u16 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as u32) * (b as u32);
                (output as u16, ((output as u32) >> 16) as u16)
            }
        }
        impl WideningMul for u32 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as u64) * (b as u64);
                (output as u32, ((output as u64) >> 32) as u32)
            }
        }
        impl WideningMul for u64 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as u128) * (b as u128);
                (output as u64, ((output as u128) >> 64) as u64)
            }
        }
        impl WideningMul for i8 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as i16) * (b as i16);
                (output as i8, ((output as u16) >> 8) as i8)
            }
        }
        impl WideningMul for i16 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as i32) * (b as i32);
                (output as i16, ((output as u32) >> 16) as i16)
            }
        }
        impl WideningMul for i32 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as i64) * (b as i64);
                (output as i32, ((output as u64) >> 32) as i32)
            }
        }
        impl WideningMul for i64 {
            #[inline]
            fn mul_widen(self, b: Self) -> (Self, Self) {
                let output = (self as i128) * (b as i128);
                (output as i64, ((output as u128) >> 64) as i64)
            }
        }
    }
    pub use iint::*;
    pub use mul::*;
}
pub use ahash;
use std::{
    any::Any, hash::Hash, io::{Read, Seek},
    sync::{Arc, LazyLock, OnceLock},
    thread::{self, available_parallelism},
    time::Duration,
};
use ahash::{HashMap, HashSet};
use moka::sync::{CacheBuilder, SegmentedCache};
use sart::structures::ffi::CallSig;
pub use sart;
use tokio::runtime::{Builder, Runtime};
use crate::{
    acaot::{JITReloc, pickle::def::PickleInstruction},
    management::management_main,
};
pub mod executor {
    pub extern "C" fn corevm_libcall() {}
}
pub mod management {
    use crate::{
        BytecodeResolver, CODE_CACHE, CacheData, FNCALL_DISPATCH, SymbolMapTable,
        ThreadSafe, acaot::pickle::{PickleWorker, def::PickleInstruction},
    };
    use ahash::{HashMap, HashMapExt};
    use rayon::iter::{IntoParallelIterator, ParallelIterator};
    use sart::structures::ffi::CallSig;
    use std::sync::Arc;
    enum ProcessResult {
        Pickle(
            u64,
            Arc<[PickleInstruction]>,
            Arc<ahash::HashMap<u64, usize>>,
            Arc<ahash::HashSet<u64>>,
        ),
        Native(u64, ThreadSafe<*const ()>, CallSig),
        None,
    }
    pub fn management_main(resolve: Arc<dyn BytecodeResolver + Send + Sync + 'static>) {
        let last = resolve.as_ref().last_section_id();
        let mut nativeptr = HashMap::new();
        (0..=last)
            .into_par_iter()
            .map(|id| match resolve.as_ref().resolve_data(id) {
                SymbolMapTable::MixedSizedBytecode { bytecode } => {
                    match resolve.as_ref().get_best_cache(id) {
                        CacheData::None => {
                            let mut worker = PickleWorker {
                                bytecode,
                                libcalls: Default::default(),
                                out: ::alloc::vec::Vec::new(),
                                jump: Default::default(),
                            };
                            worker.pass1();
                            let out: Arc<[PickleInstruction]> = Arc::from(
                                worker.out.into_boxed_slice(),
                            );
                            let jumps = Arc::new(worker.jump);
                            let libcalls = Arc::new(worker.libcalls);
                            CODE_CACHE.insert(id, (out.clone(), jumps.clone()));
                            ProcessResult::Pickle(id, out, jumps, libcalls)
                        }
                        _ => ProcessResult::None,
                    }
                }
                SymbolMapTable::NativePointer { fnptr, cdecl } => {
                    ProcessResult::Native(id, ThreadSafe(fnptr), cdecl)
                }
            })
            .filter_map(|x| match x {
                ProcessResult::None => None,
                e => Some(e),
            })
            .collect::<Box<[_]>>()
            .into_iter()
            .for_each(|outdata| match outdata {
                ProcessResult::Pickle(section, cache, jumps, libcalls) => {
                    resolve
                        .as_ref()
                        .update_cache(
                            section,
                            CacheData::Pickle {
                                out: cache,
                                jumps,
                                libcalls: Some(libcalls),
                            },
                        );
                }
                ProcessResult::Native(module, fnptr, csig) => {
                    _ = nativeptr.insert(module, (fnptr, csig));
                }
                _ => {}
            });
        let _nptr = FNCALL_DISPATCH.get_or_init(|| nativeptr);
    }
}
pub mod permute {
    use std::{num::NonZeroU64, process::abort};
    use rand::{TryRng, rngs::SysRng};
    pub mod range {
        use crate::permute::HashedPermutation;
        pub struct HashedPermutationIter {
            permutation: HashedPermutation,
            current_step: u64,
        }
        impl HashedPermutation {
            pub fn to_iter(&self) -> HashedPermutationIter {
                HashedPermutationIter {
                    permutation: HashedPermutation {
                        seed: self.seed,
                        length: self.length,
                    },
                    current_step: 0,
                }
            }
            pub fn into_iter(self) -> HashedPermutationIter {
                HashedPermutationIter {
                    permutation: self,
                    current_step: 0,
                }
            }
        }
        impl Iterator for HashedPermutationIter {
            type Item = u64;
            fn next(&mut self) -> Option<Self::Item> {
                let result = self.permutation.shuffle(self.current_step);
                if result.is_some() {
                    self.current_step += 1;
                }
                result
            }
        }
    }
    pub struct HashedPermutation {
        pub seed: u64,
        pub length: NonZeroU64,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for HashedPermutation {
        #[inline]
        fn clone(&self) -> HashedPermutation {
            HashedPermutation {
                seed: ::core::clone::Clone::clone(&self.seed),
                length: ::core::clone::Clone::clone(&self.length),
            }
        }
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for HashedPermutation {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "HashedPermutation",
                "seed",
                &self.seed,
                "length",
                &&self.length,
            )
        }
    }
    impl HashedPermutation {
        pub const fn new_with_seed(length: NonZeroU64, seed: u64) -> Self {
            HashedPermutation { length, seed }
        }
        pub fn new_panicking(length: NonZeroU64) -> Self {
            let Ok(seed) = SysRng.try_next_u64() else {
                {
                    ::std::io::_print(
                        format_args!("RNG Error : Unsupported/Unexpected\n"),
                    );
                };
                abort();
            };
            HashedPermutation { length, seed }
        }
        pub fn new(length: NonZeroU64) -> Result<Self, rand::rngs::SysError> {
            let seed = SysRng.try_next_u64()?;
            Ok(HashedPermutation { length, seed })
        }
        pub const fn shuffle(&self, input_index: u64) -> Option<u64> {
            let max_length = self.length.get();
            if input_index >= max_length {
                return None;
            }
            let mut current_index = input_index;
            let seed = self.seed;
            let mut bitmask = max_length - 1;
            bitmask |= bitmask >> 1;
            bitmask |= bitmask >> 2;
            bitmask |= bitmask >> 4;
            bitmask |= bitmask >> 8;
            bitmask |= bitmask >> 16;
            bitmask |= bitmask >> 32;
            loop {
                current_index ^= seed;
                current_index = current_index.wrapping_mul(0xd6e8feb86659fd93);
                current_index ^= seed >> 16;
                current_index ^= (current_index & bitmask) >> 4;
                current_index ^= seed >> 8;
                current_index = current_index.wrapping_mul(0x7c79e5af0654199d);
                current_index ^= seed >> 23;
                current_index ^= (current_index & bitmask) >> 1;
                current_index = current_index.wrapping_mul(1 | seed >> 27);
                current_index = current_index.wrapping_mul(0x5c4e40e7a57a55c5);
                current_index ^= (current_index & bitmask) >> 11;
                current_index = current_index.wrapping_mul(0x27cf5c4d32f5d0b5);
                current_index ^= (current_index & bitmask) >> 2;
                current_index = current_index.wrapping_mul(0x9e3779b97f4a7c15);
                current_index ^= (current_index & bitmask) >> 2;
                current_index = current_index.wrapping_mul(0xc6a4a7935bd1e995);
                current_index &= bitmask;
                current_index ^= current_index >> 5;
                if current_index < max_length {
                    break;
                }
            }
            Some((current_index.wrapping_add(seed)) % max_length)
        }
    }
    pub struct ShuffledSliceIter<'a, T> {
        slice: &'a [T],
        permutation: HashedPermutation,
        current_step: u64,
    }
    impl<'a, T> ShuffledSliceIter<'a, T> {
        pub fn new(slice: &'a [T], seed: u64) -> Self {
            let length = NonZeroU64::new(slice.len() as u64)
                .unwrap_or(NonZeroU64::new(1).unwrap());
            Self {
                slice,
                permutation: HashedPermutation::new_with_seed(length, seed),
                current_step: 0,
            }
        }
        pub fn new_panicking(slice: &'a [T]) -> Self {
            let length = NonZeroU64::new(slice.len() as u64)
                .unwrap_or_else(|| NonZeroU64::new(1).unwrap());
            Self {
                slice,
                permutation: HashedPermutation::new_panicking(length),
                current_step: 0,
            }
        }
        pub fn get(&self, idx: usize) -> Option<&T> {
            let random_index = self.permutation.shuffle(idx as _)?;
            self.slice.get(random_index as usize)
        }
    }
    impl<'a, T> Iterator for ShuffledSliceIter<'a, T> {
        type Item = &'a T;
        fn next(&mut self) -> Option<Self::Item> {
            let random_index = self.permutation.shuffle(self.current_step)?;
            self.current_step += 1;
            self.slice.get(random_index as usize)
        }
    }
}
pub mod sync {
    use std::{
        cell::UnsafeCell, hint::cold_path, mem::zeroed,
        ptr::{self, addr_of_mut, null_mut},
        sync::{Arc, OnceLock, atomic::{Ordering, compiler_fence}},
    };
    use sart::{ctr::VMTaskState, salloc, structures::QuadPackedData};
    use crate::{
        CODE_CACHE, SymbolMapTable, VM,
        acaot::pickle::{
            PickleWorker,
            def::{
                PICKLE_DISPATCH_TABLE, PICKLE_OPCODE_HINT, PICKLE_OPCODE_MARK,
                PickleInstruction,
            },
            implementation::{SIZE_128KB, WorkingSet},
        },
    };
    pub static GLOBAL_DATA: OnceLock<UnSafePtr<u8>> = OnceLock::new();
    pub struct UnSafePtr<T>(pub *mut T);
    unsafe impl<T> Send for UnSafePtr<T> {}
    unsafe impl<T> Sync for UnSafePtr<T> {}
    const SCRATCHPAD: usize = 50 * 24 * size_of::<QuadPackedData>();
    pub struct VMState {
        pub ws: WorkingSet,
        pub ts: [VMTaskState; 50],
        pub cindex: usize,
    }
    impl Drop for VMState {
        fn drop(&mut self) {
            unsafe {
                salloc::aligned_free(self.ws.largepad as _);
                salloc::aligned_free(self.ts[0].scratchpad as _);
                if !self.ws.ame.is_null() {
                    salloc::aligned_free(self.ws.ame as _);
                }
            }
        }
    }
    pub const VMSTAT: ::std::thread::LocalKey<UnsafeCell<VMState>> = {
        #[inline]
        fn __rust_std_internal_init_fn() -> UnsafeCell<VMState> {
            UnsafeCell::new(VMState {
                ws: WorkingSet {
                    arr: &[],
                    largepad: unsafe { salloc::aligned_malloc(SIZE_128KB, 8) as _ },
                    largepad_cursor: 0,
                    ame: null_mut(),
                    ame_free: true,
                    jmp: (0, 0),
                    relocmap: Default::default(),
                },
                ts: unsafe {
                    let mut ts: [VMTaskState; 50] = zeroed();
                    let alloca = salloc::aligned_malloc(SCRATCHPAD, 64)
                        as *mut QuadPackedData;
                    for (i, t) in ts.iter_mut().enumerate() {
                        t.scratchpad = alloca.add(i * 24);
                    }
                    ts
                },
                cindex: 0,
            })
        }
        unsafe {
            ::std::thread::LocalKey::new(const {
                if ::std::mem::needs_drop::<UnsafeCell<VMState>>() {
                    |__rust_std_internal_init| {
                        #[thread_local]
                        static __RUST_STD_INTERNAL_VAL: ::std::thread::local_impl::LazyStorage<
                            UnsafeCell<VMState>,
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
                            UnsafeCell<VMState>,
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
    impl VM {
        pub fn fncall(
            &self,
            sectionid: u64,
            oldtsk: *mut VMTaskState,
        ) -> [QuadPackedData; 2] {
            VMSTAT
                .with(|p| {
                    let p = p.get();
                    unsafe {
                        (*p).cindex += 1;
                        ptr::write((*p).ts.as_mut_ptr().add((*p).cindex), *oldtsk)
                    };
                });
            self.dispatch_chocolate::<true>(sectionid);
            VMSTAT
                .with(|p| {
                    let p = p.get();
                    unsafe {
                        let resp = (*p).ts.get_unchecked((*p).cindex);
                        (*p).cindex -= 1;
                        [resp.r7, resp.r8]
                    }
                })
        }
        pub fn call_section(&self, sectionid: u64) {
            return self.dispatch_chocolate::<true>(sectionid);
        }
        #[inline(always)]
        pub fn dispatch_chocolate<const JMPTOJIT: bool>(&self, sectionid: u64) {
            let Some((data, jumps)) = CODE_CACHE.get(&sectionid) else {
                return self
                    .pickle_section(sectionid, Self::dispatch_chocolate::<JMPTOJIT>);
            };
            let leng = data.len();
            #[allow(unused)]
            let mut jumptomark = None;
            #[allow(unused)]
            let mut run_jit = false;
            VMSTAT
                .with(|x| unsafe {
                    let t = x.get();
                    (*t).ws.jmp = (0, jumps.get(&0).map(|x| *x).unwrap_or_default());
                    (*t).ws.relocmap = jumps;
                    let ts = (*t).ts.as_mut_ptr().add((*t).cindex as usize);
                    (*ts).engine_or_pt.pt = self as *const _ as _;
                    (*ts).curline_or_resume.usi = 0;
                    'jcheck: loop {
                        let dt = data.as_ref();
                        loop {
                            if (*ts).curline_or_resume.usi == leng {
                                break 'jcheck;
                            }
                            let pickle = dt.get_unchecked((*ts).curline_or_resume.usi);
                            if pickle.opcode == PICKLE_OPCODE_HINT
                                && PICKLE_OPCODE_MARK == pickle.u1
                            {
                                let data: [PickleInstruction; 2] = {
                                    dt[((*ts).curline_or_resume.usi
                                            + 1)..(*ts).curline_or_resume.usi + 3]
                                        .try_into()
                                        .unwrap()
                                };
                                let out = u64::from_le_bytes([
                                    data[0].opcode,
                                    data[0].u1,
                                    data[0].u2,
                                    data[0].u3,
                                    data[1].opcode,
                                    data[1].u1,
                                    data[1].u2,
                                    data[1].u3,
                                ]);
                                jumptomark = Some(out);
                                (*ts).curline_or_resume.usi += 3;
                                continue 'jcheck;
                            }
                            if pickle.opcode == PICKLE_OPCODE_HINT {
                                let dptr = dt.as_ptr();
                                (*ts).engine_or_pt.pt = dptr as _;
                            }
                            compiler_fence(Ordering::SeqCst);
                            (PICKLE_DISPATCH_TABLE
                                .get_unchecked(
                                    pickle.opcode as usize,
                                ))(pickle, &raw mut (*t).ws, ts);
                            compiler_fence(Ordering::SeqCst);
                            (*ts).curline_or_resume.usi += 1;
                        }
                    }
                });
            cold_path();
            return self.ame_free(sectionid);
        }
        fn pickle_section(
            &self,
            sectionid: u64,
            dispatch: fn(vm: &VM, sectionid: u64) -> (),
        ) {
            let SymbolMapTable::MixedSizedBytecode { bytecode } = self
                .resolve
                .resolve_data(sectionid) else {
                return;
            };
            let mut worker = PickleWorker {
                bytecode,
                libcalls: Default::default(),
                out: ::alloc::vec::Vec::new(),
                jump: Default::default(),
            };
            worker.pass1();
            let out: Arc<[PickleInstruction]> = Arc::from(worker.out.into_boxed_slice());
            CODE_CACHE.insert(sectionid, (out, Arc::new(worker.jump)));
            CODE_CACHE.run_pending_tasks();
            return dispatch(self, sectionid);
        }
        fn ame_free(&self, _sectionid: u64) {
            VMSTAT
                .with(|vtsk| unsafe {
                    let vm = &mut *vtsk.get();
                    for tsk in &mut vm.ts {
                        if !tsk.ame.is_null() {
                            vm.ws.freeame(tsk.ame);
                            tsk.ame = null_mut();
                        }
                    }
                })
        }
    }
}
pub static TOTAL_THREADS: LazyLock<usize> = LazyLock::new(|| {
    available_parallelism().unwrap().into()
});
static VMMADE: OnceLock<()> = OnceLock::new();
pub enum SymbolMapTable<T> {
    NativePointer { fnptr: *const (), cdecl: CallSig },
    MixedSizedBytecode { bytecode: T },
}
pub enum SymbolMapTableInfo {
    NativePointer,
    MixedSizedBytecode,
}
pub type JITRelocs = Arc<[JITReloc]>;
pub type LibCalls = HashSet<u64>;
pub type SaVMJumps = HashMap<u64, usize>;
pub enum CacheData {
    None,
    Pickle {
        out: Arc<[PickleInstruction]>,
        jumps: Arc<SaVMJumps>,
        /// This should be None for returned CacheData
        libcalls: Option<Arc<LibCalls>>,
    },
    JITCache { level: CacheLevel, binary: Arc<[u8]>, reloc: JITRelocs },
}
#[automatically_derived]
impl ::core::fmt::Debug for CacheData {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match self {
            CacheData::None => ::core::fmt::Formatter::write_str(f, "None"),
            CacheData::Pickle { out: __self_0, jumps: __self_1, libcalls: __self_2 } => {
                ::core::fmt::Formatter::debug_struct_field3_finish(
                    f,
                    "Pickle",
                    "out",
                    __self_0,
                    "jumps",
                    __self_1,
                    "libcalls",
                    &__self_2,
                )
            }
            CacheData::JITCache {
                level: __self_0,
                binary: __self_1,
                reloc: __self_2,
            } => {
                ::core::fmt::Formatter::debug_struct_field3_finish(
                    f,
                    "JITCache",
                    "level",
                    __self_0,
                    "binary",
                    __self_1,
                    "reloc",
                    &__self_2,
                )
            }
        }
    }
}
#[automatically_derived]
impl ::core::clone::Clone for CacheData {
    #[inline]
    fn clone(&self) -> CacheData {
        match self {
            CacheData::None => CacheData::None,
            CacheData::Pickle { out: __self_0, jumps: __self_1, libcalls: __self_2 } => {
                CacheData::Pickle {
                    out: ::core::clone::Clone::clone(__self_0),
                    jumps: ::core::clone::Clone::clone(__self_1),
                    libcalls: ::core::clone::Clone::clone(__self_2),
                }
            }
            CacheData::JITCache {
                level: __self_0,
                binary: __self_1,
                reloc: __self_2,
            } => {
                CacheData::JITCache {
                    level: ::core::clone::Clone::clone(__self_0),
                    binary: ::core::clone::Clone::clone(__self_1),
                    reloc: ::core::clone::Clone::clone(__self_2),
                }
            }
        }
    }
}
unsafe impl Send for CacheData {}
unsafe impl Sync for CacheData {}
pub enum CacheLevel {
    Pickle,
    CraneliftCrafter,
    CraneliftEpicenter,
    LLVMCinder,
    LLVMCrater,
    LLVMEpitome,
}
#[automatically_derived]
impl ::core::fmt::Debug for CacheLevel {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::write_str(
            f,
            match self {
                CacheLevel::Pickle => "Pickle",
                CacheLevel::CraneliftCrafter => "CraneliftCrafter",
                CacheLevel::CraneliftEpicenter => "CraneliftEpicenter",
                CacheLevel::LLVMCinder => "LLVMCinder",
                CacheLevel::LLVMCrater => "LLVMCrater",
                CacheLevel::LLVMEpitome => "LLVMEpitome",
            },
        )
    }
}
#[automatically_derived]
#[doc(hidden)]
unsafe impl ::core::clone::TrivialClone for CacheLevel {}
#[automatically_derived]
impl ::core::clone::Clone for CacheLevel {
    #[inline]
    fn clone(&self) -> CacheLevel {
        *self
    }
}
#[automatically_derived]
impl ::core::marker::Copy for CacheLevel {}
pub const OPTLEVEL_PICKLE: i64 = 0;
impl CacheLevel {
    pub fn to_int(&self) -> u8 {
        match self {
            Self::Pickle => 0,
            Self::LLVMCinder => 1,
            Self::CraneliftCrafter => 2,
            Self::CraneliftEpicenter => 3,
            Self::LLVMCrater => 4,
            Self::LLVMEpitome => 5,
        }
    }
    pub fn from_int(cachelevel: i64) -> Option<Self> {
        Some(
            match cachelevel {
                0 => Self::Pickle,
                1 => Self::LLVMCinder,
                2 => Self::CraneliftCrafter,
                3 => Self::CraneliftEpicenter,
                4 => Self::LLVMCrater,
                5 => Self::LLVMEpitome,
                _ => return None,
            },
        )
    }
}
pub trait ResolvedData: Read + Seek {}
impl<T: Read + Seek> ResolvedData for T {}
pub trait BytecodeResolver: Any {
    /// Return the id of the LAST VALID section
    /// We use this to prevent unnecessary [u64] allocation
    fn last_section_id(&self) -> u64;
    /// Returns an heuristic list upto 500 elements in size over 2 clusters
    ///
    /// Cluster 1 (idx = 0)
    /// - Absolute top-notch priority entitled to DIRECT upgrade the the highest JIT Level
    ///
    /// Cluster 2 (idx = 1)
    /// - Priority over other modules
    fn heuristic_pgo(&self) -> [&[u64]; 2];
    /// Resolve the symbol map table
    fn resolve_data(&self, section: u64) -> SymbolMapTable<Box<dyn ResolvedData>>;
    /// Learn about the data present
    fn learn_data(&self, section: u64) -> SymbolMapTableInfo;
    /// Checks if the cache is available!
    fn get_best_cache(&self, section: u64) -> CacheData;
    /// Checks if the cache is available!
    fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData;
    /// Gets the SaVM libraries it depends on
    fn get_libcalls(&self, section: u64) -> Option<Arc<HashSet<u64>>>;
    /// Updates the cache
    ///
    /// We hope the callee only updates the tier of cache this produces
    ///
    /// eg. we hope it does not replace Pickle code with Cranelift code as that'll lead to performance losses next round
    fn update_cache(&self, section: u64, cache: CacheData);
}
impl BytecodeResolver for Box<dyn BytecodeResolver + Send + Sync + 'static> {
    fn get_best_cache(&self, section: u64) -> CacheData {
        BytecodeResolver::get_best_cache(self.as_ref(), section)
    }
    fn heuristic_pgo(&self) -> [&[u64]; 2] {
        BytecodeResolver::heuristic_pgo(self.as_ref())
    }
    fn get_libcalls(&self, section: u64) -> Option<Arc<HashSet<u64>>> {
        BytecodeResolver::get_libcalls(self.as_ref(), section)
    }
    fn resolve_data(&self, section: u64) -> SymbolMapTable<Box<dyn ResolvedData>> {
        BytecodeResolver::resolve_data(self.as_ref(), section)
    }
    fn learn_data(&self, section: u64) -> SymbolMapTableInfo {
        BytecodeResolver::learn_data(self.as_ref(), section)
    }
    fn last_section_id(&self) -> u64 {
        BytecodeResolver::last_section_id(self.as_ref())
    }
    fn update_cache(&self, section: u64, cache: CacheData) {
        BytecodeResolver::update_cache(self.as_ref(), section, cache)
    }
    fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData {
        BytecodeResolver::get_cache(self.as_ref(), section, level)
    }
}
pub static GLOBAL_RUNTIME: LazyLock<Runtime> = LazyLock::new(|| {
    Builder::new_multi_thread().enable_all().build().unwrap()
});
pub(crate) static FNCALL_DISPATCH: OnceLock<
    HashMap<u64, (ThreadSafe<*const ()>, CallSig)>,
> = OnceLock::new();
pub(crate) static CODE_CACHE: LazyLock<
    SegmentedCache<
        u64,
        (Arc<[PickleInstruction]>, Arc<HashMap<u64, usize>>),
        ahash::RandomState,
    >,
> = LazyLock::new(|| {
    CacheBuilder::new(1 << 10)
        .segments(available_parallelism().map(|x| x.get()).unwrap_or(4))
        .time_to_live(Duration::from_mins(20))
        .time_to_idle(Duration::from_mins(5))
        .build_with_hasher(ahash::RandomState::default())
});
pub struct ThreadSafe<T>(pub T);
#[automatically_derived]
impl<T: ::core::fmt::Debug> ::core::fmt::Debug for ThreadSafe<T> {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ThreadSafe", &&self.0)
    }
}
#[automatically_derived]
impl<T: ::core::clone::Clone> ::core::clone::Clone for ThreadSafe<T> {
    #[inline]
    fn clone(&self) -> ThreadSafe<T> {
        ThreadSafe(::core::clone::Clone::clone(&self.0))
    }
}
#[automatically_derived]
impl<T: ::core::marker::Copy> ::core::marker::Copy for ThreadSafe<T> {}
unsafe impl<T> Send for ThreadSafe<T> {}
unsafe impl<T> Sync for ThreadSafe<T> {}
impl<T: Hash> Hash for ThreadSafe<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T: PartialEq> PartialEq for ThreadSafe<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
    fn ne(&self, other: &Self) -> bool {
        self.0.ne(&other.0)
    }
}
/// We create a VM for each thread executed
#[repr(C)]
pub struct VM {
    pub resolve: Arc<dyn BytecodeResolver + Send + Sync + 'static>,
}
unsafe impl Send for VM {}
unsafe impl Sync for VM {}
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
impl VM {
    pub fn new<T: BytecodeResolver + Send + Sync + 'static>(data: T) -> Self {
        unsafe { Self::new_unsafe::<T, true>(data) }
    }
    /// Please note that module id `0` represents the main module
    ///
    /// This is not really `unsafe`
    /// This is **unsafe** by intent
    pub unsafe fn new_unsafe<
        T: BytecodeResolver + Send + Sync + 'static,
        const MGNTHTREAD: bool,
    >(data: T) -> Self {
        CODE_CACHE.run_pending_tasks();
        VMMADE.set(()).expect("Each process can only have 1 VM");
        let resolver = Arc::new(data);
        if MGNTHTREAD {
            let resolve = resolver.clone();
            thread::spawn(move || {
                management_main(resolve);
            });
        }
        Self { resolve: resolver }
    }
}
pub enum MaybeBoxed<T> {
    Boxed(Box<T>),
    Unboxed(T),
}
