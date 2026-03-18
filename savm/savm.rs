#![feature(prelude_import)]
#![allow(unused)]
#![feature(
    seek_stream_len,
    signed_bigint_helpers,
    exact_div,
    int_roundings,
    nonpoison_rwlock,
    sync_nonpoison,
    unsafe_cell_access,
    read_array,
    widening_mul
)]
extern crate std;
#[prelude_import]
use std::prelude::rust_2024::*;
pub mod acaot {
    pub mod pickle {
        use std::{collections::HashMap, io::{Read, Seek}};
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
            pub const PICKLE_OPCODE_VBOP: u8 = 25;
            pub const PICKLE_OPCODE_VROT: u8 = 26;
            pub const PICKLE_OPCODE_VSH: u8 = 27;
            pub const PICKLE_OPCODE_VCNT: u8 = 28;
            pub const PICKLE_OPCODE_VMINIMAX: u8 = 29;
            pub const PICKLE_OPCODE_VFMA: u8 = 30;
            pub const PICKLE_OPCODE_SYNCCALL: u8 = 31;
            pub const PICKLE_OPCODE_ASYNCCALL: u8 = 32;
            pub const PICKLE_OPCODE_SPAWN: u8 = 33;
            pub const PICKLE_OPCODE_TASK: u8 = 34;
            pub const PICKLE_OPCODE_ATOMIC: u8 = 36;
            const TOTAL_ITEMS: usize = data("HINT") + data("WS_PUT") + data("MOV")
                + data("REG") + data("MARK") + data("JMP") + data("JIF") + data("VCMP")
                + data("SCRATCH") + data("VCOPY") + data("VADD") + data("VADDF")
                + data("VSUB") + data("VSUBF") + data("VMUL") + data("VMULF")
                + data("VDIVF") + data("DIV") + data("REM") + data("CAST") + data("VNEG")
                + data("VABS") + data("VFOP") + data("VFCAST") + data("VBIT")
                + data("VBOP") + data("VROT") + data("VSH") + data("VCNT")
                + data("VMINIMAX") + data("VFMA") + data("SYNCCALL") + data("ASYNCCALL")
                + data("SPAWN") + data("TASK") + data("ATOMIC") + 0;
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
                call_vbop,
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
        pub mod implementation {
            use std::{
                collections::HashMap, hint::cold_path, mem::{transmute_copy, zeroed},
                ops::Sub, ptr::{addr_of_mut, read_unaligned},
                sync::LazyLock,
            };
            mod mu {
                //! Memory Unit
                use std::ptr::{null_mut, replace};
                use sart::ctr::VMTaskState;
                use crate::{
                    acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
                    resolve,
                };
                mod vcopy {
                    use sart::ctr::VMTaskState;
                    use std::ptr;
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve_location_src,
                    };
                    pub fn call_vcopy(
                        pickle: &PickleInstruction,
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
                    ) {
                        let count_bit = pickle.u1;
                        let _ = pickle.u2;
                        let srcflags = pickle.u3;
                        let src1flags = srcflags >> 4;
                        let src2flags = srcflags & 0x0F;
                        let count_data = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[0..4].try_into().unwrap_unchecked(),
                            )
                        };
                        let count = if count_bit == 0 {
                            count_data
                        } else {
                            unsafe { taskstate.r1.u32 }
                        };
                        let baseoffset = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[4..8].try_into().unwrap_unchecked(),
                            )
                        };
                        let targetoffset = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let src1 = unsafe {
                            (match src1flags {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            } as *mut u8)
                                .offset(baseoffset as _)
                        };
                        let target = unsafe {
                            (match src2flags {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            } as *mut u8)
                                .offset(targetoffset as _)
                        };
                        unsafe { ptr::copy(src1, target, count as _) };
                    }
                }
                pub use vcopy::*;
                mod cast {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve_location_src,
                    };
                    use sart::ctr::VMTaskState;
                    use std::ptr;
                    pub fn call_cast(
                        pickle: &PickleInstruction,
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
                    ) {
                        unsafe {
                            let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                            let offset1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[0..4].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let src1 = {
                                let src1 = (flags as u8) >> 4;
                                match src1 {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = {
                                let target1 = (flags as u8) & 0x0F;
                                match target1 {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let tag_initial = (flags >> 12) as u8;
                            let tag_final = ((flags >> 8) as u8) & 0x0F;
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
                mod vops {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve, resolve_location_src,
                    };
                    use sart::ctr::VMTaskState;
                    use std::{ops::Neg, ptr::{self, addr_of_mut}};
                    pub fn call_vneg(
                        pickle: &PickleInstruction,
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
                    ) {
                        let (count, typetag, src1, target, offset1, offset2) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let typetag = (flags >> 12) as u8;
                            let countbit = ((flags >> 4) & 0x01) as u8;
                            let count_data = unsafe {
                                <u32>::from_ne_bytes(
                                    ws.arr[0..4].try_into().unwrap_unchecked(),
                                )
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { taskstate.r1.u32 }
                            };
                            let offset1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[8..12].try_into().unwrap_unchecked(),
                                )
                            };
                            let src1 = unsafe {
                                let src = (flags >> 8 as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                let src = ((flags >> 4) as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
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
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
                    ) {
                        let (count, typetag, src1, target, offset1, offset2) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let typetag = (flags >> 12) as u8;
                            let countbit = ((flags >> 4) & 0x01) as u8;
                            let count_data = unsafe {
                                <u32>::from_ne_bytes(
                                    ws.arr[0..4].try_into().unwrap_unchecked(),
                                )
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { taskstate.r1.u32 }
                            };
                            let offset1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[8..12].try_into().unwrap_unchecked(),
                                )
                            };
                            let src1 = unsafe {
                                let src = (flags >> 8 as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                let src = ((flags >> 4) as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
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
                mod vbit {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr;
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
                    const _DISPATCH: [fn(
                        src1: *mut QuadPackedData,
                        src2: *mut QuadPackedData,
                        src3: *mut QuadPackedData,
                        offset1: i32,
                        offset2: i32,
                        offset3: i32,
                        count: u32,
                    ); 36] = [
                        vbitop_and_u8,
                        vbitop_and_u16,
                        vbitop_and_u32,
                        vbitop_and_u64,
                        vbitop_or_u8,
                        vbitop_or_u16,
                        vbitop_or_u32,
                        vbitop_or_u64,
                        vbitop_xor_u8,
                        vbitop_xor_u16,
                        vbitop_xor_u32,
                        vbitop_xor_u64,
                        vbitop_not_u8,
                        vbitop_not_u16,
                        vbitop_not_u32,
                        vbitop_not_u64,
                        vbitop_or_not_u8,
                        vbitop_or_not_u16,
                        vbitop_or_not_u32,
                        vbitop_or_not_u64,
                        vbitop_and_not_u8,
                        vbitop_and_not_u16,
                        vbitop_and_not_u32,
                        vbitop_and_not_u64,
                        vbitop_xor_not_u8,
                        vbitop_xor_not_u16,
                        vbitop_xor_not_u32,
                        vbitop_xor_not_u64,
                        vbitop_bitrev_u8,
                        vbitop_bitrev_u16,
                        vbitop_bitrev_u32,
                        vbitop_bitrev_u64,
                        vbitop_bswap_u8,
                        vbitop_bswap_u16,
                        vbitop_bswap_u32,
                        vbitop_bswap_u64,
                    ];
                    pub fn call_vbit(
                        pickle: &PickleInstruction,
                        ws: &mut WorkingSet,
                        ts: &mut VMTaskState,
                    ) {
                        unsafe {
                            let countbit = pickle.u3;
                            let flags = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                            let width = (flags >> 14) as u8;
                            let count = {
                                let countdata = unsafe {
                                    <u32>::from_ne_bytes(
                                        ws.arr[0..4].try_into().unwrap_unchecked(),
                                    )
                                };
                                if countbit == 0 { countdata } else { ts.r1.u32 }
                            };
                            let flags_src1 = (flags as u8) & 0x0F;
                            let flags_src2 = (flags as u8) >> 4 & 0x0F;
                            let flags_tg = (flags >> 12) as u8 & 0x0F;
                            let src1 = match flags_src1 {
                                0 => &raw mut ts.r1,
                                1 => &raw mut ts.r2,
                                2 => &raw mut ts.r3,
                                3 => &raw mut ts.r4,
                                4 => &raw mut ts.r5,
                                5 => &raw mut ts.r6,
                                6 => &raw mut ts.r7,
                                7 => &raw mut ts.r8,
                                8 => ts.scratchpad,
                                9 => ts.largepad,
                                10 => unsafe { ts.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            };
                            let src2 = match flags_src2 {
                                0 => &raw mut ts.r1,
                                1 => &raw mut ts.r2,
                                2 => &raw mut ts.r3,
                                3 => &raw mut ts.r4,
                                4 => &raw mut ts.r5,
                                5 => &raw mut ts.r6,
                                6 => &raw mut ts.r7,
                                7 => &raw mut ts.r8,
                                8 => ts.scratchpad,
                                9 => ts.largepad,
                                10 => unsafe { ts.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            };
                            let tg = match flags_tg {
                                0 => &raw mut ts.r1,
                                1 => &raw mut ts.r2,
                                2 => &raw mut ts.r3,
                                3 => &raw mut ts.r4,
                                4 => &raw mut ts.r5,
                                5 => &raw mut ts.r6,
                                6 => &raw mut ts.r7,
                                7 => &raw mut ts.r8,
                                8 => ts.scratchpad,
                                9 => ts.largepad,
                                10 => unsafe { ts.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            };
                            let of_src1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let of_src2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[8..12].try_into().unwrap_unchecked(),
                                )
                            };
                            let of_tg = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[12..16].try_into().unwrap_unchecked(),
                                )
                            };
                        }
                    }
                }
                pub use vbit::*;
                pub fn call_scratch(
                    pickle: &PickleInstruction,
                    ws: &mut WorkingSet,
                    taskstate: &mut VMTaskState,
                ) {
                    let op_class = pickle.u1;
                    let payload = u16::from_ne_bytes([pickle.u2, pickle.u3]);
                    match op_class {
                        0b00 => {
                            let size_reg = (payload as u8 >> 4);
                            let align_reg = (payload as u8 & 0x0F);
                            unsafe {
                                let size = match size_reg {
                                    0 => taskstate.r1,
                                    1 => taskstate.r2,
                                    2 => taskstate.r3,
                                    3 => taskstate.r4,
                                    4 => taskstate.r5,
                                    5 => taskstate.r6,
                                    6 => taskstate.r7,
                                    7 => taskstate.r8,
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                                    .u64;
                                let align = match align_reg {
                                    0 => taskstate.r1,
                                    1 => taskstate.r2,
                                    2 => taskstate.r3,
                                    3 => taskstate.r4,
                                    4 => taskstate.r5,
                                    5 => taskstate.r6,
                                    6 => taskstate.r7,
                                    7 => taskstate.r8,
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                                    .u64;
                                if true {
                                    if !taskstate.largepad.is_null() {
                                        ::core::panicking::panic(
                                            "assertion failed: taskstate.largepad.is_null()",
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
                                taskstate.largepad = ws.allocate(size, align);
                            }
                        }
                        0b01 => {
                            unsafe {
                                let pt = taskstate.largepad;
                                taskstate.largepad = null_mut();
                                ws.free(pt);
                            }
                        }
                        0b10 => {
                            unsafe {
                                let pt = taskstate.largepad;
                                taskstate.largepad = null_mut();
                                ws.salloc_free(pt);
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
            pub use mu::*;
            mod au {
                use crate::{
                    acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
                    arrcastint, resolve, resolve_location_src,
                };
                use sart::ctr::VMTaskState;
                use std::ptr::{self, addr_of_mut};
                mod fp {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve, resolve_location_src,
                    };
                    use sart::ctr::VMTaskState;
                    use std::{
                        ops::{Add, Div, Mul, Sub},
                        ptr::{self, addr_of_mut},
                    };
                    pub fn call_vaddf(
                        pickle: &PickleInstruction,
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
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
                        ) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let countbit = ((flags >> 12) & 0x01) as u8;
                            let fptype = ((flags >> 13) & 0x01) as u8;
                            let inst = ((flags >> 14) & 0x01) as u8;
                            let count_data = unsafe {
                                <u32>::from_ne_bytes(
                                    ws.arr[0..4].try_into().unwrap_unchecked(),
                                )
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { taskstate.r1.u32 }
                            };
                            let offset1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[8..12].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset3 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[12..16].try_into().unwrap_unchecked(),
                                )
                            };
                            let src1 = unsafe {
                                let src = (flags >> 8 as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                let src = (flags as u8) >> 4;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                let src = (flags as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            (
                                inst,
                                fptype,
                                count,
                                src1,
                                src2,
                                target,
                                offset1,
                                offset2,
                                offset3,
                            )
                        };
                        unsafe {
                            match (fptype) {
                                0 => {
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
                                1 => {
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
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
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
                        ) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let countbit = ((flags >> 12) & 0x01) as u8;
                            let fptype = ((flags >> 13) & 0x01) as u8;
                            let inst = ((flags >> 14) & 0x01) as u8;
                            let count_data = unsafe {
                                <u32>::from_ne_bytes(
                                    ws.arr[0..4].try_into().unwrap_unchecked(),
                                )
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { taskstate.r1.u32 }
                            };
                            let offset1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[8..12].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset3 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[12..16].try_into().unwrap_unchecked(),
                                )
                            };
                            let src1 = unsafe {
                                let src = (flags >> 8 as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                let src = (flags as u8) >> 4;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                let src = (flags as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            (
                                inst,
                                fptype,
                                count,
                                src1,
                                src2,
                                target,
                                offset1,
                                offset2,
                                offset3,
                            )
                        };
                        unsafe {
                            match (fptype) {
                                0 => {
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
                                1 => {
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
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
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
                        ) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let countbit = ((flags >> 12) & 0x01) as u8;
                            let fptype = ((flags >> 13) & 0x01) as u8;
                            let inst = ((flags >> 14) & 0x01) as u8;
                            let count_data = unsafe {
                                <u32>::from_ne_bytes(
                                    ws.arr[0..4].try_into().unwrap_unchecked(),
                                )
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { taskstate.r1.u32 }
                            };
                            let offset1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[8..12].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset3 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[12..16].try_into().unwrap_unchecked(),
                                )
                            };
                            let src1 = unsafe {
                                let src = (flags >> 8 as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                let src = (flags as u8) >> 4;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                let src = (flags as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            (
                                inst,
                                fptype,
                                count,
                                src1,
                                src2,
                                target,
                                offset1,
                                offset2,
                                offset3,
                            )
                        };
                        unsafe {
                            match (fptype) {
                                0 => {
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
                                1 => {
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
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
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
                        ) = {
                            let f1 = pickle.u1;
                            let f2 = pickle.u2;
                            let flags = u16::from_ne_bytes([f1, f2]);
                            let countbit = ((flags >> 12) & 0x01) as u8;
                            let fptype = ((flags >> 13) & 0x01) as u8;
                            let inst = ((flags >> 14) & 0x01) as u8;
                            let count_data = unsafe {
                                <u32>::from_ne_bytes(
                                    ws.arr[0..4].try_into().unwrap_unchecked(),
                                )
                            };
                            let count = if (countbit == 0) {
                                count_data
                            } else {
                                unsafe { taskstate.r1.u32 }
                            };
                            let offset1 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[4..8].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset2 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[8..12].try_into().unwrap_unchecked(),
                                )
                            };
                            let offset3 = unsafe {
                                <i32>::from_ne_bytes(
                                    ws.arr[12..16].try_into().unwrap_unchecked(),
                                )
                            };
                            let src1 = unsafe {
                                let src = (flags >> 8 as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let src2 = unsafe {
                                let src = (flags as u8) >> 4;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            let target = unsafe {
                                let src = (flags as u8) & 0x0F;
                                match src {
                                    0 => &raw mut taskstate.r1,
                                    1 => &raw mut taskstate.r2,
                                    2 => &raw mut taskstate.r3,
                                    3 => &raw mut taskstate.r4,
                                    4 => &raw mut taskstate.r5,
                                    5 => &raw mut taskstate.r6,
                                    6 => &raw mut taskstate.r7,
                                    7 => &raw mut taskstate.r8,
                                    8 => taskstate.scratchpad,
                                    9 => taskstate.largepad,
                                    10 => unsafe { taskstate.r2.selfref }
                                    _ => ::core::panicking::panic("not implemented"),
                                }
                            };
                            (
                                inst,
                                fptype,
                                count,
                                src1,
                                src2,
                                target,
                                offset1,
                                offset2,
                                offset3,
                            )
                        };
                        unsafe {
                            match (fptype) {
                                0 => {
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
                                1 => {
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
                mod vfop {
                    use crate::{
                        acaot::pickle::{
                            def::PickleInstruction, implementation::WorkingSet,
                        },
                        arrcastint, resolve, resolve_location_src,
                    };
                    use sart::{ctr::VMTaskState, structures::QuadPackedData};
                    use std::ptr::{self, addr_of_mut};
                    pub fn call_vfop(
                        pickle: &PickleInstruction,
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
                    ) {
                        let flags = u16::from_le_bytes([pickle.u1, pickle.u2]);
                        let count_data = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[0..4].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset1 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[4..8].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset2 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let subop = (flags as u8) & 0x7;
                        let countbit = ((flags as u8) >> 3) & 0x1;
                        let count = if (countbit == 0) {
                            count_data
                        } else {
                            unsafe { taskstate.r1.u32 }
                        };
                        let target1 = (flags as u8) >> 4;
                        let src1 = (flags >> 8) as u8 & 0xF;
                        let float_type = ((flags >> 12) as u8) & 0x1;
                        match float_type {
                            0 => {
                                match subop {
                                    0 => {
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
                                    1 => {
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
                                    2 => {
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
                                    3 => {
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
                                    _ => ::core::panicking::panic("explicit panic"),
                                }
                            }
                            1 => {
                                match subop {
                                    0 => {
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
                                    1 => {
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
                                    2 => {
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
                                    3 => {
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
                                    _ => ::core::panicking::panic("explicit panic"),
                                }
                            }
                            _ => ::core::panicking::panic("explicit panic"),
                        }
                    }
                    pub fn call_vfcast(
                        pickle: &PickleInstruction,
                        ws: &mut WorkingSet,
                        taskstate: &mut VMTaskState,
                    ) {
                        let flags = u16::from_le_bytes([pickle.u1, pickle.u2]);
                        let target1 = {
                            let tg = (flags as u8) & 0x0F;
                            match tg {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let src1 = {
                            let s1 = ((flags >> 4) as u8) & 0x0F;
                            match s1 {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let typetag = (flags >> 8) as u8 & 0x07;
                        let fwidth = (flags >> 11) as u8 & 0x01;
                        let op = (flags >> 12) as u8 & 0x01;
                        let countbit = (flags >> 13) as u8 & 0x01;
                        let count_data = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[0..4].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset1 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[4..8].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset2 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let count = if (countbit == 0) {
                            count_data
                        } else {
                            unsafe { taskstate.r1.u32 }
                        };
                        let f = match fwidth {
                            0 => {
                                match op {
                                    0 => {
                                        match typetag {
                                            0 => as_cast::<f64, u64>,
                                            1 => as_cast::<f64, u32>,
                                            2 => as_cast::<f64, u16>,
                                            3 => as_cast::<f64, u8>,
                                            4 => as_cast::<f64, i64>,
                                            5 => as_cast::<f64, i32>,
                                            6 => as_cast::<f64, i16>,
                                            7 => as_cast::<f64, i8>,
                                            _ => ::core::panicking::panic("explicit panic"),
                                        }
                                    }
                                    1 => {
                                        match typetag {
                                            0 => as_cast::<u64, f64>,
                                            1 => as_cast::<u32, f64>,
                                            2 => as_cast::<u16, f64>,
                                            3 => as_cast::<u8, f64>,
                                            4 => as_cast::<i64, f64>,
                                            5 => as_cast::<i32, f64>,
                                            6 => as_cast::<i16, f64>,
                                            7 => as_cast::<i8, f64>,
                                            _ => ::core::panicking::panic("explicit panic"),
                                        }
                                    }
                                    _ => ::core::panicking::panic("explicit panic"),
                                }
                            }
                            1 => {
                                match op {
                                    0 => {
                                        match typetag {
                                            0 => as_cast::<f32, u64>,
                                            1 => as_cast::<f32, u32>,
                                            2 => as_cast::<f32, u16>,
                                            3 => as_cast::<f32, u8>,
                                            4 => as_cast::<f32, i64>,
                                            5 => as_cast::<f32, i32>,
                                            6 => as_cast::<f32, i16>,
                                            7 => as_cast::<f32, i8>,
                                            _ => ::core::panicking::panic("explicit panic"),
                                        }
                                    }
                                    1 => {
                                        match typetag {
                                            0 => as_cast::<u64, f32>,
                                            1 => as_cast::<u32, f32>,
                                            2 => as_cast::<u16, f32>,
                                            3 => as_cast::<u8, f32>,
                                            4 => as_cast::<i64, f32>,
                                            5 => as_cast::<i32, f32>,
                                            6 => as_cast::<i16, f32>,
                                            7 => as_cast::<i8, f32>,
                                            _ => ::core::panicking::panic("explicit panic"),
                                        }
                                    }
                                    _ => ::core::panicking::panic("explicit panic"),
                                }
                            }
                            _ => ::core::panicking::panic("explicit panic"),
                        };
                        f(src1, target1, offset1, offset2, count);
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
                pub fn call_vadd(
                    pickle: &PickleInstruction,
                    ws: &mut WorkingSet,
                    taskstate: &mut VMTaskState,
                ) {
                    let (instdefined, typetag, count, src1, src2, target, t1, t2, t3) = {
                        let flags = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[0..4].try_into().unwrap_unchecked(),
                            )
                        };
                        let instdefined = flags as u16;
                        let topflags = (flags >> 16) as u16;
                        let countbit = (flags >> 12 as u8) & 0x01;
                        let typetag = flags >> 13 as u8;
                        let count_data = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[4..8].try_into().unwrap_unchecked(),
                            )
                        };
                        let count = if (countbit == 0) {
                            count_data
                        } else {
                            unsafe { taskstate.r1.u32 }
                        };
                        let offset1 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset2 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[12..16].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset3 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[16..20].try_into().unwrap_unchecked(),
                            )
                        };
                        let src1 = unsafe {
                            let src = (flags >> 8 as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let src2 = unsafe {
                            let src = (flags as u8) >> 4;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target = unsafe {
                            let src = (flags as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        (
                            instdefined,
                            typetag,
                            count,
                            src1,
                            src2,
                            target,
                            offset1,
                            offset2,
                            offset3,
                        )
                    };
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
                        if !!((carry || saturate) && count != 1) {
                            ::core::panicking::panic(
                                "assertion failed: !((carry || saturate) && count != 1)",
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
                                                &raw mut taskstate.r5 as *mut u64,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u64),
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
                                                &raw mut taskstate.r5 as *mut u32,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u32),
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
                                                &raw mut taskstate.r5 as *mut u16,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u16),
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
                                                &raw mut taskstate.r5 as *mut u8,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u8),
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
                                                &raw mut taskstate.r5 as *mut i64,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i64),
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
                                                &raw mut taskstate.r5 as *mut i32,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i32),
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
                                                &raw mut taskstate.r5 as *mut i16,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i16),
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
                                                &raw mut taskstate.r5 as *mut i8,
                                            ) != 0;
                                            let output = (s1).carrying_add(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i8),
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
                            _ => {
                                ::core::panicking::panic(
                                    "internal error: entered unreachable code",
                                )
                            }
                        }
                    }
                }
                pub fn call_vsub(
                    pickle: &PickleInstruction,
                    ws: &mut WorkingSet,
                    taskstate: &mut VMTaskState,
                ) {
                    let (instdefined, typetag, count, src1, src2, target, t1, t2, t3) = {
                        let flags = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[0..4].try_into().unwrap_unchecked(),
                            )
                        };
                        let instdefined = flags as u16;
                        let topflags = (flags >> 16) as u16;
                        let countbit = (flags >> 12 as u8) & 0x01;
                        let typetag = flags >> 13 as u8;
                        let count_data = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[4..8].try_into().unwrap_unchecked(),
                            )
                        };
                        let count = if (countbit == 0) {
                            count_data
                        } else {
                            unsafe { taskstate.r1.u32 }
                        };
                        let offset1 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset2 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[12..16].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset3 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[16..20].try_into().unwrap_unchecked(),
                            )
                        };
                        let src1 = unsafe {
                            let src = (flags >> 8 as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let src2 = unsafe {
                            let src = (flags as u8) >> 4;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target = unsafe {
                            let src = (flags as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        (
                            instdefined,
                            typetag,
                            count,
                            src1,
                            src2,
                            target,
                            offset1,
                            offset2,
                            offset3,
                        )
                    };
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
                        if !!((carry || saturate) && count != 1) {
                            ::core::panicking::panic(
                                "assertion failed: !((carry || saturate) && count != 1)",
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
                                                &raw mut taskstate.r5 as *mut u64,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u64),
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
                                                &raw mut taskstate.r5 as *mut u32,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u32),
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
                                                &raw mut taskstate.r5 as *mut u16,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u16),
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
                                                &raw mut taskstate.r5 as *mut u8,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut u8),
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
                                                &raw mut taskstate.r5 as *mut i64,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i64),
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
                                                &raw mut taskstate.r5 as *mut i32,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i32),
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
                                                &raw mut taskstate.r5 as *mut i16,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i16),
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
                                                &raw mut taskstate.r5 as *mut i8,
                                            ) != 0;
                                            let output = (s1).borrowing_sub(s2, carry);
                                            ptr::write_unaligned(t, output.0);
                                            ptr::write_unaligned(
                                                (&raw mut taskstate.r5 as *mut i8),
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
                            _ => {
                                ::core::panicking::panic(
                                    "internal error: entered unreachable code",
                                )
                            }
                        }
                    }
                }
                pub fn call_vmul(
                    pickle: &PickleInstruction,
                    ws: &mut WorkingSet,
                    taskstate: &mut VMTaskState,
                ) {
                    let (instdefined, typetag, count, src1, src2, target, t1, t2, t3) = {
                        let flags = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[0..4].try_into().unwrap_unchecked(),
                            )
                        };
                        let instdefined = flags as u16;
                        let topflags = (flags >> 16) as u16;
                        let countbit = (flags >> 12 as u8) & 0x01;
                        let typetag = flags >> 13 as u8;
                        let count_data = unsafe {
                            <u32>::from_ne_bytes(
                                ws.arr[4..8].try_into().unwrap_unchecked(),
                            )
                        };
                        let count = if (countbit == 0) {
                            count_data
                        } else {
                            unsafe { taskstate.r1.u32 }
                        };
                        let offset1 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset2 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[12..16].try_into().unwrap_unchecked(),
                            )
                        };
                        let offset3 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[16..20].try_into().unwrap_unchecked(),
                            )
                        };
                        let src1 = unsafe {
                            let src = (flags >> 8 as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let src2 = unsafe {
                            let src = (flags as u8) >> 4;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target = unsafe {
                            let src = (flags as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        (
                            instdefined,
                            typetag,
                            count,
                            src1,
                            src2,
                            target,
                            offset1,
                            offset2,
                            offset3,
                        )
                    };
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (a, b) = (s1).widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
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
                                                let (_, b) = s1.widening_mul(s2);
                                                ptr::write_unaligned(t, b);
                                            }
                                        }
                                    }
                                    _ => ::core::panicking::panic("explicit panic"),
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
                pub fn call_div(
                    pickle: &PickleInstruction,
                    ws: &mut WorkingSet,
                    taskstate: &mut VMTaskState,
                ) {
                    let (typetag, src1, src2, target, t1, t2, t3) = {
                        let args = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                        let typetag = (args >> 12) as u8;
                        let t1 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let t2 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[12..16].try_into().unwrap_unchecked(),
                            )
                        };
                        let t3 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[16..20].try_into().unwrap_unchecked(),
                            )
                        };
                        let src1 = unsafe {
                            let src = (args >> 8 as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let src2 = unsafe {
                            let src = (args as u8) >> 4;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target = unsafe {
                            let src = (args as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        (typetag, src1, src2, target, t1, t2, t3)
                    };
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
                pub fn call_rem(
                    pickle: &PickleInstruction,
                    ws: &mut WorkingSet,
                    taskstate: &mut VMTaskState,
                ) {
                    let (typetag, src1, src2, target, t1, t2, t3) = {
                        let args = u16::from_ne_bytes([pickle.u1, pickle.u2]);
                        let typetag = (args >> 12) as u8;
                        let t1 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[8..12].try_into().unwrap_unchecked(),
                            )
                        };
                        let t2 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[12..16].try_into().unwrap_unchecked(),
                            )
                        };
                        let t3 = unsafe {
                            <i32>::from_ne_bytes(
                                ws.arr[16..20].try_into().unwrap_unchecked(),
                            )
                        };
                        let src1 = unsafe {
                            let src = (args >> 8 as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let src2 = unsafe {
                            let src = (args as u8) >> 4;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        let target = unsafe {
                            let src = (args as u8) & 0x0F;
                            match src {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                8 => taskstate.scratchpad,
                                9 => taskstate.largepad,
                                10 => unsafe { taskstate.r2.selfref }
                                _ => ::core::panicking::panic("not implemented"),
                            }
                        };
                        (typetag, src1, src2, target, t1, t2, t3)
                    };
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
            pub use au::*;
            use sart::{ctr::VMTaskState, structures::QuadPackedData};
            use crate::acaot::pickle::def::PickleInstruction;
            pub const SIZE_128KB: usize = 128 * 1024 / size_of::<QuadPackedData>();
            pub struct WorkingSet {
                pub arr: [u8; 20],
                pub largepad: *mut QuadPackedData,
                pub largepad_cursor: usize,
                pub relocmap: HashMap<u64, usize, ahash::RandomState>,
            }
            impl WorkingSet {
                pub fn allocate(
                    &mut self,
                    size: u64,
                    align: u64,
                ) -> *mut QuadPackedData {
                    if align != 0 {
                        return unsafe {
                            sart::salloc::aligned_malloc(
                                (size as usize) * size_of::<QuadPackedData>(),
                                align as usize,
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
                ws: &mut WorkingSet,
                taskstate: &mut VMTaskState,
            ) -> ();
            pub fn call_hint(
                _pickle: &PickleInstruction,
                _ws: &mut WorkingSet,
                _taskstate: &mut VMTaskState,
            ) {}
            pub fn call_mark(
                _pickle: &PickleInstruction,
                _ws: &mut WorkingSet,
                _taskstate: &mut VMTaskState,
            ) {}
            pub fn call_ws_put(
                pickle: &PickleInstruction,
                ws: &mut WorkingSet,
                _taskstate: &mut VMTaskState,
            ) {
                let offset = pickle.u1 as usize;
                ws.arr[offset * 2] = pickle.u2;
                ws.arr[offset * 2 + 1] = pickle.u3;
            }
            pub fn call_mov(
                pickle: &PickleInstruction,
                _ws: &mut WorkingSet,
                taskstate: &mut VMTaskState,
            ) {
                let source = pickle.u1;
                let target = pickle.u2;
                if source == target {
                    cold_path();
                    match source {
                        12 => {
                            taskstate.r1.selfref = taskstate.scratchpad;
                        }
                        13 => {
                            taskstate.r1.selfref = taskstate.largepad;
                        }
                        _ => {
                            match target {
                                0 => taskstate.r1,
                                1 => taskstate.r2,
                                2 => taskstate.r3,
                                3 => taskstate.r4,
                                4 => taskstate.r5,
                                5 => taskstate.r6,
                                6 => taskstate.r7,
                                7 => taskstate.r8,
                                _ => ::core::panicking::panic("not implemented"),
                            }
                                .selfref = match target {
                                0 => &raw mut taskstate.r1,
                                1 => &raw mut taskstate.r2,
                                2 => &raw mut taskstate.r3,
                                3 => &raw mut taskstate.r4,
                                4 => &raw mut taskstate.r5,
                                5 => &raw mut taskstate.r6,
                                6 => &raw mut taskstate.r7,
                                7 => &raw mut taskstate.r8,
                                _ => ::core::panicking::panic("not implemented"),
                            };
                        }
                    }
                } else {
                    let rsrc = match source {
                        0 => taskstate.r1,
                        1 => taskstate.r2,
                        2 => taskstate.r3,
                        3 => taskstate.r4,
                        4 => taskstate.r5,
                        5 => taskstate.r6,
                        6 => taskstate.r7,
                        7 => taskstate.r8,
                        _ => ::core::panicking::panic("not implemented"),
                    };
                    let ptarget = &mut match target {
                        0 => taskstate.r1,
                        1 => taskstate.r2,
                        2 => taskstate.r3,
                        3 => taskstate.r4,
                        4 => taskstate.r5,
                        5 => taskstate.r6,
                        6 => taskstate.r7,
                        7 => taskstate.r8,
                        _ => ::core::panicking::panic("not implemented"),
                    };
                    *ptarget = rsrc;
                }
            }
            pub fn call_reg(
                pickle: &PickleInstruction,
                ws: &mut WorkingSet,
                taskstate: &mut VMTaskState,
            ) {
                let reg = pickle.u1;
                let mut filled = [0u8; 8];
                filled[0..6].copy_from_slice(&ws.arr[0..6]);
                filled[6..8].copy_from_slice(&[pickle.u2, pickle.u3]);
                let data = u64::from_ne_bytes(filled);
                *(&mut match reg {
                    0 => taskstate.r1,
                    1 => taskstate.r2,
                    2 => taskstate.r3,
                    3 => taskstate.r4,
                    4 => taskstate.r5,
                    5 => taskstate.r6,
                    6 => taskstate.r7,
                    7 => taskstate.r8,
                    _ => ::core::panicking::panic("not implemented"),
                }) = QuadPackedData { u64: data };
            }
            pub fn call_jmp(
                pickle: &PickleInstruction,
                ws: &mut WorkingSet,
                taskstate: &mut VMTaskState,
            ) {
                let mut filled = [0u8; 8];
                filled[0..6].copy_from_slice(&ws.arr[0..6]);
                filled[6..8].copy_from_slice(&[pickle.u1, pickle.u2]);
                let data = u64::from_ne_bytes(filled);
                unsafe {
                    taskstate.curline_or_resume.unsigned = *ws
                        .relocmap
                        .get(&data)
                        .unwrap_unchecked() as _;
                }
            }
            pub fn call_jif(
                pickle: &PickleInstruction,
                ws: &mut WorkingSet,
                taskstate: &mut VMTaskState,
            ) {
                let intent = pickle.u1;
                let relocation_src = pickle.u2;
                let width = pickle.u3;
                let offset = i32::from_ne_bytes(unsafe {
                    ws.arr[0..4].try_into().unwrap_unchecked()
                });
                let marker = u64::from_ne_bytes(unsafe {
                    ws.arr[4..12].try_into().unwrap_unchecked()
                });
                let not_zero = unsafe {
                    let src = match relocation_src {
                        0 => &raw mut taskstate.r1,
                        1 => &raw mut taskstate.r2,
                        2 => &raw mut taskstate.r3,
                        3 => &raw mut taskstate.r4,
                        4 => &raw mut taskstate.r5,
                        5 => &raw mut taskstate.r6,
                        6 => &raw mut taskstate.r7,
                        7 => &raw mut taskstate.r8,
                        8 => taskstate.scratchpad,
                        9 => taskstate.largepad,
                        10 => unsafe { taskstate.r2.selfref }
                        _ => ::core::panicking::panic("not implemented"),
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
                        taskstate.curline_or_resume.unsigned = *ws
                            .relocmap
                            .get(&marker)
                            .unwrap_unchecked() as _;
                    }
                }
            }
            pub fn call_vcmp(
                pickle: &PickleInstruction,
                ws: &mut WorkingSet,
                taskstate: &mut VMTaskState,
            ) {
                let count_bit = pickle.u1;
                let op_width = pickle.u2;
                let op = op_width & 0x1F;
                let width = op_width >> 5;
                let srcflags = unsafe {
                    <u16>::from_ne_bytes(ws.arr[0..2].try_into().unwrap_unchecked())
                };
                let src1 = (srcflags >> 6) as u8;
                let src2 = ((srcflags >> 4) & 0x3) as u8;
                let target = ((srcflags >> 2) & 0x3) as u8;
                let count = if count_bit == 0 {
                    unsafe {
                        <u32>::from_ne_bytes(ws.arr[2..6].try_into().unwrap_unchecked())
                    }
                } else {
                    unsafe { taskstate.r1.u32 }
                };
                let offset1 = unsafe {
                    <i32>::from_ne_bytes(ws.arr[6..10].try_into().unwrap_unchecked())
                };
                let offset2 = unsafe {
                    <i32>::from_ne_bytes(ws.arr[10..14].try_into().unwrap_unchecked())
                };
                let offset3 = unsafe {
                    <i32>::from_ne_bytes(ws.arr[14..18].try_into().unwrap_unchecked())
                };
                let src1 = unsafe {
                    match src1 {
                        0 => &raw mut taskstate.r1,
                        1 => &raw mut taskstate.r2,
                        2 => &raw mut taskstate.r3,
                        3 => &raw mut taskstate.r4,
                        4 => &raw mut taskstate.r5,
                        5 => &raw mut taskstate.r6,
                        6 => &raw mut taskstate.r7,
                        7 => &raw mut taskstate.r8,
                        8 => taskstate.scratchpad,
                        9 => taskstate.largepad,
                        10 => unsafe { taskstate.r2.selfref }
                        _ => ::core::panicking::panic("not implemented"),
                    }
                };
                let src2 = unsafe {
                    match src2 {
                        0 => &raw mut taskstate.r1,
                        1 => &raw mut taskstate.r2,
                        2 => &raw mut taskstate.r3,
                        3 => &raw mut taskstate.r4,
                        4 => &raw mut taskstate.r5,
                        5 => &raw mut taskstate.r6,
                        6 => &raw mut taskstate.r7,
                        7 => &raw mut taskstate.r8,
                        8 => taskstate.scratchpad,
                        9 => taskstate.largepad,
                        10 => unsafe { taskstate.r2.selfref }
                        _ => ::core::panicking::panic("not implemented"),
                    }
                };
                let target = unsafe {
                    match target {
                        0 => &raw mut taskstate.r1,
                        1 => &raw mut taskstate.r2,
                        2 => &raw mut taskstate.r3,
                        3 => &raw mut taskstate.r4,
                        4 => &raw mut taskstate.r5,
                        5 => &raw mut taskstate.r6,
                        6 => &raw mut taskstate.r7,
                        7 => &raw mut taskstate.r8,
                        8 => taskstate.scratchpad,
                        9 => taskstate.largepad,
                        10 => unsafe { taskstate.r2.selfref }
                        _ => ::core::panicking::panic("not implemented"),
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
                ) = if op >= 0 && op <= 9 {
                    let is_signed = [2, 4, 6, 8].iter().any(|o| op == *o);
                    match (is_signed, width) {
                        (true, 0) => vcmp_inner::<i8>,
                        (true, 1) => vcmp_inner::<i16>,
                        (true, 2) => vcmp_inner::<i32>,
                        (true, 3) => vcmp_inner::<i64>,
                        (false, 0) => vcmp_inner::<u8>,
                        (false, 1) => vcmp_inner::<u16>,
                        (false, 2) => vcmp_inner::<u32>,
                        (false, 3) => vcmp_inner::<u64>,
                        _ => ::core::panicking::panic("explicit panic"),
                    }
                } else {
                    match width {
                        2 => vcmp_f_inner::<f32, i32>,
                        3 => vcmp_f_inner::<f64, i64>,
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
            pub(crate) bytecode: T,
            pub out: Vec<PickleInstruction>,
            pub jump: HashMap<u64, usize, ahash::RandomState>,
        }
        trait Extract: Read + Sized {
            fn extract<const N: usize>(&mut self) -> [u8; N] {
                self.read_array::<N>().unwrap()
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
                while let Ok([opcode]) = self.bytecode.read_array::<1>() {
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
                        _ => {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                }
            }
            fn handle_atomic(&mut self) {
                let opcode = PICKLE_OPCODE_ATOMIC;
                let flagu32 = self.bytecode.extract::<2>().swap_if_be();
                let [instdef1, instdef2] = self.bytecode.extract::<2>().swap_if_be();
                self.emit_copy_bytes(opcode, flagu32);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: instdef1,
                        u2: instdef2,
                        u3: 0,
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
                let [flags] = self.bytecode.extract::<1>();
                let sectionid = self.bytecode.extract::<8>().swap_if_be();
                let scratchpad_begin = self.bytecode.extract::<4>().swap_if_be();
                let scratchpad_end = self.bytecode.extract::<4>().swap_if_be();
                let mut copy = [0u8; 14];
                copy[0..4].copy_from_slice(&scratchpad_begin);
                copy[4..8].copy_from_slice(&scratchpad_end);
                copy[8..14].copy_from_slice(&sectionid[0..6]);
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: sectionid[6],
                        u2: sectionid[7],
                        u3: flags,
                    });
            }
            fn handle_asynccall(&mut self) {
                let opcode = PICKLE_OPCODE_ASYNCCALL;
                let sectionid = self.bytecode.extract::<8>().swap_if_be();
                let marker = self.bytecode.extract::<8>().swap_if_be();
                let mut copy = [0u8; 14];
                copy[0..8].copy_from_slice(&sectionid);
                copy[8..14].copy_from_slice(&marker[0..6]);
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: marker[6],
                        u2: marker[7],
                        u3: 0,
                    });
            }
            fn handle_synccall(&mut self) {
                let opcode = PICKLE_OPCODE_SYNCCALL;
                let sectionid = self.bytecode.extract::<8>().swap_if_be();
                let mut copy = [0u8; 6];
                copy[0..6].copy_from_slice(&sectionid[0..6]);
                self.emit_copy_bytes(opcode, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: opcode,
                        u1: sectionid[6],
                        u2: sectionid[7],
                        u3: 0,
                    });
            }
            fn handle_vfma(&mut self) {
                let opcode = PICKLE_OPCODE_VFMA;
                let [flags1, flags2, flags3] = self.bytecode.extract::<3>().swap_if_be();
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
                        u3: flags3,
                    });
            }
            fn handle_vminimax(&mut self) {
                let opcode = PICKLE_OPCODE_VMINIMAX;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let [maxbit] = self.bytecode.extract::<1>();
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
                        u3: maxbit,
                    });
            }
            fn handle_vcnt(&mut self) {
                let opcode = PICKLE_OPCODE_VCNT;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
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
                        u3: 0,
                    });
            }
            fn handle_vsh(&mut self) {
                let opcode = PICKLE_OPCODE_VSH;
                let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
                let [countbit] = self.bytecode.extract::<1>();
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
                        u3: countbit,
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
                let [dt, src_flags] = self.bytecode.extract::<2>();
                let count = dt >> 7;
                let flags = dt & 0x7F;
                let mut copy = [0u8; 12];
                copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
                self.emit_copy_bytes(PICKLE_OPCODE_VCOPY, copy);
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_VCOPY,
                        u1: count,
                        u2: flags,
                        u3: src_flags,
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
                let count = r0 >> 7;
                let operation = r0 & 0x7F;
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
                        u1: count,
                        u2: operation,
                        u3: 0,
                    });
            }
            fn handle_jif(&mut self) {
                let [cond] = self.bytecode.read_array::<1>().unwrap();
                let offset = i32::from_le_bytes(self.bytecode.read_array::<4>().unwrap())
                    .to_ne_bytes();
                let marker = u64::from_le_bytes(self.bytecode.read_array::<8>().unwrap())
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
                let data = u64::from_le_bytes(self.bytecode.read_array::<8>().unwrap())
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
                    self.bytecode.read_array::<8>().unwrap(),
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
                self.jump.insert(marker, self.out.len());
            }
            fn handle_reg(&mut self) {
                let [register] = self.bytecode.read_array().expect("");
                let data_ne: [u8; 8] = u64::from_le_bytes(
                        self.bytecode.read_array::<8>().expect(""),
                    )
                    .to_ne_bytes();
                self.emit_copy_bytes::<
                        6,
                    >(PICKLE_OPCODE_REG, data_ne[0..6].try_into().unwrap());
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_REG,
                        u1: register,
                        u2: data_ne[6],
                        u3: data_ne[7],
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
                self.out
                    .push(PickleInstruction {
                        opcode: PICKLE_OPCODE_HINT,
                        u1: opcode,
                        u2: (N / 2) as u8,
                        u3: 0,
                    });
                for i in 0..N / 2 {
                    self.out
                        .push(PickleInstruction {
                            opcode: PICKLE_OPCODE_WS_PUT,
                            u1: i as u8,
                            u2: data[i * 2],
                            u3: data[i * 2 + 1],
                        });
                }
            }
            fn handle_mov(&mut self) {
                let [registers] = self.bytecode.read_array().expect("");
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
}
use std::{
    fs::File, hash::Hash, io::{Read, Seek},
    mem::zeroed, os::raw::c_void,
    sync::{Arc, LazyLock, OnceLock, atomic::Ordering, nonpoison::RwLock},
    thread::{self, available_parallelism},
    time::Duration,
};
use evmap::{StableHashEq, handles::ReadHandle};
use moka::sync::{CacheBuilder, SegmentedCache};
use sart::{code::SwappableCodeStore, ctr::{CVMTaskState, Instruction}};
pub use sart;
use tokio::runtime::{Builder, Runtime};
use crate::{acaot::pickle::def::PickleInstruction, management::management_main};
pub mod executor {}
pub(crate) mod management {
    use crate::{
        BytecodeResolver, CODE_CACHE, CacheData, SymbolMapTable,
        acaot::pickle::PickleWorker,
    };
    use evmap::handles::WriteHandle;
    use std::sync::Arc;
    use rayon::iter::{IntoParallelIterator, ParallelIterator};
    pub fn management_main<T: BytecodeResolver + Send + Sync + 'static>(
        writer: WriteHandle<u64, usize>,
        resolve: Arc<T>,
    ) {
        (0..=resolve.as_ref().last_section_id())
            .into_par_iter()
            .map(|id| match resolve.as_ref().resolve_data(id) {
                SymbolMapTable::MixedSizedBytecode { bytecode } => {
                    match resolve.as_ref().get_best_cache(id) {
                        CacheData::None => {
                            let mut worker = PickleWorker {
                                bytecode,
                                out: ::alloc::vec::Vec::new(),
                                jump: Default::default(),
                            };
                            worker.pass1();
                            let out = Arc::new(worker.out.into_boxed_slice());
                            CODE_CACHE.insert(id, out.clone());
                            Some((id, out))
                        }
                        _ => None,
                    }
                }
                SymbolMapTable::NativePointer { .. } => None,
            })
            .filter_map(|x| x)
            .collect::<Box<[_]>>()
            .into_iter()
            .for_each(|(section, cache)| {
                resolve.as_ref().update_cache(section, CacheData::Pickle { out: cache });
            });
    }
}
pub mod sync {
    use std::{cell::UnsafeCell, collections::HashMap, hint::cold_path, mem::zeroed};
    use evmap::refs::{ReadGuard, Values};
    use sart::{ctr::VMTaskState, salloc, structures::QuadPackedData};
    use crate::{
        BytecodeResolver, CODE_CACHE, JIT_CACHE, VM,
        acaot::pickle::{
            def::{PICKLE_DISPATCH_TABLE, PICKLE_OPCODE_HINT, PICKLE_OPCODE_MARK},
            implementation::{SIZE_128KB, WorkingSet},
        },
    };
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
            }
        }
    }
    pub const VMSTAT: ::std::thread::LocalKey<UnsafeCell<VMState>> = {
        #[inline]
        fn __rust_std_internal_init_fn() -> UnsafeCell<VMState> {
            UnsafeCell::new(VMState {
                ws: WorkingSet {
                    arr: [0u8; 20],
                    largepad: unsafe { salloc::aligned_malloc(SIZE_128KB, 8) as _ },
                    largepad_cursor: 0,
                    relocmap: HashMap::default(),
                },
                ts: unsafe {
                    let mut ts: [VMTaskState; 50] = zeroed();
                    let alloca = salloc::aligned_malloc(SCRATCHPAD, 64)
                        as *mut QuadPackedData;
                    for (i, t) in ts.iter_mut().enumerate() {
                        t.scratchpad = alloca.add(i * 24 * size_of::<QuadPackedData>());
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
    impl<T: BytecodeResolver + Send + Sync + 'static> VM<T> {
        pub fn call_section(&self, sectionid: u64) {
            let Some(data) = CODE_CACHE.get(&sectionid) else {
                return self.pickle_section(sectionid);
            };
            let leng = data.len();
            let dt = data.as_ref();
            let mut run_jit = false;
            VMSTAT
                .with(|x| unsafe {
                    let t = &mut *x.get();
                    let ts = t.ts.get_unchecked_mut(t.cindex);
                    ts.engine_or_pt.pt = self as *const _ as _;
                    ts.curline_or_resume.usi = 0;
                    'jcheck: loop {
                        if let Some(d) = unsafe { &JIT_CACHE.get().unwrap_unchecked().0 }
                            .get(&sectionid)
                        {
                            drop(d);
                            run_jit = true;
                            break 'jcheck;
                        }
                        loop {
                            if ts.curline_or_resume.usi == leng {
                                break 'jcheck;
                            }
                            let pickle = dt.get_unchecked(ts.curline_or_resume.usi);
                            if pickle.opcode == PICKLE_OPCODE_HINT
                                && pickle.u1 == PICKLE_OPCODE_MARK
                            {
                                ts.curline_or_resume.usi += 1;
                                continue 'jcheck;
                            }
                            (PICKLE_DISPATCH_TABLE
                                .get_unchecked(
                                    pickle.opcode as usize,
                                ))(pickle, &mut t.ws, ts);
                            ts.curline_or_resume.usi += 1;
                        }
                    }
                });
            if run_jit {
                return self.dispatch_jit(sectionid);
            }
            cold_path();
        }
        pub(crate) fn dispatch_jit(&self, sectionid: u64) {}
        fn pickle_section(&self, sectionid: u64) {
            return self.call_section(sectionid);
        }
    }
}
pub static TOTAL_THREADS: LazyLock<usize> = LazyLock::new(|| {
    available_parallelism().unwrap().into()
});
static VMMADE: OnceLock<()> = OnceLock::new();
pub enum SymbolMapTable<T> {
    NativePointer { fnptr: extern "C" fn(vm: *mut CVMTaskState) },
    MixedSizedBytecode { bytecode: T },
}
pub enum CacheData {
    None,
    Pickle { out: Arc<Box<[PickleInstruction]>> },
    CraneliftAbs8 {},
    CraneliftRel {},
}
pub enum CacheLevel {
    Pickle,
    CraneliftAbs8,
    CraneliftRel,
}
pub trait BytecodeResolver {
    type Output: Read + Seek;
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
    fn resolve_data(&self, section: u64) -> SymbolMapTable<Self::Output>;
    /// Checks if the cache is available!
    fn get_best_cache(&self, section: u64) -> CacheData;
    /// Checks if the cache is available!
    fn get_cache(&self, section: u64, level: CacheLevel) -> CacheData;
    /// Updates the cache
    ///
    /// We hope the callee only updates the tier of cache this produces
    ///
    /// eg. we hope it does not replace Pickle code with Cranelift code as that'll lead to performance losses next round
    fn update_cache(&self, section: u64, cache: CacheData);
}
impl BytecodeResolver
for Box<dyn BytecodeResolver<Output = File> + Send + Sync + 'static> {
    type Output = File;
    fn get_best_cache(&self, section: u64) -> CacheData {
        BytecodeResolver::get_best_cache(self.as_ref(), section)
    }
    fn heuristic_pgo(&self) -> [&[u64]; 2] {
        BytecodeResolver::heuristic_pgo(self.as_ref())
    }
    fn resolve_data(&self, section: u64) -> SymbolMapTable<Self::Output> {
        BytecodeResolver::resolve_data(self.as_ref(), section)
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
pub static VMCONF: RwLock<VmConfig> = RwLock::new(unsafe { zeroed() });
pub(crate) static CODE_CACHE: LazyLock<
    SegmentedCache<u64, Arc<Box<[PickleInstruction]>>, ahash::RandomState>,
> = LazyLock::new(|| {
    CacheBuilder::new(1 << 10)
        .segments(available_parallelism().map(|x| x.get()).unwrap_or(4))
        .time_to_live(Duration::from_mins(20))
        .time_to_idle(Duration::from_mins(5))
        .build_with_hasher(ahash::RandomState::default())
});
pub type JITStorage = *mut SwappableCodeStore<()>;
pub(crate) static JIT_CACHE: OnceLock<ThreadSafe<ReadHandle<u64, usize>>> = OnceLock::new();
pub(crate) struct ThreadSafe<T>(pub T);
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
}
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
        CODE_CACHE.run_pending_tasks();
        VMMADE.set(()).expect("Each process can only have 1 VM");
        let resolver = Arc::new(data);
        {
            let resolve = resolver.clone();
            let (writer, reader) = evmap::new::<u64, usize>();
            JIT_CACHE.set(ThreadSafe(reader)).expect("impossible");
            thread::spawn(move || management_main(writer, resolve));
        }
        Self { resolve: resolver }
    }
}
pub enum MaybeBoxed<T> {
    Boxed(Box<T>),
    Unboxed(T),
}
