#![feature(prelude_import)]
extern crate std;
#[prelude_import]
use std::prelude::rust_2024::*;
use std::{
    cell::UnsafeCell, collections::HashMap, hash::Hash, marker::PhantomData,
    num::NonZeroUsize, rc::Rc, sync::{Arc, atomic::{AtomicUsize, Ordering}},
};
use dashmap::DashMap;
pub mod llir {
    pub mod instr {
        use const_str::convert_ascii_case;
        use loc::LocSrc;
        use sart::ctr::*;
        use std::fmt::Formatter;
        use crate::{
            llir::instr::flags::{Count, VCopyMemFlags},
            mir::block::instr::{AHQF, Register},
        };
        #[macro_use]
        mod macros {}
        pub mod flags {
            use crate::mir::block::instr::AHQF;
            pub enum Count {
                Abs { abs: u32 },
                ReadFromR1,
            }
            impl AHQF for Count {
                fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        &Self::Abs { abs } => f.write_fmt(format_args!("{0}", abs)),
                        Self::ReadFromR1 => f.write_fmt(format_args!("@count:r1")),
                    }
                }
            }
            pub struct VCopyMemFlags {
                pub volatile: bool,
                pub nonoverlapping: bool,
                pub srcalign: AlignData,
                pub tgtalign: AlignData,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for VCopyMemFlags {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_struct_field4_finish(
                        f,
                        "VCopyMemFlags",
                        "volatile",
                        &self.volatile,
                        "nonoverlapping",
                        &self.nonoverlapping,
                        "srcalign",
                        &self.srcalign,
                        "tgtalign",
                        &&self.tgtalign,
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for VCopyMemFlags {}
            #[automatically_derived]
            impl ::core::clone::Clone for VCopyMemFlags {
                #[inline]
                fn clone(&self) -> VCopyMemFlags {
                    let _: ::core::clone::AssertParamIsClone<bool>;
                    let _: ::core::clone::AssertParamIsClone<AlignData>;
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for VCopyMemFlags {}
            impl VCopyMemFlags {
                pub fn lower(self, counttag: bool) -> u8 {
                    let mut out = 0;
                    for (id, op) in [
                        (1 << 7, counttag),
                        (1 << 5, self.volatile),
                        (1 << 4, self.nonoverlapping),
                    ] {
                        if op {
                            out |= id;
                        }
                    }
                    {
                        out |= self.srcalign.lower_vadd_style() << 2;
                        out |= self.tgtalign.lower_vadd_style();
                    }
                    out
                }
            }
            impl Default for VCopyMemFlags {
                fn default() -> Self {
                    Self {
                        volatile: true,
                        nonoverlapping: false,
                        srcalign: AlignData::Unknown,
                        tgtalign: AlignData::Unknown,
                    }
                }
            }
            impl AHQF for VCopyMemFlags {
                fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    [
                        ("volatile", self.volatile),
                        ("nonoverlapping", self.nonoverlapping),
                    ]
                        .into_iter()
                        .filter(|(_, t)| *t)
                        .map(|(x, _)| x)
                        .enumerate()
                        .try_for_each(|(idx, flag)| {
                            if idx != 0 {
                                f.write_fmt(format_args!(" "))?;
                            }
                            f.write_fmt(format_args!("{0}", flag))
                        })?;
                    f.write_fmt(format_args!("src:"))?;
                    self.srcalign.f(f)?;
                    f.write_fmt(format_args!(" tgt:"))?;
                    self.tgtalign.f(f)
                }
            }
            pub enum AlignData {
                #[default]
                Unknown,
                B16,
                B32,
                B64,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for AlignData {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(
                        f,
                        match self {
                            AlignData::Unknown => "Unknown",
                            AlignData::B16 => "B16",
                            AlignData::B32 => "B32",
                            AlignData::B64 => "B64",
                        },
                    )
                }
            }
            #[automatically_derived]
            impl ::core::default::Default for AlignData {
                #[inline]
                fn default() -> AlignData {
                    Self::Unknown
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for AlignData {}
            #[automatically_derived]
            impl ::core::clone::Clone for AlignData {
                #[inline]
                fn clone(&self) -> AlignData {
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for AlignData {}
            impl AlignData {
                pub fn lower_vadd_style(self) -> u8 {
                    match self {
                        Self::Unknown => 0b00,
                        Self::B16 => 0b01,
                        Self::B32 => 0b10,
                        Self::B64 => 0b11,
                    }
                }
            }
            impl AHQF for AlignData {
                fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(
                        format_args!(
                            "{0}",
                            match self {
                                Self::Unknown => "align(def)",
                                Self::B16 => "align(16)",
                                Self::B32 => "align(32)",
                                Self::B64 => "align(64)",
                            },
                        ),
                    )
                }
            }
        }
        pub mod loc {
            use std::fmt::Debug;
            use crate::mir::block::instr::AHQF;
            pub struct LocSrc {
                pub loc: VMLoc,
                pub offset: i8,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for LocSrc {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "LocSrc",
                        "loc",
                        &self.loc,
                        "offset",
                        &&self.offset,
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for LocSrc {}
            #[automatically_derived]
            impl ::core::clone::Clone for LocSrc {
                #[inline]
                fn clone(&self) -> LocSrc {
                    let _: ::core::clone::AssertParamIsClone<VMLoc>;
                    let _: ::core::clone::AssertParamIsClone<i8>;
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
                    self.offset == other.offset && self.loc == other.loc
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for LocSrc {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {
                    let _: ::core::cmp::AssertParamIsEq<VMLoc>;
                    let _: ::core::cmp::AssertParamIsEq<i8>;
                }
            }
            impl LocSrc {
                pub fn get_loc_bits(&self) -> u8 {
                    self.loc as u8
                }
            }
            impl AHQF for LocSrc {
                fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    self.loc.f(f)?;
                    f.write_fmt(format_args!(".of({0})", self.offset))
                }
            }
            #[repr(u8)]
            pub enum VMLoc {
                R1 = 0,
                R2 = 1,
                R3 = 2,
                R4 = 3,
                R5 = 4,
                R6 = 5,
                R7 = 6,
                R8 = 7,
                Scratchpad = 8,
                Largepad = 9,
                PtrFromR2 = 10,
                PtrFromR3 = 11,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for VMLoc {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(
                        f,
                        match self {
                            VMLoc::R1 => "R1",
                            VMLoc::R2 => "R2",
                            VMLoc::R3 => "R3",
                            VMLoc::R4 => "R4",
                            VMLoc::R5 => "R5",
                            VMLoc::R6 => "R6",
                            VMLoc::R7 => "R7",
                            VMLoc::R8 => "R8",
                            VMLoc::Scratchpad => "Scratchpad",
                            VMLoc::Largepad => "Largepad",
                            VMLoc::PtrFromR2 => "PtrFromR2",
                            VMLoc::PtrFromR3 => "PtrFromR3",
                        },
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for VMLoc {}
            #[automatically_derived]
            impl ::core::clone::Clone for VMLoc {
                #[inline]
                fn clone(&self) -> VMLoc {
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for VMLoc {}
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for VMLoc {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for VMLoc {
                #[inline]
                fn eq(&self, other: &VMLoc) -> bool {
                    let __self_discr = ::core::intrinsics::discriminant_value(self);
                    let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                    __self_discr == __arg1_discr
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for VMLoc {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {}
            }
            impl AHQF for VMLoc {
                fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(
                        format_args!(
                            "{0}",
                            match self {
                                Self::R1 => "r1",
                                Self::R2 => "r2",
                                Self::R3 => "r3",
                                Self::R4 => "r4",
                                Self::R5 => "r5",
                                Self::R6 => "r6",
                                Self::R7 => "r7",
                                Self::R8 => "r8",
                                Self::Largepad => "*pad_l",
                                Self::Scratchpad => "*pad_s",
                                Self::PtrFromR2 => "*r2",
                                Self::PtrFromR3 => "*r3",
                            },
                        ),
                    )
                }
            }
        }
        #[repr(u8)]
        pub enum IntTy {
            U64 = 0,
            U32 = 1,
            U16 = 2,
            U8 = 3,
            I64 = 4,
            I32 = 5,
            I16 = 6,
            I8 = 7,
            F64 = 8,
            F32 = 9,
        }
        impl AHQF for IntTy {
            fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(
                    format_args!(
                        "{0}",
                        match self {
                            Self::U64 => "u64",
                            Self::U32 => "u32",
                            Self::U16 => "u16",
                            Self::U8 => "u8",
                            Self::I64 => "i64",
                            Self::I32 => "i32",
                            Self::I16 => "i16",
                            Self::I8 => "i8",
                            Self::F64 => "f64",
                            Self::F32 => "f32",
                        },
                    ),
                )
            }
        }
        /// `V*` instructions support BOTH vector and scalar values
        /// non `V` prefixed instructions are scalar only
        ///
        /// `V_` prefixed instructions mean that they selectively accept vectors
        pub enum LLInstruction {
            /// Vectored copy operation
            /// This helps to copy between two locations
            Vcopy { src: LocSrc, target: LocSrc, count: Count, memflags: VCopyMemFlags },
            /// The MOV instruction
            ///
            /// The offset of src and target are ignored
            Mov { src: LocSrc, target: LocSrc },
            /// Returns the pointer to largepad in register r1
            LargepadPtr {},
            /// Returns the pointer to globalRWData in register r1
            GlobalRWPtr {},
            /// The vadd instruction
            Vadd {
                src1: LocSrc,
                src2: LocSrc,
                out: LocSrc,
                typedata: IntTy,
                count: u32,
            },
        }
        pub mod llprelude {
            use super::*;
            pub fn inst_vcopy(
                count: Count,
                memflags: VCopyMemFlags,
                src: LocSrc,
                target: LocSrc,
            ) -> LLInstruction {
                LLInstruction::inst_vcopy(count, memflags, src, target)
            }
            pub fn inst_mov(src: LocSrc, target: LocSrc) -> LLInstruction {
                LLInstruction::inst_mov(src, target)
            }
            pub fn inst_largepadptr() -> LLInstruction {
                LLInstruction::inst_largepadptr()
            }
            pub fn inst_globalrwptr() -> LLInstruction {
                LLInstruction::inst_globalrwptr()
            }
            pub fn inst_vadd(
                typedata: IntTy,
                count: u32,
                src1: LocSrc,
                src2: LocSrc,
                out: LocSrc,
            ) -> LLInstruction {
                LLInstruction::inst_vadd(typedata, count, src1, src2, out)
            }
        }
        impl LLInstruction {
            pub fn inst_vcopy(
                count: Count,
                memflags: VCopyMemFlags,
                src: LocSrc,
                target: LocSrc,
            ) -> Self {
                Self::Vcopy {
                    count,
                    memflags,
                    src,
                    target,
                }
            }
            pub fn inst_mov(src: LocSrc, target: LocSrc) -> Self {
                Self::Mov { src, target }
            }
            pub fn inst_largepadptr() -> Self {
                Self::LargepadPtr {}
            }
            pub fn inst_globalrwptr() -> Self {
                Self::GlobalRWPtr {}
            }
            pub fn inst_vadd(
                typedata: IntTy,
                count: u32,
                src1: LocSrc,
                src2: LocSrc,
                out: LocSrc,
            ) -> Self {
                Self::Vadd {
                    typedata,
                    count,
                    src1,
                    src2,
                    out,
                }
            }
            pub fn lower(&self, buf: &mut Vec<u8>) {
                match self {
                    Self::Vcopy { src, target, count, memflags } => {
                        buf.push(INSTRUCTION_VCOPY);
                        (|
                            buf: &mut Vec<u8>,
                            count,
                            flags: &VCopyMemFlags,
                            src: &LocSrc,
                            target: &LocSrc|
                        {
                            let (counttag, count) = match count {
                                &Count::Abs { abs } => (false, abs),
                                Count::ReadFromR1 => (true, 0),
                            };
                            buf.push(flags.lower(counttag));
                            buf.push({
                                let mut out = 0;
                                out |= src.get_loc_bits() << 4;
                                out |= target.get_loc_bits();
                                out
                            });
                            buf.extend(count.to_le_bytes());
                            buf.extend((src.offset as i32).to_le_bytes());
                            buf.extend((target.offset as i32).to_le_bytes());
                        })(buf, count, memflags, src, target);
                    }
                    Self::Mov { src, target } => {
                        buf.push(INSTRUCTION_MOV);
                        (|buf: &mut Vec<u8>, src: &LocSrc, target: &LocSrc| {
                            buf.push({
                                let mut out = 0;
                                out |= src.get_loc_bits() << 4;
                                out |= target.get_loc_bits();
                                out
                            });
                        })(buf, src, target);
                    }
                    Self::LargepadPtr {} => {
                        buf.push(INSTRUCTION_MOV);
                        (|buf: &mut Vec<u8>| {
                            buf.push({
                                let mut out = 0;
                                out |= 12 << 4;
                                out |= 12;
                                out
                            });
                        })(buf);
                    }
                    Self::GlobalRWPtr {} => {
                        buf.push(INSTRUCTION_MOV);
                        (|buf: &mut Vec<u8>| {
                            buf.push({
                                let mut out = 0;
                                out |= 13 << 4;
                                out |= 13;
                                out
                            });
                        })(buf);
                    }
                    Self::Vadd { src1, src2, out, typedata, count } => {
                        buf.push(INSTRUCTION_VADD);
                        (|buf, typedata, count, src1, src2, out| {})(
                            buf,
                            typedata,
                            count,
                            src1,
                            src2,
                            out,
                        );
                    }
                }
            }
            pub fn format(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    Self::Vcopy { src, target, count, memflags } => {
                        let name = "vcopy";
                        #[allow(unused)]
                        let mut outputs = false;
                        target.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        outputs = true;
                        if outputs {
                            f.write_fmt(format_args!("= "))?;
                        }
                        f.write_fmt(format_args!("{0}", name))?;
                        f.write_fmt(format_args!(" "))?;
                        count.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        memflags.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        src.f(f)?;
                    }
                    Self::Mov { src, target } => {
                        let name = "mov";
                        #[allow(unused)]
                        let mut outputs = false;
                        target.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        outputs = true;
                        if outputs {
                            f.write_fmt(format_args!("= "))?;
                        }
                        f.write_fmt(format_args!("{0}", name))?;
                        f.write_fmt(format_args!(" "))?;
                        src.f(f)?;
                    }
                    Self::LargepadPtr {} => {
                        let name = "largepadptr";
                        #[allow(unused)]
                        let mut outputs = false;
                        if outputs {
                            f.write_fmt(format_args!("= "))?;
                        }
                        f.write_fmt(format_args!("{0}", name))?;
                    }
                    Self::GlobalRWPtr {} => {
                        let name = "globalrwptr";
                        #[allow(unused)]
                        let mut outputs = false;
                        if outputs {
                            f.write_fmt(format_args!("= "))?;
                        }
                        f.write_fmt(format_args!("{0}", name))?;
                    }
                    Self::Vadd { src1, src2, out, typedata, count } => {
                        let name = "vadd";
                        #[allow(unused)]
                        let mut outputs = false;
                        out.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        outputs = true;
                        if outputs {
                            f.write_fmt(format_args!("= "))?;
                        }
                        f.write_fmt(format_args!("{0}", name))?;
                        f.write_fmt(format_args!(" "))?;
                        typedata.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        count.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        src1.f(f)?;
                        f.write_fmt(format_args!(" "))?;
                        src2.f(f)?;
                    }
                }
                f.write_fmt(format_args!(""))
            }
        }
    }
}
pub mod mir {
    use std::{collections::HashMap, fmt::Debug, marker::PhantomData, num::NonZeroUsize};
    use rapidhash::{HashMapExt, RapidHashMap};
    use crate::{
        StringRef, StringStore,
        mir::{
            function::Function,
            value::{BaseType, ValueType, ValueTypeRef, sig::{Signature, SignatureRef}},
        },
        saemit::machine::TargetVM,
    };
    pub mod block {
        use std::{collections::HashSet, fmt::Formatter};
        use crate::{
            StringStore, mir::{block::instr::HLInstruction, function::ssa::ValueId},
        };
        use rapidhash::fast::RandomState;
        pub mod instr {
            use crate::mir::{block::BlockId, function::ssa::ValueId, value::BaseType};
            use const_str::convert_ascii_case;
            use std::fmt::Formatter;
            pub mod loc {
                use crate::llir::instr::loc::VMLoc;
                pub struct LocSrc {
                    pub reg: VMLoc,
                    pub offset: i8,
                    /// What's the canoical width
                    pub width: usize,
                    /// Upto how much does this span
                    pub count: u8,
                }
                #[automatically_derived]
                impl ::core::fmt::Debug for LocSrc {
                    #[inline]
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        ::core::fmt::Formatter::debug_struct_field4_finish(
                            f,
                            "LocSrc",
                            "reg",
                            &self.reg,
                            "offset",
                            &self.offset,
                            "width",
                            &self.width,
                            "count",
                            &&self.count,
                        )
                    }
                }
                #[automatically_derived]
                #[doc(hidden)]
                unsafe impl ::core::clone::TrivialClone for LocSrc {}
                #[automatically_derived]
                impl ::core::clone::Clone for LocSrc {
                    #[inline]
                    fn clone(&self) -> LocSrc {
                        let _: ::core::clone::AssertParamIsClone<VMLoc>;
                        let _: ::core::clone::AssertParamIsClone<i8>;
                        let _: ::core::clone::AssertParamIsClone<usize>;
                        let _: ::core::clone::AssertParamIsClone<u8>;
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
                        self.offset == other.offset && self.count == other.count
                            && self.reg == other.reg && self.width == other.width
                    }
                }
                #[automatically_derived]
                impl ::core::cmp::Eq for LocSrc {
                    #[inline]
                    #[doc(hidden)]
                    #[coverage(off)]
                    fn assert_fields_are_eq(&self) {
                        let _: ::core::cmp::AssertParamIsEq<VMLoc>;
                        let _: ::core::cmp::AssertParamIsEq<i8>;
                        let _: ::core::cmp::AssertParamIsEq<usize>;
                        let _: ::core::cmp::AssertParamIsEq<u8>;
                    }
                }
            }
            /// `V*` instructions support BOTH vector and scalar values
            /// non `V` prefixed instructions are scalar only
            ///
            /// `V_` prefixed instructions mean that they selectively accept vectors
            pub enum HLInstruction<T: Register> {
                Vadd { src1: T, src2: T, out: T },
                Vads { src1: T, src2: T, out: T },
                Adc { src1: T, src2: T, out: T },
                VSub { src1: T, src2: T, out: T },
                VSsat { src1: T, src2: T, out: T },
                Sbb { src1: T, src2: T, out: T },
                VMulLo { src1: T, src2: T, out: T },
                VMulHi { src1: T, src2: T, out: T },
                /// out is 2x the size of src
                VMulWide { src1: T, src2: T, out: T },
                Div { src: T, divisor: T, out: T },
                Rem { src: T, divisor: T, out: T },
                VAddf { a: T, b: T, out: T },
                VSubf { a: T, b: T, out: T },
                VMulf { a: T, b: T, out: T },
                VDivf { a: T, b: T, out: T },
                /// Computes `a*b + c` as a single step
                VFma { a: T, b: T, c: T, out: T },
                Jump { block: BlockId, args: Box<[ValueId]> },
                JumpIf { val: T, zero: BlockId, nonzero: BlockId, args: Box<[ValueId]> },
                ICompare { a: T, b: T, result: T, comparison: IntComparison },
                FCompare { a: T, b: T, result: T, comparison: FloatComparison },
                /// Set an immediate upto 8 bytes
                Set { out: T, typedata: BaseType, value: u64 },
                Return { out: T },
            }
            impl<T: Register> HLInstruction<T> {
                pub fn format(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    match self {
                        Self::Vadd { src1, src2, out } => {
                            let name = "vadd";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::Vads { src1, src2, out } => {
                            let name = "vads";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::Adc { src1, src2, out } => {
                            let name = "adc";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::VSub { src1, src2, out } => {
                            let name = "vsub";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::VSsat { src1, src2, out } => {
                            let name = "vssat";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::Sbb { src1, src2, out } => {
                            let name = "sbb";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::VMulLo { src1, src2, out } => {
                            let name = "vmullo";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::VMulHi { src1, src2, out } => {
                            let name = "vmulhi";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::VMulWide { src1, src2, out } => {
                            let name = "vmulwide";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src1.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            src2.f(f)?;
                        }
                        Self::Div { src, divisor, out } => {
                            let name = "div";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            divisor.f(f)?;
                        }
                        Self::Rem { src, divisor, out } => {
                            let name = "rem";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            src.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            divisor.f(f)?;
                        }
                        Self::VAddf { a, b, out } => {
                            let name = "vaddf";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            a.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            b.f(f)?;
                        }
                        Self::VSubf { a, b, out } => {
                            let name = "vsubf";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            a.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            b.f(f)?;
                        }
                        Self::VMulf { a, b, out } => {
                            let name = "vmulf";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            a.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            b.f(f)?;
                        }
                        Self::VDivf { a, b, out } => {
                            let name = "vdivf";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            a.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            b.f(f)?;
                        }
                        Self::VFma { a, b, c, out } => {
                            let name = "vfma";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            a.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            b.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            c.f(f)?;
                        }
                        Self::Jump { block, args } => {
                            let name = "jump";
                            #[allow(unused)]
                            let mut outputs = false;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            block.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            args.f(f)?;
                        }
                        Self::JumpIf { val, zero, nonzero, args } => {
                            let name = "jumpif";
                            #[allow(unused)]
                            let mut outputs = false;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            val.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            zero.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            nonzero.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            args.f(f)?;
                        }
                        Self::ICompare { a, b, result, comparison } => {
                            let name = "icompare";
                            #[allow(unused)]
                            let mut outputs = false;
                            result.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            a.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            b.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            comparison.f(f)?;
                        }
                        Self::FCompare { a, b, result, comparison } => {
                            let name = "fcompare";
                            #[allow(unused)]
                            let mut outputs = false;
                            result.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            a.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            b.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            comparison.f(f)?;
                        }
                        Self::Set { out, typedata, value } => {
                            let name = "set";
                            #[allow(unused)]
                            let mut outputs = false;
                            out.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            outputs = true;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            typedata.f(f)?;
                            f.write_fmt(format_args!(" "))?;
                            value.f(f)?;
                        }
                        Self::Return { out } => {
                            let name = "return";
                            #[allow(unused)]
                            let mut outputs = false;
                            if outputs {
                                f.write_fmt(format_args!("= "))?;
                            }
                            f.write_fmt(format_args!("{0}", name))?;
                            f.write_fmt(format_args!(" "))?;
                            out.f(f)?;
                        }
                    }
                    f.write_fmt(format_args!(""))
                }
                pub fn all_vals<E>(&self, mut cb: E)
                where
                    E: FnMut(&T),
                {
                    match self {
                        Self::Vadd { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::Vads { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::Adc { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::VSub { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::VSsat { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::Sbb { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::VMulLo { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::VMulHi { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::VMulWide { src1, src2, out, .. } => {
                            cb(src1);
                            cb(src2);
                            cb(out);
                        }
                        Self::Div { src, divisor, out, .. } => {
                            cb(src);
                            cb(divisor);
                            cb(out);
                        }
                        Self::Rem { src, divisor, out, .. } => {
                            cb(src);
                            cb(divisor);
                            cb(out);
                        }
                        Self::VAddf { a, b, out, .. } => {
                            cb(a);
                            cb(b);
                            cb(out);
                        }
                        Self::VSubf { a, b, out, .. } => {
                            cb(a);
                            cb(b);
                            cb(out);
                        }
                        Self::VMulf { a, b, out, .. } => {
                            cb(a);
                            cb(b);
                            cb(out);
                        }
                        Self::VDivf { a, b, out, .. } => {
                            cb(a);
                            cb(b);
                            cb(out);
                        }
                        Self::VFma { a, b, c, out, .. } => {
                            cb(a);
                            cb(b);
                            cb(c);
                            cb(out);
                        }
                        Self::Jump { .. } => {}
                        Self::JumpIf { val, .. } => {
                            cb(val);
                        }
                        Self::ICompare { a, b, result, .. } => {
                            cb(a);
                            cb(b);
                            cb(result);
                        }
                        Self::FCompare { a, b, result, .. } => {
                            cb(a);
                            cb(b);
                            cb(result);
                        }
                        Self::Set { out, .. } => {
                            cb(out);
                        }
                        Self::Return { out, .. } => {
                            cb(out);
                        }
                    }
                }
                pub fn src<E>(&self, mut cb: E)
                where
                    E: FnMut(&T),
                {
                    match self {
                        Self::Vadd { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::Vads { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::Adc { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::VSub { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::VSsat { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::Sbb { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::VMulLo { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::VMulHi { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::VMulWide { src1, src2, .. } => {
                            cb(src1);
                            cb(src2);
                        }
                        Self::Div { src, divisor, .. } => {
                            cb(src);
                            cb(divisor);
                        }
                        Self::Rem { src, divisor, .. } => {
                            cb(src);
                            cb(divisor);
                        }
                        Self::VAddf { a, b, .. } => {
                            cb(a);
                            cb(b);
                        }
                        Self::VSubf { a, b, .. } => {
                            cb(a);
                            cb(b);
                        }
                        Self::VMulf { a, b, .. } => {
                            cb(a);
                            cb(b);
                        }
                        Self::VDivf { a, b, .. } => {
                            cb(a);
                            cb(b);
                        }
                        Self::VFma { a, b, c, .. } => {
                            cb(a);
                            cb(b);
                            cb(c);
                        }
                        Self::Jump { .. } => {}
                        Self::JumpIf { val, .. } => {
                            cb(val);
                        }
                        Self::ICompare { a, b, .. } => {
                            cb(a);
                            cb(b);
                        }
                        Self::FCompare { a, b, .. } => {
                            cb(a);
                            cb(b);
                        }
                        Self::Set { .. } => {}
                        Self::Return { out, .. } => {
                            cb(out);
                        }
                    }
                }
                pub fn outputs<E>(&self, mut cb: E)
                where
                    E: FnMut(&T),
                {
                    match self {
                        Self::Vadd { out, .. } => {
                            cb(out);
                        }
                        Self::Vads { out, .. } => {
                            cb(out);
                        }
                        Self::Adc { out, .. } => {
                            cb(out);
                        }
                        Self::VSub { out, .. } => {
                            cb(out);
                        }
                        Self::VSsat { out, .. } => {
                            cb(out);
                        }
                        Self::Sbb { out, .. } => {
                            cb(out);
                        }
                        Self::VMulLo { out, .. } => {
                            cb(out);
                        }
                        Self::VMulHi { out, .. } => {
                            cb(out);
                        }
                        Self::VMulWide { out, .. } => {
                            cb(out);
                        }
                        Self::Div { out, .. } => {
                            cb(out);
                        }
                        Self::Rem { out, .. } => {
                            cb(out);
                        }
                        Self::VAddf { out, .. } => {
                            cb(out);
                        }
                        Self::VSubf { out, .. } => {
                            cb(out);
                        }
                        Self::VMulf { out, .. } => {
                            cb(out);
                        }
                        Self::VDivf { out, .. } => {
                            cb(out);
                        }
                        Self::VFma { out, .. } => {
                            cb(out);
                        }
                        Self::Jump { .. } => {}
                        Self::JumpIf { .. } => {}
                        Self::ICompare { result, .. } => {
                            cb(result);
                        }
                        Self::FCompare { result, .. } => {
                            cb(result);
                        }
                        Self::Set { out, .. } => {
                            cb(out);
                        }
                        Self::Return { .. } => {}
                    }
                }
            }
            #[repr(u8)]
            pub enum IntComparison {
                Equal = 0,
                NotEqual = 1,
                LessThan = 2,
                LessThanEqual = 3,
                GreaterThan = 4,
                GreaterThanEqual = 5,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for IntComparison {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(
                        f,
                        match self {
                            IntComparison::Equal => "Equal",
                            IntComparison::NotEqual => "NotEqual",
                            IntComparison::LessThan => "LessThan",
                            IntComparison::LessThanEqual => "LessThanEqual",
                            IntComparison::GreaterThan => "GreaterThan",
                            IntComparison::GreaterThanEqual => "GreaterThanEqual",
                        },
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for IntComparison {}
            #[automatically_derived]
            impl ::core::clone::Clone for IntComparison {
                #[inline]
                fn clone(&self) -> IntComparison {
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for IntComparison {}
            #[automatically_derived]
            impl ::core::hash::Hash for IntComparison {
                #[inline]
                fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                    let __self_discr = ::core::intrinsics::discriminant_value(self);
                    ::core::hash::Hash::hash(&__self_discr, state)
                }
            }
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for IntComparison {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for IntComparison {
                #[inline]
                fn eq(&self, other: &IntComparison) -> bool {
                    let __self_discr = ::core::intrinsics::discriminant_value(self);
                    let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                    __self_discr == __arg1_discr
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for IntComparison {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {}
            }
            impl IntComparison {
                pub(crate) fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(
                        format_args!(
                            "{0}",
                            match self {
                                Self::Equal => "eq",
                                Self::NotEqual => "ne",
                                Self::LessThan => "lt",
                                Self::LessThanEqual => "le",
                                Self::GreaterThan => "gt",
                                Self::GreaterThanEqual => "ge",
                            },
                        ),
                    )
                }
            }
            #[repr(u8)]
            pub enum FloatComparison {
                Ordered = 10,
                Unordered = 11,
                Equal = 12,
                NotEqual = 13,
                OrderedNotEqual = 14,
                UnorderedOrEqual = 15,
                LessThan = 16,
                LessThanOrEqual = 17,
                GreaterThan = 18,
                GreaterThanOrEqual = 19,
                UnorderedOrLessThan = 20,
                UnorderedOrLessThanOrEqual = 21,
                UnorderedOrGreaterThan = 22,
                UnorderedOrGreaterThanOrEqual = 23,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for FloatComparison {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(
                        f,
                        match self {
                            FloatComparison::Ordered => "Ordered",
                            FloatComparison::Unordered => "Unordered",
                            FloatComparison::Equal => "Equal",
                            FloatComparison::NotEqual => "NotEqual",
                            FloatComparison::OrderedNotEqual => "OrderedNotEqual",
                            FloatComparison::UnorderedOrEqual => "UnorderedOrEqual",
                            FloatComparison::LessThan => "LessThan",
                            FloatComparison::LessThanOrEqual => "LessThanOrEqual",
                            FloatComparison::GreaterThan => "GreaterThan",
                            FloatComparison::GreaterThanOrEqual => "GreaterThanOrEqual",
                            FloatComparison::UnorderedOrLessThan => "UnorderedOrLessThan",
                            FloatComparison::UnorderedOrLessThanOrEqual => {
                                "UnorderedOrLessThanOrEqual"
                            }
                            FloatComparison::UnorderedOrGreaterThan => {
                                "UnorderedOrGreaterThan"
                            }
                            FloatComparison::UnorderedOrGreaterThanOrEqual => {
                                "UnorderedOrGreaterThanOrEqual"
                            }
                        },
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for FloatComparison {}
            #[automatically_derived]
            impl ::core::clone::Clone for FloatComparison {
                #[inline]
                fn clone(&self) -> FloatComparison {
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for FloatComparison {}
            #[automatically_derived]
            impl ::core::hash::Hash for FloatComparison {
                #[inline]
                fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                    let __self_discr = ::core::intrinsics::discriminant_value(self);
                    ::core::hash::Hash::hash(&__self_discr, state)
                }
            }
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for FloatComparison {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for FloatComparison {
                #[inline]
                fn eq(&self, other: &FloatComparison) -> bool {
                    let __self_discr = ::core::intrinsics::discriminant_value(self);
                    let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                    __self_discr == __arg1_discr
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for FloatComparison {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {}
            }
            impl FloatComparison {
                pub(crate) fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(
                        format_args!(
                            "{0}",
                            match self {
                                Self::Ordered => "ord",
                                Self::Unordered => "nord",
                                Self::Equal => "eq",
                                Self::NotEqual => "ne",
                                Self::OrderedNotEqual => "one",
                                Self::UnorderedOrEqual => "ue",
                                Self::LessThan => "lt",
                                Self::LessThanOrEqual => "le",
                                Self::GreaterThan => "gt",
                                Self::GreaterThanOrEqual => "ge",
                                Self::UnorderedOrLessThan => "ult",
                                Self::UnorderedOrLessThanOrEqual => "ule",
                                Self::UnorderedOrGreaterThan => "ugt",
                                Self::UnorderedOrGreaterThanOrEqual => "uge",
                            },
                        ),
                    )
                }
            }
            #[allow(private_bounds)]
            pub trait Register: Internal {
                fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
            }
            pub(crate) trait Internal {}
            impl Internal for ValueId {}
            impl Register for ValueId {
                fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(format_args!("v{0}", self.0))
                }
            }
            pub(crate) trait AHQF {
                fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
            }
            impl AHQF for u64 {
                fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(format_args!("{0}", self))
                }
            }
            impl AHQF for u32 {
                fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(format_args!("{0}", self))
                }
            }
            impl AHQF for BaseType {
                fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    self.format(f)
                }
            }
            impl AHQF for &Box<[ValueId]> {
                fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(format_args!("("))?;
                    for item in self.iter() {
                        f.write_fmt(format_args!(" "))?;
                        item.f(f)?;
                    }
                    f.write_fmt(format_args!(" )"))
                }
            }
        }
        pub struct Block<'a, T: StringStore> {
            pub store: &'a T,
            pub(crate) v0: bool,
            pub(crate) instr: Vec<HLInstruction<ValueId>>,
            pub(crate) preds: HashSet<BlockId, RandomState>,
            pub(crate) succ: HashSet<BlockId, RandomState>,
            pub(crate) params: Vec<ValueId>,
        }
        #[repr(transparent)]
        pub struct BlockId(pub(crate) usize);
        #[automatically_derived]
        impl ::core::fmt::Debug for BlockId {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BlockId", &&self.0)
            }
        }
        #[automatically_derived]
        #[doc(hidden)]
        unsafe impl ::core::clone::TrivialClone for BlockId {}
        #[automatically_derived]
        impl ::core::clone::Clone for BlockId {
            #[inline]
            fn clone(&self) -> BlockId {
                let _: ::core::clone::AssertParamIsClone<usize>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for BlockId {}
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for BlockId {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for BlockId {
            #[inline]
            fn eq(&self, other: &BlockId) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for BlockId {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_fields_are_eq(&self) {
                let _: ::core::cmp::AssertParamIsEq<usize>;
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for BlockId {
            #[inline]
            fn partial_cmp(
                &self,
                other: &BlockId,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Ord for BlockId {
            #[inline]
            fn cmp(&self, other: &BlockId) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
        #[automatically_derived]
        impl ::core::hash::Hash for BlockId {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                ::core::hash::Hash::hash(&self.0, state)
            }
        }
        pub const BLOCK_0: BlockId = BlockId(0);
        impl BlockId {
            pub(crate) fn f(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(format_args!("#{0}", self.0))
            }
        }
    }
    pub mod function {
        use crate::{
            StringRef, StringStore,
            mir::{
                Module, block::Block,
                function::{builder::FunctionBuilder, ssa::{SSA, ValueId}},
                value::sig::SignatureRef,
            },
        };
        pub mod builder {
            use crate::{
                StringStore,
                mir::{
                    Module, block::{Block, BlockId, instr::HLInstruction},
                    function::{Function, ir::InstructionId, ssa::{SSA, ValueId}},
                    ssa::SSAResolver, value::{ValueType, ValueTypeRef},
                },
            };
            pub struct FunctionBuilder<'a, 'b, T: StringStore> {
                pub(crate) parent: &'b mut Function<'a, T>,
                pub(crate) module: &'b Module<'a, T>,
                pub(crate) currblock: BlockId,
                instr: InstId,
                pub hmap: Vec<SSAResolver>,
            }
            impl<'a, 'b, T: StringStore> FunctionBuilder<'a, 'b, T> {
                pub(crate) fn new(
                    parent: &'b mut Function<'a, T>,
                    module: &'b Module<'a, T>,
                ) -> Self {
                    let sig = module.sigs.get(parent.sig.0).unwrap();
                    let empty_blocks = parent.blocks.is_empty();
                    let mut out = Self {
                        parent,
                        module,
                        currblock: BlockId(0),
                        instr: InstId(0),
                        hmap: Vec::with_capacity(12),
                    };
                    if empty_blocks {
                        if let Some(arg) = sig.args {
                            out.block(&[arg]);
                        } else {
                            out.block(&[]);
                        }
                    }
                    out
                }
                /// Creates a new Block with the given DEFAULT params
                pub fn block(&mut self, params: &[ValueTypeRef]) -> BlockId {
                    let params = params
                        .iter()
                        .map(|&typetag| self.define_ssa(typetag))
                        .collect::<Vec<_>>();
                    let newid = self.parent.blocks.len();
                    self.parent
                        .blocks
                        .push(Block {
                            store: self.parent.store,
                            instr: Vec::with_capacity(16),
                            v0: newid == 0,
                            preds: Default::default(),
                            succ: Default::default(),
                            params,
                        });
                    BlockId(newid)
                }
                /// Get the total [HLInstruction] at the current moment for the supplied block id
                pub fn block_inst(
                    &mut self,
                    block: BlockId,
                ) -> Option<impl Iterator<Item = (InstId, &HLInstruction<ValueId>)>> {
                    self.parent
                        .blocks
                        .get(block.0)
                        .map(|x| {
                            x.instr
                                .as_slice()
                                .iter()
                                .enumerate()
                                .map(|(idx, i)| (InstId(idx), i))
                        })
                }
                /// Get the total [HLInstruction]s at the current moment for the supplied block id
                pub fn block_total_inst(&mut self, block: BlockId) -> Option<usize> {
                    self.parent.blocks.get(block.0).map(|x| x.instr.len())
                }
                /// Position builder at the end of the block.
                pub fn position_end(&mut self, block: BlockId) -> Option<()> {
                    self.currblock = block;
                    self.instr = InstId(
                        { self.parent.blocks.get(block.0)? }.instr.len(),
                    );
                    Some(())
                }
                /// Position builder at the Instruction specified by its ID
                pub fn position_at(
                    &mut self,
                    block: BlockId,
                    inst: InstId,
                ) -> Option<()> {
                    self.currblock = block;
                    let max = { self.parent.blocks.get(block.0)? }.instr.len();
                    if inst.0 > max {
                        return None;
                    }
                    self.instr = inst;
                    Some(())
                }
                pub fn type_of(&self, v: ValueId) -> (ValueTypeRef, &ValueType<'_>) {
                    let tag = self.parent.get_ssa(v).unwrap().typetag;
                    (tag, self.module.type_data(tag).unwrap())
                }
                pub(crate) fn define_ssa(&mut self, typetag: ValueTypeRef) -> ValueId {
                    let idx = self.parent.ssa.len();
                    self.parent
                        .ssa
                        .push(SSA {
                            _parent: self.parent.store,
                            typetag,
                        });
                    ValueId(idx)
                }
                pub(crate) fn inst_process(
                    &mut self,
                    inst: HLInstruction<ValueId>,
                ) -> InstructionId {
                    self.instr.0 += 1;
                    let instr = &mut unsafe {
                        self.parent.blocks.get_unchecked_mut(self.currblock.0)
                    }
                        .instr;
                    let id = instr.len();
                    instr.push(inst);
                    InstId(id)
                }
            }
            pub struct InstId(pub(crate) usize);
            #[automatically_derived]
            impl ::core::fmt::Debug for InstId {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "InstId",
                        &&self.0,
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for InstId {}
            #[automatically_derived]
            impl ::core::clone::Clone for InstId {
                #[inline]
                fn clone(&self) -> InstId {
                    let _: ::core::clone::AssertParamIsClone<usize>;
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for InstId {}
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for InstId {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for InstId {
                #[inline]
                fn eq(&self, other: &InstId) -> bool {
                    self.0 == other.0
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for InstId {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {
                    let _: ::core::cmp::AssertParamIsEq<usize>;
                }
            }
            #[automatically_derived]
            impl ::core::cmp::PartialOrd for InstId {
                #[inline]
                fn partial_cmp(
                    &self,
                    other: &InstId,
                ) -> ::core::option::Option<::core::cmp::Ordering> {
                    ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Ord for InstId {
                #[inline]
                fn cmp(&self, other: &InstId) -> ::core::cmp::Ordering {
                    ::core::cmp::Ord::cmp(&self.0, &other.0)
                }
            }
            #[automatically_derived]
            impl ::core::hash::Hash for InstId {
                #[inline]
                fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                    ::core::hash::Hash::hash(&self.0, state)
                }
            }
        }
        pub mod ir {
            use crate::{
                StringStore,
                mir::{
                    block::{BlockId, instr::HLInstruction},
                    function::{
                        builder::{FunctionBuilder, InstId},
                        ssa::ValueId,
                    },
                    value::{ValueType, ValueTypeRef},
                },
            };
            #[macro_use]
            mod macrodata {}
            impl<'a, 'b, T: StringStore> FunctionBuilder<'a, 'b, T> {
                fn jmpverify(
                    &mut self,
                    block: BlockId,
                    args: &[ValueId],
                ) -> Result<(), CommonError> {
                    let block = unsafe { self.parent.blocks.get_unchecked(block.0) };
                    if block.params.len() != args.len() {
                        return Err(CommonError::InvalidBlockArgs);
                    }
                    let succ = block
                        .params
                        .iter()
                        .zip(args.iter())
                        .map(|(&a, &b)| {
                            (
                                self.parent.get_ssa(a).unwrap(),
                                self.parent.get_ssa(b).unwrap(),
                            )
                        })
                        .all(|(a, b)| a.typetag == b.typetag);
                    if !succ {
                        return Err(CommonError::InvalidBlockArgs);
                    }
                    Ok(())
                }
                #[allow(unused_parens)]
                pub fn vadd(
                    &mut self,
                    a: ValueId,
                    b: ValueId,
                ) -> Result<Instruction<(ValueId)>, CommonError> {
                    let ctx = self;
                    {
                        if ![ctx.type_of(a), ctx.type_of(b)]
                            .windows(2)
                            .all(|d| {
                                let (x, v) = d[0];
                                let (y, _) = d[1];
                                x == y && v.is_num()
                            })
                        {
                            return Err(CommonError::TypeVerificationFailure);
                        }
                    };
                    let out = {
                        {
                            let (vtype, _) = ctx.type_of(a);
                            let out = ctx.define_ssa(vtype);
                            let (_, t) = ctx.type_of(a);
                            let id = if t.is_float() {
                                ctx.inst_process(HLInstruction::VAddf { a, b, out })
                            } else {
                                ctx.inst_process(HLInstruction::Vadd {
                                    src1: a,
                                    src2: b,
                                    out,
                                })
                            };
                            Instruction { id, out }
                        }
                    };
                    Ok(out)
                }
                #[allow(unused_parens)]
                pub fn vsub(
                    &mut self,
                    a: ValueId,
                    b: ValueId,
                ) -> Result<Instruction<(ValueId)>, CommonError> {
                    let ctx = self;
                    {
                        if ![ctx.type_of(a), ctx.type_of(b)]
                            .windows(2)
                            .all(|d| {
                                let (x, v) = d[0];
                                let (y, _) = d[1];
                                x == y && v.is_num()
                            })
                        {
                            return Err(CommonError::TypeVerificationFailure);
                        }
                    };
                    let out = {
                        {
                            let (vtype, _) = ctx.type_of(a);
                            let out = ctx.define_ssa(vtype);
                            let (_, t) = ctx.type_of(a);
                            let id = if t.is_float() {
                                ctx.inst_process(HLInstruction::VSubf { a, b, out })
                            } else {
                                ctx.inst_process(HLInstruction::VSub {
                                    src1: a,
                                    src2: b,
                                    out,
                                })
                            };
                            Instruction { id, out }
                        }
                    };
                    Ok(out)
                }
                #[allow(unused_parens)]
                pub fn vmul(
                    &mut self,
                    a: ValueId,
                    b: ValueId,
                ) -> Result<Instruction<(ValueId)>, CommonError> {
                    let ctx = self;
                    {
                        if ![ctx.type_of(a), ctx.type_of(b)]
                            .windows(2)
                            .all(|d| {
                                let (x, v) = d[0];
                                let (y, _) = d[1];
                                x == y && v.is_num()
                            })
                        {
                            return Err(CommonError::TypeVerificationFailure);
                        }
                    };
                    let out = {
                        {
                            let (vtype, _) = ctx.type_of(a);
                            let out = ctx.define_ssa(vtype);
                            let (_, t) = ctx.type_of(a);
                            let id = if !t.is_float() {
                                ctx.inst_process(HLInstruction::VMulLo {
                                    src1: a,
                                    src2: b,
                                    out,
                                })
                            } else {
                                ctx.inst_process(HLInstruction::VMulf { a, b, out })
                            };
                            Instruction { id, out }
                        }
                    };
                    Ok(out)
                }
                #[allow(unused_parens)]
                pub fn div(
                    &mut self,
                    a: ValueId,
                    b: ValueId,
                ) -> Result<Instruction<(ValueId)>, CommonError> {
                    let ctx = self;
                    {
                        if ![ctx.type_of(a), ctx.type_of(b)]
                            .windows(2)
                            .all(|d| {
                                let (x, v) = d[0];
                                let (y, _) = d[1];
                                x == y && v.is_int()
                            })
                        {
                            return Err(CommonError::TypeVerificationFailure);
                        }
                        if ![ctx.type_of(a), ctx.type_of(b)]
                            .windows(2)
                            .all(|d| {
                                let (x, v) = d[0];
                                let (y, _) = d[1];
                                x == y && v.is_scalar()
                            })
                        {
                            return Err(CommonError::TypeVerificationFailure);
                        }
                    };
                    let out = {
                        {
                            let (vtype, _) = ctx.type_of(a);
                            let out = ctx.define_ssa(vtype);
                            let id = ctx
                                .inst_process(HLInstruction::Div {
                                    src: a,
                                    divisor: b,
                                    out,
                                });
                            Instruction { id, out }
                        }
                    };
                    Ok(out)
                }
                #[allow(unused_parens)]
                pub fn vdivf(
                    &mut self,
                    a: ValueId,
                    b: ValueId,
                ) -> Result<Instruction<(ValueId)>, CommonError> {
                    let ctx = self;
                    {
                        if ![ctx.type_of(a), ctx.type_of(b)]
                            .windows(2)
                            .all(|d| {
                                let (x, v) = d[0];
                                let (y, _) = d[1];
                                x == y && v.is_float()
                            })
                        {
                            return Err(CommonError::TypeVerificationFailure);
                        }
                    };
                    let out = {
                        {
                            let (vtype, _) = ctx.type_of(a);
                            let out = ctx.define_ssa(vtype);
                            let id = ctx
                                .inst_process(HLInstruction::VDivf { a, b, out });
                            Instruction { id, out }
                        }
                    };
                    Ok(out)
                }
                #[allow(unused_parens)]
                pub fn jump(
                    &mut self,
                    block: BlockId,
                    args: &[ValueId],
                ) -> Result<Instruction<()>, CommonError> {
                    let ctx = self;
                    {
                        Self::jmpverify(ctx, block, args)?;
                    };
                    let out = {
                        {
                            let id = ctx
                                .inst_process(HLInstruction::Jump {
                                    block,
                                    args: Box::from(args),
                                });
                            unsafe {
                                _ = ctx
                                    .parent
                                    .blocks
                                    .get_unchecked_mut(ctx.currblock.0)
                                    .succ
                                    .insert(block);
                                _ = ctx
                                    .parent
                                    .blocks
                                    .get_unchecked_mut(block.0)
                                    .preds
                                    .insert(ctx.currblock);
                            }
                            Instruction { id, out: () }
                        }
                    };
                    Ok(out)
                }
                #[allow(unused_parens)]
                /// Initialize a constant integer
                pub fn iconst(
                    &mut self,
                    intty: ValueTypeRef,
                    value: u64,
                ) -> Result<Instruction<(ValueId)>, CommonError> {
                    let ctx = self;
                    {
                        let tt = ctx.module.type_data(intty).unwrap();
                        if !(tt.is_int() && tt.is_scalar()) {
                            return Err(CommonError::TypeVerificationFailure);
                        }
                    };
                    let out = {
                        {
                            let tt = ctx.module.type_data(intty).unwrap();
                            let typedata = match tt {
                                &ValueType::Base { base, .. } => base,
                                _ => {
                                    ::core::panicking::panic(
                                        "internal error: entered unreachable code",
                                    )
                                }
                            };
                            let out = ctx.define_ssa(intty);
                            let id = ctx
                                .inst_process(HLInstruction::Set {
                                    out,
                                    typedata,
                                    value,
                                });
                            Instruction { id, out }
                        }
                    };
                    Ok(out)
                }
            }
            pub enum CommonError {
                TypeVerificationFailure,
                InvalidBlockArgs,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for CommonError {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(
                        f,
                        match self {
                            CommonError::TypeVerificationFailure => {
                                "TypeVerificationFailure"
                            }
                            CommonError::InvalidBlockArgs => "InvalidBlockArgs",
                        },
                    )
                }
            }
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for CommonError {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for CommonError {
                #[inline]
                fn eq(&self, other: &CommonError) -> bool {
                    let __self_discr = ::core::intrinsics::discriminant_value(self);
                    let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                    __self_discr == __arg1_discr
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for CommonError {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {}
            }
            #[automatically_derived]
            impl ::core::cmp::PartialOrd for CommonError {
                #[inline]
                fn partial_cmp(
                    &self,
                    other: &CommonError,
                ) -> ::core::option::Option<::core::cmp::Ordering> {
                    ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Ord for CommonError {
                #[inline]
                fn cmp(&self, other: &CommonError) -> ::core::cmp::Ordering {
                    let __self_discr = ::core::intrinsics::discriminant_value(self);
                    let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                    ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr)
                }
            }
            #[automatically_derived]
            impl ::core::clone::Clone for CommonError {
                #[inline]
                fn clone(&self) -> CommonError {
                    match self {
                        CommonError::TypeVerificationFailure => {
                            CommonError::TypeVerificationFailure
                        }
                        CommonError::InvalidBlockArgs => CommonError::InvalidBlockArgs,
                    }
                }
            }
            pub struct Instruction<T> {
                pub id: InstId,
                pub out: T,
            }
            pub type InstructionId = InstId;
        }
        pub mod ssa {
            use std::fmt::Debug;
            use crate::{StringStore, mir::value::ValueTypeRef};
            pub struct SSA<'a, T: StringStore> {
                pub(crate) _parent: &'a T,
                pub typetag: ValueTypeRef,
            }
            pub struct ValueId(pub(crate) usize);
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for ValueId {}
            #[automatically_derived]
            impl ::core::clone::Clone for ValueId {
                #[inline]
                fn clone(&self) -> ValueId {
                    let _: ::core::clone::AssertParamIsClone<usize>;
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for ValueId {}
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for ValueId {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for ValueId {
                #[inline]
                fn eq(&self, other: &ValueId) -> bool {
                    self.0 == other.0
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for ValueId {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {
                    let _: ::core::cmp::AssertParamIsEq<usize>;
                }
            }
            #[automatically_derived]
            impl ::core::cmp::PartialOrd for ValueId {
                #[inline]
                fn partial_cmp(
                    &self,
                    other: &ValueId,
                ) -> ::core::option::Option<::core::cmp::Ordering> {
                    ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Ord for ValueId {
                #[inline]
                fn cmp(&self, other: &ValueId) -> ::core::cmp::Ordering {
                    ::core::cmp::Ord::cmp(&self.0, &other.0)
                }
            }
            #[automatically_derived]
            impl ::core::hash::Hash for ValueId {
                #[inline]
                fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                    ::core::hash::Hash::hash(&self.0, state)
                }
            }
            impl Debug for ValueId {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(format_args!("#{0}", self.0))
                }
            }
        }
        pub struct Function<'a, T: StringStore> {
            pub store: &'a T,
            pub(crate) name: StringRef<'a, T>,
            sig: SignatureRef,
            pub(crate) ssa: Vec<SSA<'a, T>>,
            pub(crate) blocks: Vec<Block<'a, T>>,
        }
        impl<'a, T: StringStore> Function<'a, T> {
            pub(crate) fn new(
                store: &'a T,
                name: StringRef<'a, T>,
                sig: SignatureRef,
            ) -> Self {
                Self {
                    store,
                    name,
                    sig,
                    ssa: Vec::with_capacity(16),
                    blocks: Vec::with_capacity(16),
                }
            }
            pub fn get_ssa(&self, ssa: ValueId) -> Option<&SSA<'a, T>> {
                self.ssa.get(ssa.0)
            }
            pub fn builder<'b>(
                &'b mut self,
                module: &'b Module<'a, T>,
            ) -> FunctionBuilder<'a, 'b, T> {
                FunctionBuilder::new(self, module)
            }
        }
        pub(crate) mod internal {
            use crate::{
                StringStore, mir::{Module, function::Function, value::ValueType},
            };
            use std::fmt::{Formatter, Result};
            impl<'a, T: StringStore> Function<'a, T> {
                pub(crate) fn print(
                    &self,
                    f: &mut Formatter,
                    module: &Module<T>,
                ) -> Result {
                    f.write_fmt(
                        format_args!(
                            "  fun {0} (@sig:#{1}):\n",
                            self.store.resolve(self.name).as_ref(),
                            self.sig.0 + 1,
                        ),
                    )?;
                    for (id, block) in self.blocks.iter().enumerate() {
                        {
                            f.write_fmt(format_args!("    @sig ("))?;
                            for &param in &block.params {
                                let typetag = self.get_ssa(param).unwrap().typetag;
                                match module.type_data(typetag).unwrap() {
                                    ValueType::Base { base, .. } => {
                                        f.write_fmt(format_args!(" "))?;
                                        base.format(f)?
                                    }
                                    _ => {
                                        f.write_fmt(format_args!(" @type:{0}", typetag.0.get()))?
                                    }
                                }
                            }
                            f.write_fmt(format_args!(" )\n"))?;
                        }
                        if block.v0 {
                            f.write_fmt(format_args!("    @entry\n"))?;
                        }
                        if !block.v0 {
                            if block.preds.is_empty() {
                                f.write_fmt(format_args!("    @orphan\n"))?;
                            } else {
                                f.write_fmt(format_args!("    @preds ("))?;
                                for &param in &block.preds {
                                    f.write_fmt(format_args!(" #{0}", param.0))?
                                }
                                f.write_fmt(format_args!(" )\n"))?;
                            }
                        }
                        if !block.succ.is_empty() {
                            f.write_fmt(format_args!("    @succs ("))?;
                            for &param in &block.succ {
                                f.write_fmt(format_args!(" #{0}", param.0))?
                            }
                            f.write_fmt(format_args!(" )\n"))?;
                        }
                        f.write_fmt(format_args!("    block #{0}", id))?;
                        if !block.params.is_empty() {
                            f.write_fmt(format_args!("("))?;
                            for &param in &block.params {
                                f.write_fmt(format_args!(" v{0}", param.0))?;
                            }
                            f.write_fmt(format_args!(" )"))?;
                        }
                        f.write_fmt(format_args!(":\n"))?;
                        for inst in &block.instr {
                            f.write_fmt(format_args!("      "))?;
                            inst.format(f)?;
                            f.write_fmt(format_args!("\n"))?;
                        }
                        f.write_fmt(format_args!("\n"))?;
                    }
                    Ok(())
                }
            }
        }
    }
    pub mod ssa {
        use std::collections::HashMap;
        use crate::mir::{block::BlockId, function::ssa::ValueId, value::ValueTypeRef};
        pub struct SSAResolver {
            pub typetag: ValueTypeRef,
            initval: ValueId,
            block_defs: HashMap<BlockId, ValueId, rapidhash::fast::RandomState>,
            phis: HashMap<BlockId, ValueId, rapidhash::fast::RandomState>,
        }
    }
    pub mod value {
        use std::{
            marker::PhantomData, num::NonZeroUsize, ops::{Deref, Sub},
            rc::Rc,
        };
        pub mod calc {
            use crate::{
                StringStore, mir::{Module, value::{Alignment, BaseType, ValueType}},
            };
            impl<'a> ValueType<'a> {
                pub fn size<T: StringStore>(&self, module: &Module<T>) -> usize {
                    match self {
                        Self::Base { base, .. } => width(base),
                        Self::Vector { base, count } => width(base) * (*count as usize),
                        Self::PrimaryUnion { composition, count, .. } => {
                            composition
                                .get(0..(*count as usize))
                                .unwrap_or(&[])
                                .iter()
                                .map(width)
                                .max()
                                .unwrap_or(8)
                                .next_multiple_of(self.align(module))
                        }
                        Self::Union { composition, .. } => {
                            composition
                                .iter()
                                .copied()
                                .filter_map(|x| module.type_data(x))
                                .map(|x| x.size(module))
                                .max()
                                .unwrap_or(8)
                                .next_multiple_of(self.align(module))
                        }
                        Self::PrimaryComposite { composition, count, .. } => {
                            let unpadded_size = composition
                                .get(0..(*count as usize))
                                .unwrap_or(&[])
                                .iter()
                                .fold(
                                    0usize,
                                    |current_size, field_ty| {
                                        let field_align = width(field_ty);
                                        current_size.next_multiple_of(field_align) + width(field_ty)
                                    },
                                );
                            unpadded_size.next_multiple_of(self.align(module))
                        }
                        Self::Composite { composition, .. } => {
                            let unpadded_size = composition
                                .as_ref()
                                .iter()
                                .filter_map(|&vtr| module.type_data(vtr))
                                .fold(
                                    0usize,
                                    |current_size, field_ty| {
                                        let field_align = field_ty.align(module);
                                        current_size.next_multiple_of(field_align)
                                            + field_ty.size(module)
                                    },
                                );
                            unpadded_size.next_multiple_of(self.align(module))
                        }
                    }
                }
                pub fn align<T: StringStore>(&self, module: &Module<T>) -> usize {
                    match self {
                        Self::Base { base, .. } => width(base),
                        Self::Vector { base, .. } => width(base),
                        Self::PrimaryUnion { composition, count, align } => {
                            align
                                .map(Alignment::align)
                                .unwrap_or_else(|| {
                                    composition
                                        .get(0..(*count as usize))
                                        .unwrap_or(&[])
                                        .iter()
                                        .map(width)
                                        .max()
                                        .unwrap_or(8)
                                })
                        }
                        Self::PrimaryComposite { composition, count, align } => {
                            align
                                .map(Alignment::align)
                                .unwrap_or_else(|| {
                                    composition
                                        .get(0..(*count as usize))
                                        .unwrap_or(&[])
                                        .iter()
                                        .map(width)
                                        .max()
                                        .unwrap_or(8)
                                })
                        }
                        Self::Composite { composition, align } => {
                            align
                                .map(Alignment::align)
                                .unwrap_or_else(|| {
                                    composition
                                        .iter()
                                        .copied()
                                        .filter_map(|x| module.type_data(x))
                                        .map(|x| x.align(module))
                                        .max()
                                        .unwrap_or(8)
                                })
                        }
                        Self::Union { composition, align } => {
                            align
                                .map(Alignment::align)
                                .unwrap_or_else(|| {
                                    composition
                                        .iter()
                                        .copied()
                                        .filter_map(|x| module.type_data(x))
                                        .map(|x| x.align(module))
                                        .max()
                                        .unwrap_or(8)
                                })
                        }
                    }
                }
            }
            fn width(base: &BaseType) -> usize {
                match base {
                    BaseType::UInt64 | BaseType::Int64 | BaseType::Double64 => 8,
                    BaseType::Float32 | BaseType::Int32 | BaseType::UInt32 => 4,
                    BaseType::Int16 | BaseType::UInt16 => 2,
                    BaseType::Int8 | BaseType::UInt8 => 1,
                }
            }
        }
        pub mod sig {
            use std::rc::Rc;
            use crate::{
                StringStore,
                mir::{Module, value::{ValueType, ValueTypeArray, ValueTypeRef}},
            };
            #[repr(align(8))]
            pub struct Signature {
                pub args: Option<ValueTypeRef>,
                pub returns: Option<ValueTypeRef>,
            }
            impl Signature {
                pub fn new<T: StringStore>(
                    module: &mut Module<T>,
                    argv: &[ValueTypeRef],
                    r#return: Option<ValueTypeRef>,
                ) -> Result<Self, SigError> {
                    let mut args = None;
                    let mut returns = None;
                    if !argv.is_empty() {
                        let unified = ValueType::Composite {
                            composition: ValueTypeArray::Rc(Rc::from(argv)),
                            align: None,
                        };
                        if unified.align(module) > 16 {
                            return Err(SigError::ArgvOveraligned);
                        }
                        if unified.size(module) > 64 {
                            return Err(SigError::ArgvOversized);
                        }
                        args = Some(module.insert_type(unified));
                    }
                    if let Some(ret) = r#return {
                        let rett = module
                            .type_data(ret)
                            .ok_or(SigError::ReturnTypeNotFound)?;
                        if rett.size(module) > 16 {
                            return Err(SigError::ReturnSizeOverflow);
                        }
                        if rett.align(module) > 32 {
                            return Err(SigError::ReturnAlignOverflow);
                        }
                        returns = Some(ret);
                    }
                    Ok(Self { args, returns })
                }
            }
            pub enum SigError {
                ArgvOversized,
                ArgvOveraligned,
                ReturnAlignOverflow,
                ReturnSizeOverflow,
                ReturnTypeNotFound,
            }
            #[automatically_derived]
            impl ::core::fmt::Debug for SigError {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::write_str(
                        f,
                        match self {
                            SigError::ArgvOversized => "ArgvOversized",
                            SigError::ArgvOveraligned => "ArgvOveraligned",
                            SigError::ReturnAlignOverflow => "ReturnAlignOverflow",
                            SigError::ReturnSizeOverflow => "ReturnSizeOverflow",
                            SigError::ReturnTypeNotFound => "ReturnTypeNotFound",
                        },
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for SigError {}
            #[automatically_derived]
            impl ::core::clone::Clone for SigError {
                #[inline]
                fn clone(&self) -> SigError {
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for SigError {}
            pub struct SignatureRef(pub(crate) usize);
            #[automatically_derived]
            impl ::core::fmt::Debug for SignatureRef {
                #[inline]
                fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "SignatureRef",
                        &&self.0,
                    )
                }
            }
            #[automatically_derived]
            #[doc(hidden)]
            unsafe impl ::core::clone::TrivialClone for SignatureRef {}
            #[automatically_derived]
            impl ::core::clone::Clone for SignatureRef {
                #[inline]
                fn clone(&self) -> SignatureRef {
                    let _: ::core::clone::AssertParamIsClone<usize>;
                    *self
                }
            }
            #[automatically_derived]
            impl ::core::marker::Copy for SignatureRef {}
            #[automatically_derived]
            impl ::core::marker::StructuralPartialEq for SignatureRef {}
            #[automatically_derived]
            impl ::core::cmp::PartialEq for SignatureRef {
                #[inline]
                fn eq(&self, other: &SignatureRef) -> bool {
                    self.0 == other.0
                }
            }
            #[automatically_derived]
            impl ::core::cmp::PartialOrd for SignatureRef {
                #[inline]
                fn partial_cmp(
                    &self,
                    other: &SignatureRef,
                ) -> ::core::option::Option<::core::cmp::Ordering> {
                    ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Eq for SignatureRef {
                #[inline]
                #[doc(hidden)]
                #[coverage(off)]
                fn assert_fields_are_eq(&self) {
                    let _: ::core::cmp::AssertParamIsEq<usize>;
                }
            }
            #[automatically_derived]
            impl ::core::cmp::Ord for SignatureRef {
                #[inline]
                fn cmp(&self, other: &SignatureRef) -> ::core::cmp::Ordering {
                    ::core::cmp::Ord::cmp(&self.0, &other.0)
                }
            }
            #[automatically_derived]
            impl ::core::hash::Hash for SignatureRef {
                #[inline]
                fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                    ::core::hash::Hash::hash(&self.0, state)
                }
            }
            pub(crate) mod internal {
                use crate::{
                    StringStore,
                    mir::{
                        Module,
                        value::{
                            ValueType, ValueTypeRef,
                            consts::{D64, F32, I8, I16, I32, I64, U8, U16, U32, U64},
                            sig::Signature,
                        },
                    },
                };
                use std::fmt::Formatter;
                impl Signature {
                    pub(crate) fn print<T: StringStore>(
                        &self,
                        idx: usize,
                        module: &Module<T>,
                        f: &mut Formatter,
                    ) -> std::fmt::Result {
                        f.write_fmt(format_args!("  sig #{0}(", idx + 1))?;
                        if let Some(args) = self.args {
                            let ty = module
                                .type_data(args)
                                .expect(
                                    "Invariant violation : Module has a dangling pointer to a variable",
                                );
                            match ty {
                                ValueType::Composite { composition, .. } => {
                                    let mut first = true;
                                    for comp in composition.as_ref() {
                                        if first {
                                            first = false;
                                        } else {
                                            f.write_fmt(format_args!(","))?;
                                        }
                                        let mt = match comp {
                                            &I64 => "i64",
                                            &U64 => "u64",
                                            &I32 => "i32",
                                            &U32 => "u32",
                                            &I16 => "i16",
                                            &U16 => "u16",
                                            &I8 => "i8",
                                            &U8 => "u8",
                                            &F32 => "f32",
                                            &D64 => "f64",
                                            &ValueTypeRef(rf) => {
                                                let rf = rf.get();
                                                f.write_fmt(format_args!(" #{0}", rf))?;
                                                ""
                                            }
                                        };
                                        if !mt.is_empty() {
                                            f.write_fmt(format_args!(" {0}", mt))?
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        f.write_fmt(format_args!(" ) -> ("))?;
                        if let Some(ret) = self.returns {
                            let mt = match &ret {
                                &I64 => "i64",
                                &U64 => "u64",
                                &I32 => "i32",
                                &U32 => "u32",
                                &I16 => "i16",
                                &U16 => "u16",
                                &I8 => "i8",
                                &U8 => "u8",
                                &F32 => "f32",
                                &D64 => "f64",
                                &ValueTypeRef(rf) => {
                                    let rf = rf.get();
                                    f.write_fmt(format_args!("#{0}", rf))?;
                                    ""
                                }
                            };
                            if !mt.is_empty() {
                                f.write_fmt(format_args!("{0}", mt))?
                            }
                        }
                        f.write_fmt(format_args!(")\n"))?;
                        Ok(())
                    }
                }
            }
        }
        #[repr(transparent)]
        pub struct Value(NonZeroUsize);
        #[automatically_derived]
        impl ::core::fmt::Debug for Value {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(f, "Value", &&self.0)
            }
        }
        #[automatically_derived]
        #[doc(hidden)]
        unsafe impl ::core::clone::TrivialClone for Value {}
        #[automatically_derived]
        impl ::core::clone::Clone for Value {
            #[inline]
            fn clone(&self) -> Value {
                let _: ::core::clone::AssertParamIsClone<NonZeroUsize>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for Value {}
        #[automatically_derived]
        impl ::core::hash::Hash for Value {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                ::core::hash::Hash::hash(&self.0, state)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Value {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Value {
            #[inline]
            fn eq(&self, other: &Value) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Value {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_fields_are_eq(&self) {
                let _: ::core::cmp::AssertParamIsEq<NonZeroUsize>;
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for Value {
            #[inline]
            fn partial_cmp(
                &self,
                other: &Value,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Ord for Value {
            #[inline]
            fn cmp(&self, other: &Value) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
        pub struct ValueMeta {}
        #[repr(align(1))]
        pub enum ValueType<'a> {
            Base {
                base: BaseType,
                #[allow(private_interfaces)]
                _uninstantiable: PhantomData<CoreType>,
            },
            Vector { base: BaseType, count: u8 },
            /// The size and alignment of the largest type is respected!
            PrimaryUnion {
                composition: [BaseType; 36],
                count: u8,
                align: Option<Alignment>,
            },
            Union { composition: ValueTypeArray<'a>, align: Option<Alignment> },
            /// Padding is automatically inserted!
            PrimaryComposite {
                composition: [BaseType; 36],
                count: u8,
                align: Option<Alignment>,
            },
            /// Padding is automatically inserted!
            Composite { composition: ValueTypeArray<'a>, align: Option<Alignment> },
        }
        #[automatically_derived]
        impl<'a> ::core::clone::Clone for ValueType<'a> {
            #[inline]
            fn clone(&self) -> ValueType<'a> {
                match self {
                    ValueType::Base { base: __self_0, _uninstantiable: __self_1 } => {
                        ValueType::Base {
                            base: ::core::clone::Clone::clone(__self_0),
                            _uninstantiable: ::core::clone::Clone::clone(__self_1),
                        }
                    }
                    ValueType::Vector { base: __self_0, count: __self_1 } => {
                        ValueType::Vector {
                            base: ::core::clone::Clone::clone(__self_0),
                            count: ::core::clone::Clone::clone(__self_1),
                        }
                    }
                    ValueType::PrimaryUnion {
                        composition: __self_0,
                        count: __self_1,
                        align: __self_2,
                    } => {
                        ValueType::PrimaryUnion {
                            composition: ::core::clone::Clone::clone(__self_0),
                            count: ::core::clone::Clone::clone(__self_1),
                            align: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                    ValueType::Union { composition: __self_0, align: __self_1 } => {
                        ValueType::Union {
                            composition: ::core::clone::Clone::clone(__self_0),
                            align: ::core::clone::Clone::clone(__self_1),
                        }
                    }
                    ValueType::PrimaryComposite {
                        composition: __self_0,
                        count: __self_1,
                        align: __self_2,
                    } => {
                        ValueType::PrimaryComposite {
                            composition: ::core::clone::Clone::clone(__self_0),
                            count: ::core::clone::Clone::clone(__self_1),
                            align: ::core::clone::Clone::clone(__self_2),
                        }
                    }
                    ValueType::Composite { composition: __self_0, align: __self_1 } => {
                        ValueType::Composite {
                            composition: ::core::clone::Clone::clone(__self_0),
                            align: ::core::clone::Clone::clone(__self_1),
                        }
                    }
                }
            }
        }
        #[automatically_derived]
        impl<'a> ::core::hash::Hash for ValueType<'a> {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                ::core::hash::Hash::hash(&__self_discr, state);
                match self {
                    ValueType::Base { base: __self_0, _uninstantiable: __self_1 } => {
                        ::core::hash::Hash::hash(__self_0, state);
                        ::core::hash::Hash::hash(__self_1, state)
                    }
                    ValueType::Vector { base: __self_0, count: __self_1 } => {
                        ::core::hash::Hash::hash(__self_0, state);
                        ::core::hash::Hash::hash(__self_1, state)
                    }
                    ValueType::PrimaryUnion {
                        composition: __self_0,
                        count: __self_1,
                        align: __self_2,
                    } => {
                        ::core::hash::Hash::hash(__self_0, state);
                        ::core::hash::Hash::hash(__self_1, state);
                        ::core::hash::Hash::hash(__self_2, state)
                    }
                    ValueType::Union { composition: __self_0, align: __self_1 } => {
                        ::core::hash::Hash::hash(__self_0, state);
                        ::core::hash::Hash::hash(__self_1, state)
                    }
                    ValueType::PrimaryComposite {
                        composition: __self_0,
                        count: __self_1,
                        align: __self_2,
                    } => {
                        ::core::hash::Hash::hash(__self_0, state);
                        ::core::hash::Hash::hash(__self_1, state);
                        ::core::hash::Hash::hash(__self_2, state)
                    }
                    ValueType::Composite { composition: __self_0, align: __self_1 } => {
                        ::core::hash::Hash::hash(__self_0, state);
                        ::core::hash::Hash::hash(__self_1, state)
                    }
                }
            }
        }
        #[automatically_derived]
        impl<'a> ::core::marker::StructuralPartialEq for ValueType<'a> {}
        #[automatically_derived]
        impl<'a> ::core::cmp::PartialEq for ValueType<'a> {
            #[inline]
            fn eq(&self, other: &ValueType<'a>) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
                    && match (self, other) {
                        (
                            ValueType::Base {
                                base: __self_0,
                                _uninstantiable: __self_1,
                            },
                            ValueType::Base { base: __arg1_0, _uninstantiable: __arg1_1 },
                        ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                        (
                            ValueType::Vector { base: __self_0, count: __self_1 },
                            ValueType::Vector { base: __arg1_0, count: __arg1_1 },
                        ) => __self_1 == __arg1_1 && __self_0 == __arg1_0,
                        (
                            ValueType::PrimaryUnion {
                                composition: __self_0,
                                count: __self_1,
                                align: __self_2,
                            },
                            ValueType::PrimaryUnion {
                                composition: __arg1_0,
                                count: __arg1_1,
                                align: __arg1_2,
                            },
                        ) => {
                            __self_1 == __arg1_1 && __self_0 == __arg1_0
                                && __self_2 == __arg1_2
                        }
                        (
                            ValueType::Union { composition: __self_0, align: __self_1 },
                            ValueType::Union { composition: __arg1_0, align: __arg1_1 },
                        ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                        (
                            ValueType::PrimaryComposite {
                                composition: __self_0,
                                count: __self_1,
                                align: __self_2,
                            },
                            ValueType::PrimaryComposite {
                                composition: __arg1_0,
                                count: __arg1_1,
                                align: __arg1_2,
                            },
                        ) => {
                            __self_1 == __arg1_1 && __self_0 == __arg1_0
                                && __self_2 == __arg1_2
                        }
                        (
                            ValueType::Composite {
                                composition: __self_0,
                                align: __self_1,
                            },
                            ValueType::Composite {
                                composition: __arg1_0,
                                align: __arg1_1,
                            },
                        ) => __self_0 == __arg1_0 && __self_1 == __arg1_1,
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
            }
        }
        #[automatically_derived]
        impl<'a> ::core::cmp::Eq for ValueType<'a> {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_fields_are_eq(&self) {
                let _: ::core::cmp::AssertParamIsEq<BaseType>;
                let _: ::core::cmp::AssertParamIsEq<PhantomData<CoreType>>;
                let _: ::core::cmp::AssertParamIsEq<u8>;
                let _: ::core::cmp::AssertParamIsEq<[BaseType; 36]>;
                let _: ::core::cmp::AssertParamIsEq<Option<Alignment>>;
                let _: ::core::cmp::AssertParamIsEq<ValueTypeArray<'a>>;
                let _: ::core::cmp::AssertParamIsEq<Option<Alignment>>;
                let _: ::core::cmp::AssertParamIsEq<[BaseType; 36]>;
                let _: ::core::cmp::AssertParamIsEq<Option<Alignment>>;
                let _: ::core::cmp::AssertParamIsEq<ValueTypeArray<'a>>;
                let _: ::core::cmp::AssertParamIsEq<Option<Alignment>>;
            }
        }
        #[automatically_derived]
        impl<'a> ::core::cmp::PartialOrd for ValueType<'a> {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ValueType<'a>,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }
        #[automatically_derived]
        impl<'a> ::core::cmp::Ord for ValueType<'a> {
            #[inline]
            fn cmp(&self, other: &ValueType<'a>) -> ::core::cmp::Ordering {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                match ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr) {
                    ::core::cmp::Ordering::Equal => {
                        match (self, other) {
                            (
                                ValueType::Base {
                                    base: __self_0,
                                    _uninstantiable: __self_1,
                                },
                                ValueType::Base {
                                    base: __arg1_0,
                                    _uninstantiable: __arg1_1,
                                },
                            ) => {
                                match ::core::cmp::Ord::cmp(__self_0, __arg1_0) {
                                    ::core::cmp::Ordering::Equal => {
                                        ::core::cmp::Ord::cmp(__self_1, __arg1_1)
                                    }
                                    cmp => cmp,
                                }
                            }
                            (
                                ValueType::Vector { base: __self_0, count: __self_1 },
                                ValueType::Vector { base: __arg1_0, count: __arg1_1 },
                            ) => {
                                match ::core::cmp::Ord::cmp(__self_0, __arg1_0) {
                                    ::core::cmp::Ordering::Equal => {
                                        ::core::cmp::Ord::cmp(__self_1, __arg1_1)
                                    }
                                    cmp => cmp,
                                }
                            }
                            (
                                ValueType::PrimaryUnion {
                                    composition: __self_0,
                                    count: __self_1,
                                    align: __self_2,
                                },
                                ValueType::PrimaryUnion {
                                    composition: __arg1_0,
                                    count: __arg1_1,
                                    align: __arg1_2,
                                },
                            ) => {
                                match ::core::cmp::Ord::cmp(__self_0, __arg1_0) {
                                    ::core::cmp::Ordering::Equal => {
                                        match ::core::cmp::Ord::cmp(__self_1, __arg1_1) {
                                            ::core::cmp::Ordering::Equal => {
                                                ::core::cmp::Ord::cmp(__self_2, __arg1_2)
                                            }
                                            cmp => cmp,
                                        }
                                    }
                                    cmp => cmp,
                                }
                            }
                            (
                                ValueType::Union { composition: __self_0, align: __self_1 },
                                ValueType::Union { composition: __arg1_0, align: __arg1_1 },
                            ) => {
                                match ::core::cmp::Ord::cmp(__self_0, __arg1_0) {
                                    ::core::cmp::Ordering::Equal => {
                                        ::core::cmp::Ord::cmp(__self_1, __arg1_1)
                                    }
                                    cmp => cmp,
                                }
                            }
                            (
                                ValueType::PrimaryComposite {
                                    composition: __self_0,
                                    count: __self_1,
                                    align: __self_2,
                                },
                                ValueType::PrimaryComposite {
                                    composition: __arg1_0,
                                    count: __arg1_1,
                                    align: __arg1_2,
                                },
                            ) => {
                                match ::core::cmp::Ord::cmp(__self_0, __arg1_0) {
                                    ::core::cmp::Ordering::Equal => {
                                        match ::core::cmp::Ord::cmp(__self_1, __arg1_1) {
                                            ::core::cmp::Ordering::Equal => {
                                                ::core::cmp::Ord::cmp(__self_2, __arg1_2)
                                            }
                                            cmp => cmp,
                                        }
                                    }
                                    cmp => cmp,
                                }
                            }
                            (
                                ValueType::Composite {
                                    composition: __self_0,
                                    align: __self_1,
                                },
                                ValueType::Composite {
                                    composition: __arg1_0,
                                    align: __arg1_1,
                                },
                            ) => {
                                match ::core::cmp::Ord::cmp(__self_0, __arg1_0) {
                                    ::core::cmp::Ordering::Equal => {
                                        ::core::cmp::Ord::cmp(__self_1, __arg1_1)
                                    }
                                    cmp => cmp,
                                }
                            }
                            _ => unsafe { ::core::intrinsics::unreachable() }
                        }
                    }
                    cmp => cmp,
                }
            }
        }
        impl<'a> ValueType<'a> {
            pub fn is_num(&self) -> bool {
                #[allow(non_exhaustive_omitted_patterns)]
                match self {
                    Self::Base { .. } | Self::Vector { .. } => true,
                    _ => false,
                }
            }
            pub fn is_vector(&self) -> bool {
                #[allow(non_exhaustive_omitted_patterns)]
                match self {
                    Self::Vector { .. } => true,
                    _ => false,
                }
            }
            pub fn is_scalar(&self) -> bool {
                !self.is_vector()
            }
            pub fn is_int(&self) -> bool {
                self.is_num() && !self.is_float()
            }
            pub fn is_float(&self) -> bool {
                match self {
                    Self::Base { base, .. } | Self::Vector { base, .. } => {
                        #[allow(non_exhaustive_omitted_patterns)]
                        match base {
                            BaseType::Float32 | BaseType::Double64 => true,
                            _ => false,
                        }
                    }
                    _ => false,
                }
            }
        }
        #[repr(align(1))]
        pub enum ValueTypeArray<'a> {
            Rc(Rc<[ValueTypeRef]>),
            Slice(&'a [ValueTypeRef]),
        }
        #[automatically_derived]
        impl<'a> ::core::clone::Clone for ValueTypeArray<'a> {
            #[inline]
            fn clone(&self) -> ValueTypeArray<'a> {
                match self {
                    ValueTypeArray::Rc(__self_0) => {
                        ValueTypeArray::Rc(::core::clone::Clone::clone(__self_0))
                    }
                    ValueTypeArray::Slice(__self_0) => {
                        ValueTypeArray::Slice(::core::clone::Clone::clone(__self_0))
                    }
                }
            }
        }
        #[automatically_derived]
        impl<'a> ::core::hash::Hash for ValueTypeArray<'a> {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                ::core::hash::Hash::hash(&__self_discr, state);
                match self {
                    ValueTypeArray::Rc(__self_0) => {
                        ::core::hash::Hash::hash(__self_0, state)
                    }
                    ValueTypeArray::Slice(__self_0) => {
                        ::core::hash::Hash::hash(__self_0, state)
                    }
                }
            }
        }
        #[automatically_derived]
        impl<'a> ::core::marker::StructuralPartialEq for ValueTypeArray<'a> {}
        #[automatically_derived]
        impl<'a> ::core::cmp::PartialEq for ValueTypeArray<'a> {
            #[inline]
            fn eq(&self, other: &ValueTypeArray<'a>) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
                    && match (self, other) {
                        (ValueTypeArray::Rc(__self_0), ValueTypeArray::Rc(__arg1_0)) => {
                            __self_0 == __arg1_0
                        }
                        (
                            ValueTypeArray::Slice(__self_0),
                            ValueTypeArray::Slice(__arg1_0),
                        ) => __self_0 == __arg1_0,
                        _ => unsafe { ::core::intrinsics::unreachable() }
                    }
            }
        }
        #[automatically_derived]
        impl<'a> ::core::cmp::Eq for ValueTypeArray<'a> {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_fields_are_eq(&self) {
                let _: ::core::cmp::AssertParamIsEq<Rc<[ValueTypeRef]>>;
                let _: ::core::cmp::AssertParamIsEq<&'a [ValueTypeRef]>;
            }
        }
        #[automatically_derived]
        impl<'a> ::core::cmp::PartialOrd for ValueTypeArray<'a> {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ValueTypeArray<'a>,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }
        #[automatically_derived]
        impl<'a> ::core::cmp::Ord for ValueTypeArray<'a> {
            #[inline]
            fn cmp(&self, other: &ValueTypeArray<'a>) -> ::core::cmp::Ordering {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                match ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr) {
                    ::core::cmp::Ordering::Equal => {
                        match (self, other) {
                            (
                                ValueTypeArray::Rc(__self_0),
                                ValueTypeArray::Rc(__arg1_0),
                            ) => ::core::cmp::Ord::cmp(__self_0, __arg1_0),
                            (
                                ValueTypeArray::Slice(__self_0),
                                ValueTypeArray::Slice(__arg1_0),
                            ) => ::core::cmp::Ord::cmp(__self_0, __arg1_0),
                            _ => unsafe { ::core::intrinsics::unreachable() }
                        }
                    }
                    cmp => cmp,
                }
            }
        }
        impl<'a> AsRef<[ValueTypeRef]> for ValueTypeArray<'a> {
            fn as_ref(&self) -> &[ValueTypeRef] {
                match self {
                    Self::Rc(x) => x.as_ref(),
                    Self::Slice(x) => x,
                }
            }
        }
        impl<'a> Deref for ValueTypeArray<'a> {
            type Target = [ValueTypeRef];
            fn deref(&self) -> &Self::Target {
                self.as_ref()
            }
        }
        pub enum Alignment {
            B1,
            B2,
            B4,
            B8,
            B16,
            B32,
            B64,
            B128,
            B256,
            B512,
            B1024,
            B2048,
            B4096,
            B8192,
            B16384,
            B32768,
            B65536,
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for Alignment {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(
                    f,
                    match self {
                        Alignment::B1 => "B1",
                        Alignment::B2 => "B2",
                        Alignment::B4 => "B4",
                        Alignment::B8 => "B8",
                        Alignment::B16 => "B16",
                        Alignment::B32 => "B32",
                        Alignment::B64 => "B64",
                        Alignment::B128 => "B128",
                        Alignment::B256 => "B256",
                        Alignment::B512 => "B512",
                        Alignment::B1024 => "B1024",
                        Alignment::B2048 => "B2048",
                        Alignment::B4096 => "B4096",
                        Alignment::B8192 => "B8192",
                        Alignment::B16384 => "B16384",
                        Alignment::B32768 => "B32768",
                        Alignment::B65536 => "B65536",
                    },
                )
            }
        }
        #[automatically_derived]
        #[doc(hidden)]
        unsafe impl ::core::clone::TrivialClone for Alignment {}
        #[automatically_derived]
        impl ::core::clone::Clone for Alignment {
            #[inline]
            fn clone(&self) -> Alignment {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for Alignment {}
        #[automatically_derived]
        impl ::core::hash::Hash for Alignment {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                ::core::hash::Hash::hash(&__self_discr, state)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for Alignment {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for Alignment {
            #[inline]
            fn eq(&self, other: &Alignment) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for Alignment {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_fields_are_eq(&self) {}
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for Alignment {
            #[inline]
            fn partial_cmp(
                &self,
                other: &Alignment,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Ord for Alignment {
            #[inline]
            fn cmp(&self, other: &Alignment) -> ::core::cmp::Ordering {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr)
            }
        }
        impl Alignment {
            pub fn align(self) -> usize {
                match self {
                    Self::B1 => 1,
                    Self::B2 => 2,
                    Self::B4 => 4,
                    Self::B8 => 8,
                    Self::B16 => 16,
                    Self::B32 => 32,
                    Self::B64 => 64,
                    Self::B128 => 128,
                    Self::B256 => 256,
                    Self::B512 => 512,
                    Self::B1024 => 1024,
                    Self::B2048 => 2048,
                    Self::B4096 => 4096,
                    Self::B8192 => 8192,
                    Self::B16384 => 16384,
                    Self::B32768 => 32768,
                    Self::B65536 => 65536,
                }
            }
            pub const fn parse(data: usize) -> Alignment {
                match data {
                    1 => Self::B1,
                    2 => Self::B2,
                    4 => Self::B4,
                    8 => Self::B8,
                    16 => Self::B16,
                    32 => Self::B32,
                    64 => Self::B64,
                    128 => Self::B128,
                    256 => Self::B256,
                    512 => Self::B512,
                    1024 => Self::B1024,
                    2048 => Self::B2048,
                    4096 => Self::B4096,
                    8192 => Self::B8192,
                    16384 => Self::B16384,
                    32768 => Self::B32768,
                    65536 => Self::B65536,
                    _ => {
                        ::core::panicking::panic_fmt(
                            format_args!("Could not correctly construct alignment"),
                        );
                    }
                }
            }
        }
        pub(crate) struct CoreType;
        pub struct ValueTypeRef(pub(crate) NonZeroUsize);
        #[automatically_derived]
        impl ::core::fmt::Debug for ValueTypeRef {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "ValueTypeRef",
                    &&self.0,
                )
            }
        }
        #[automatically_derived]
        #[doc(hidden)]
        unsafe impl ::core::clone::TrivialClone for ValueTypeRef {}
        #[automatically_derived]
        impl ::core::clone::Clone for ValueTypeRef {
            #[inline]
            fn clone(&self) -> ValueTypeRef {
                let _: ::core::clone::AssertParamIsClone<NonZeroUsize>;
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for ValueTypeRef {}
        #[automatically_derived]
        impl ::core::hash::Hash for ValueTypeRef {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                ::core::hash::Hash::hash(&self.0, state)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for ValueTypeRef {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for ValueTypeRef {
            #[inline]
            fn eq(&self, other: &ValueTypeRef) -> bool {
                self.0 == other.0
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for ValueTypeRef {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_fields_are_eq(&self) {
                let _: ::core::cmp::AssertParamIsEq<NonZeroUsize>;
            }
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for ValueTypeRef {
            #[inline]
            fn partial_cmp(
                &self,
                other: &ValueTypeRef,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Ord for ValueTypeRef {
            #[inline]
            fn cmp(&self, other: &ValueTypeRef) -> ::core::cmp::Ordering {
                ::core::cmp::Ord::cmp(&self.0, &other.0)
            }
        }
        impl ValueTypeRef {
            pub fn index(self) -> usize {
                self.0.get().sub(1)
            }
        }
        pub enum BaseType {
            Int64,
            Int32,
            Int16,
            Int8,
            UInt64,
            UInt32,
            UInt16,
            UInt8,
            Double64,
            Float32,
        }
        #[automatically_derived]
        impl ::core::fmt::Debug for BaseType {
            #[inline]
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                ::core::fmt::Formatter::write_str(
                    f,
                    match self {
                        BaseType::Int64 => "Int64",
                        BaseType::Int32 => "Int32",
                        BaseType::Int16 => "Int16",
                        BaseType::Int8 => "Int8",
                        BaseType::UInt64 => "UInt64",
                        BaseType::UInt32 => "UInt32",
                        BaseType::UInt16 => "UInt16",
                        BaseType::UInt8 => "UInt8",
                        BaseType::Double64 => "Double64",
                        BaseType::Float32 => "Float32",
                    },
                )
            }
        }
        #[automatically_derived]
        #[doc(hidden)]
        unsafe impl ::core::clone::TrivialClone for BaseType {}
        #[automatically_derived]
        impl ::core::clone::Clone for BaseType {
            #[inline]
            fn clone(&self) -> BaseType {
                *self
            }
        }
        #[automatically_derived]
        impl ::core::marker::Copy for BaseType {}
        #[automatically_derived]
        impl ::core::hash::Hash for BaseType {
            #[inline]
            fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                ::core::hash::Hash::hash(&__self_discr, state)
            }
        }
        #[automatically_derived]
        impl ::core::marker::StructuralPartialEq for BaseType {}
        #[automatically_derived]
        impl ::core::cmp::PartialEq for BaseType {
            #[inline]
            fn eq(&self, other: &BaseType) -> bool {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                __self_discr == __arg1_discr
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Eq for BaseType {
            #[inline]
            #[doc(hidden)]
            #[coverage(off)]
            fn assert_fields_are_eq(&self) {}
        }
        #[automatically_derived]
        impl ::core::cmp::PartialOrd for BaseType {
            #[inline]
            fn partial_cmp(
                &self,
                other: &BaseType,
            ) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }
        #[automatically_derived]
        impl ::core::cmp::Ord for BaseType {
            #[inline]
            fn cmp(&self, other: &BaseType) -> ::core::cmp::Ordering {
                let __self_discr = ::core::intrinsics::discriminant_value(self);
                let __arg1_discr = ::core::intrinsics::discriminant_value(other);
                ::core::cmp::Ord::cmp(&__self_discr, &__arg1_discr)
            }
        }
        pub mod consts {
            use crate::mir::value::ValueTypeRef;
            use std::num::NonZeroUsize;
            /// Literally an Alias to [I64]
            pub const PTR: ValueTypeRef = I64;
            pub const I64: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(1)
            });
            pub const I32: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(2)
            });
            pub const I16: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(3)
            });
            pub const I8: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(4)
            });
            pub const U64: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(5)
            });
            pub const U32: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(6)
            });
            pub const U16: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(7)
            });
            pub const U8: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(8)
            });
            pub const D64: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(9)
            });
            pub const F32: ValueTypeRef = ValueTypeRef(unsafe {
                NonZeroUsize::new_unchecked(10)
            });
        }
        pub(crate) mod internal {
            use crate::{StringStore, mir::{Module, value::{BaseType, ValueType}}};
            use std::fmt::Formatter;
            impl BaseType {
                pub(crate) fn format(self, f: &mut Formatter) -> std::fmt::Result {
                    f.write_fmt(
                        format_args!(
                            "@{0}",
                            match self {
                                Self::Int64 => "i64",
                                Self::Int32 => "i32",
                                Self::Int16 => "i16",
                                Self::Int8 => "i8",
                                Self::UInt64 => "u64",
                                Self::UInt32 => "u32",
                                Self::UInt16 => "u16",
                                Self::UInt8 => "u8",
                                Self::Float32 => "f32",
                                Self::Double64 => "d64",
                            },
                        ),
                    )
                }
            }
            impl<'a> ValueType<'a> {
                pub(crate) fn fmt<T: StringStore>(
                    &self,
                    id: usize,
                    store: &Module<T>,
                    f: &mut Formatter,
                ) -> std::fmt::Result {
                    f.write_fmt(
                        format_args!(
                            "  type #{0}({1}, {2}) = ",
                            id,
                            self.size(store),
                            self.align(store),
                        ),
                    )?;
                    match self {
                        Self::Base { base, .. } => {
                            base.format(f)?;
                            f.write_fmt(format_args!("\n"))?;
                        }
                        &Self::PrimaryUnion { ref composition, count, .. } => {
                            f.write_fmt(format_args!("union {{\n"))?;
                            for item in &composition[0..(count as usize)] {
                                f.write_fmt(format_args!("    "))?;
                                item.format(f)?;
                                f.write_fmt(format_args!("\n"))?;
                            }
                            f.write_fmt(format_args!("  }}\n"))?;
                        }
                        &Self::PrimaryComposite { ref composition, count, .. } => {
                            f.write_fmt(format_args!("struct {{\n"))?;
                            for item in &composition.as_ref()[0..(count as usize)] {
                                f.write_fmt(format_args!("    "))?;
                                item.format(f)?;
                                f.write_fmt(format_args!("\n"))?;
                            }
                            f.write_fmt(format_args!("  }}\n"))?;
                        }
                        Self::Vector { base, count } => {
                            f.write_fmt(format_args!("vector <"))?;
                            base.format(f)?;
                            f.write_fmt(format_args!(" x {0}>\n", count))?;
                        }
                        Self::Union { composition, .. } => {
                            f.write_fmt(format_args!("union {{\n"))?;
                            for &vtr in composition.as_ref() {
                                f.write_fmt(format_args!("    "))?;
                                if let Some(x) = store.type_data(vtr) {
                                    match x {
                                        ValueType::Base { base, .. } => base.format(f)?,
                                        _ => f.write_fmt(format_args!("@type:{0}", vtr.0))?,
                                    }
                                    f.write_fmt(format_args!("\n"))?;
                                } else {
                                    f.write_fmt(format_args!("<error>\n"))?;
                                }
                            }
                            f.write_fmt(format_args!("  }}\n"))?;
                        }
                        Self::Composite { composition, .. } => {
                            f.write_fmt(format_args!("struct {{\n"))?;
                            for &vtr in composition.as_ref() {
                                f.write_fmt(format_args!("    "))?;
                                if let Some(x) = store.type_data(vtr) {
                                    match x {
                                        ValueType::Base { base, .. } => base.format(f)?,
                                        _ => f.write_fmt(format_args!("@type:{0}", vtr.0))?,
                                    }
                                    f.write_fmt(format_args!("\n"))?;
                                } else {
                                    f.write_fmt(format_args!("<error>\n"))?;
                                }
                            }
                            f.write_fmt(format_args!("  }}\n"))?;
                        }
                    }
                    Ok(())
                }
            }
        }
    }
    pub struct Module<'a, T: StringStore> {
        pub store: &'a T,
        imports: HashMap<StringRef<'a, T>, SignatureRef, rapidhash::fast::RandomState>,
        exports: Vec<StringRef<'a, T>>,
        typemap: Vec<ValueType<'a>>,
        sigs: Vec<Signature>,
        functions: HashMap<
            StringRef<'a, T>,
            function::Function<'a, T>,
            rapidhash::fast::RandomState,
        >,
        name: StringRef<'a, T>,
        pub arch: &'a dyn TargetVM<T = T>,
    }
    impl<'a, T: StringStore> Module<'a, T> {
        pub fn new(store: &'a T, name: &str, arch: &'a dyn TargetVM<T = T>) -> Self {
            let mut typemap: Vec<ValueType> = [
                BaseType::Int64,
                BaseType::Int32,
                BaseType::Int16,
                BaseType::Int8,
                BaseType::UInt64,
                BaseType::UInt32,
                BaseType::UInt16,
                BaseType::UInt8,
                BaseType::Double64,
                BaseType::Float32,
            ]
                .into_iter()
                .map(|base| ValueType::Base {
                    base,
                    _uninstantiable: PhantomData,
                })
                .collect();
            typemap.reserve(64);
            Self {
                name: store.matchval(name),
                store,
                typemap,
                sigs: Vec::with_capacity(8),
                imports: RapidHashMap::with_capacity(32),
                exports: Vec::with_capacity(32),
                functions: Default::default(),
                arch,
            }
        }
        pub fn import(&mut self, symbol: &str, sig: SignatureRef) {
            _ = self.imports.insert(self.store.matchval(symbol), sig);
        }
        pub fn signature(&mut self, sig: Signature) -> SignatureRef {
            let idx = self.sigs.len();
            self.sigs.push(sig);
            SignatureRef(idx)
        }
        /// Creates a new Function structure
        ///
        /// The created function is NOT added to this module's functions list
        /// to do that use [Module::add_function]
        pub fn function(&mut self, name: &str, sig: SignatureRef) -> Function<'a, T> {
            let name = self.store.matchval(name);
            Function::new(self.store, name, sig)
        }
        /// Adds the function or returns None if there is a name collision
        pub fn add_function(&mut self, f: Function<'a, T>) -> Option<()> {
            if self.functions.contains_key(&f.name) {
                return None;
            }
            _ = self.functions.insert(f.name, f);
            Some(())
        }
        /// ## Please Note:
        /// Inserting Duplicate ValueType will **NOT** merge them into the same
        /// [ValueTypeRef] and worse than that - our verifier will **NOT** treat
        /// the two as equal types.
        pub fn insert_type(&mut self, t: ValueType<'a>) -> ValueTypeRef {
            self.typemap.push(t);
            unsafe { ValueTypeRef(NonZeroUsize::new_unchecked(self.typemap.len())) }
        }
        pub fn type_data(&self, id: ValueTypeRef) -> Option<&ValueType<'_>> {
            self.typemap.get(id.index())
        }
        /// Mark the function symbol to be exported
        ///
        /// This does not check if the function with the symbol currently exists or not
        pub fn export_fn(&mut self, symbol: &str) {
            self.exports.push(self.store.matchval(symbol));
        }
        pub fn name(&self) -> StringRef<'a, T> {
            self.name
        }
        pub fn functions(
            &self,
        ) -> &HashMap<
            StringRef<'a, T>,
            function::Function<'a, T>,
            rapidhash::fast::RandomState,
        > {
            &self.functions
        }
        pub fn imports(
            &self,
        ) -> impl Iterator<Item = (&StringRef<'a, T>, &SignatureRef)> {
            self.imports.iter()
        }
        pub fn exports(&self) -> &[StringRef<'a, T>] {
            &self.exports
        }
    }
    impl<'a, T: StringStore> Debug for Module<'a, T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!("; target=\"{0:?}\"\n", self.arch))?;
            f.write_fmt(
                format_args!(
                    "define module {0} {{\n",
                    self.store.resolve(self.name).as_ref(),
                ),
            )?;
            f.write_fmt(format_args!("; Types (size, align)\n"))?;
            for (id, valtype) in self.typemap.iter().enumerate() {
                valtype.fmt(id + 1, self, f)?;
            }
            f.write_fmt(format_args!("\n"))?;
            f.write_fmt(format_args!("; Signatures\n"))?;
            for (idx, sig) in self.sigs.iter().enumerate() {
                sig.print(idx, self, f)?;
            }
            f.write_fmt(format_args!("\n"))?;
            f.write_fmt(format_args!("; Imports & Exports\n"))?;
            for (&import, a) in self.imports.iter() {
                f.write_fmt(
                    format_args!(
                        "  #import {0} (@sig:#{1})\n",
                        self.store.resolve(import).as_ref(),
                        a.0 + 1,
                    ),
                )?;
            }
            for &export in &self.exports {
                f.write_fmt(
                    format_args!("  #export {0}\n", self.store.resolve(export).as_ref()),
                )?;
            }
            f.write_fmt(format_args!("\n"))?;
            f.write_fmt(format_args!("; Functions\n"))?;
            for (_, function) in &self.functions {
                function.print(f, self)?;
            }
            f.write_fmt(format_args!("\n"))?;
            f.write_fmt(format_args!("}}\n"))?;
            Ok(())
        }
    }
}
pub mod saemit {
    pub mod machine {
        use crate::{
            StringStore, llir::instr::loc::VMLoc,
            mir::{
                Module, block::instr::loc::LocSrc, function::{Function, ssa::ValueId},
                value::{Alignment, ValueType, ValueTypeArray, ValueTypeRef},
            },
        };
        use std::{fmt::Debug, rc::Rc};
        pub mod v0 {
            use std::{fmt::Debug, marker::PhantomData};
            use crate::{
                StringStore, mir::{Module, function::Function},
                saemit::machine::{TargetVM, sabi_map, v0::blockalloc::BlockAlloc},
            };
            mod blockalloc {
                #![allow(dead_code, unused)]
                use crate::mir::function::ssa::ValueId;
                pub struct BlockAlloc {
                    r1: Reg,
                    r2: Reg,
                    r3: Reg,
                    r4: Reg,
                    r5: Reg,
                    r6: Reg,
                    r7: Reg,
                    r8: Reg,
                }
                #[automatically_derived]
                impl ::core::fmt::Debug for BlockAlloc {
                    #[inline]
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        let names: &'static _ = &[
                            "r1",
                            "r2",
                            "r3",
                            "r4",
                            "r5",
                            "r6",
                            "r7",
                            "r8",
                        ];
                        let values: &[&dyn ::core::fmt::Debug] = &[
                            &self.r1,
                            &self.r2,
                            &self.r3,
                            &self.r4,
                            &self.r5,
                            &self.r6,
                            &self.r7,
                            &&self.r8,
                        ];
                        ::core::fmt::Formatter::debug_struct_fields_finish(
                            f,
                            "BlockAlloc",
                            names,
                            values,
                        )
                    }
                }
                #[automatically_derived]
                impl ::core::default::Default for BlockAlloc {
                    #[inline]
                    fn default() -> BlockAlloc {
                        BlockAlloc {
                            r1: ::core::default::Default::default(),
                            r2: ::core::default::Default::default(),
                            r3: ::core::default::Default::default(),
                            r4: ::core::default::Default::default(),
                            r5: ::core::default::Default::default(),
                            r6: ::core::default::Default::default(),
                            r7: ::core::default::Default::default(),
                            r8: ::core::default::Default::default(),
                        }
                    }
                }
                impl BlockAlloc {
                    pub fn clear(&mut self) {
                        for n in [
                            &mut self.r1,
                            &mut self.r2,
                            &mut self.r3,
                            &mut self.r4,
                            &mut self.r5,
                            &mut self.r6,
                            &mut self.r7,
                            &mut self.r8,
                        ] {
                            n.clear();
                        }
                    }
                }
                pub struct Reg {
                    values: Vec<ValueId>,
                }
                #[automatically_derived]
                impl ::core::fmt::Debug for Reg {
                    #[inline]
                    fn fmt(
                        &self,
                        f: &mut ::core::fmt::Formatter,
                    ) -> ::core::fmt::Result {
                        ::core::fmt::Formatter::debug_struct_field1_finish(
                            f,
                            "Reg",
                            "values",
                            &&self.values,
                        )
                    }
                }
                #[automatically_derived]
                impl ::core::default::Default for Reg {
                    #[inline]
                    fn default() -> Reg {
                        Reg {
                            values: ::core::default::Default::default(),
                        }
                    }
                }
                impl Reg {
                    pub fn storable() {}
                    pub fn clear(&mut self) {
                        self.values.clear();
                    }
                }
            }
            /// Targets the Instruction Set Architecture of `v0` of the SaVM Language
            ///
            /// Triple : `savm64le-sa-v0`
            pub struct IsaV0<T: StringStore> {
                _inner: PhantomData<T>,
            }
            impl<T: StringStore> IsaV0<T> {
                pub fn generate() -> Self {
                    Self { _inner: PhantomData }
                }
            }
            impl<T: StringStore> TargetVM for IsaV0<T> {
                type T = T;
                fn regalloc(
                    &self,
                    func: &Function<'_, Self::T>,
                    module: &Module<'_, Self::T>,
                ) {
                    let mut blockalloc = BlockAlloc::default();
                    for block in &func.blocks {
                        blockalloc.clear();
                        let sabimap = sabi_map(&block.params, &func, &module);
                        {
                            ::std::io::_print(format_args!("{0:?}\n", sabimap));
                        };
                    }
                }
            }
            impl<T: StringStore> Debug for IsaV0<T> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(format_args!("savm64le-savm-sabi"))
                }
            }
        }
        pub trait TargetVM: Debug {
            type T: StringStore;
            fn regalloc(
                &self,
                func: &Function<'_, Self::T>,
                module: &Module<'_, Self::T>,
            );
        }
        pub(crate) fn sabi_map<T: StringStore>(
            args: &[ValueId],
            func: &Function<'_, T>,
            module: &Module<'_, T>,
        ) -> Option<LocSrc> {
            let mut output = None;
            let composition: Rc<[ValueTypeRef]> = Rc::from(
                args
                    .iter()
                    .map(|x| unsafe { func.ssa.get_unchecked(x.0) }.typetag)
                    .collect::<Box<[_]>>(),
            );
            let argstruct = ValueType::Composite {
                align: composition
                    .as_ref()
                    .iter()
                    .filter_map(|&x| module.type_data(x).map(|x| x.align(module)))
                    .max()
                    .map(Alignment::parse),
                composition: ValueTypeArray::Rc(composition),
            };
            let modsize = argstruct.size(module);
            match modsize {
                0 => {}
                1..=8 => {
                    output = Some(LocSrc {
                        offset: 0,
                        reg: VMLoc::R7,
                        count: 1,
                        width: 8,
                    });
                }
                9..=16 => {
                    output = Some(LocSrc {
                        offset: 0,
                        reg: VMLoc::R7,
                        count: 2,
                        width: 8,
                    });
                }
                _ => {
                    let size = argstruct.size(module);
                    if size >= 64 {
                        output = Some(LocSrc {
                            offset: 0,
                            reg: VMLoc::Largepad,
                            count: 1,
                            width: size,
                        });
                    } else {
                        output = Some(LocSrc {
                            offset: 0,
                            reg: VMLoc::Scratchpad,
                            count: 1,
                            width: size,
                        });
                    }
                }
            }
            output
        }
    }
}
#[repr(align(64))]
pub struct SingleThreadedStringStore {
    data: UnsafeCell<Vec<Rc<str>>>,
    sets: UnsafeCell<HashMap<Rc<str>, NonZeroUsize, rapidhash::fast::RandomState>>,
}
impl SingleThreadedStringStore {
    pub fn new() -> Self {
        Self {
            data: UnsafeCell::new(Default::default()),
            sets: UnsafeCell::new(Default::default()),
        }
    }
}
impl StringStore for SingleThreadedStringStore {
    type T<'a> = Rc<str> where Self: 'a;
    fn matchval<'a, Data: AsRef<str>>(&'a self, data: Data) -> StringRef<'a, Self> {
        let sdata = data.as_ref();
        let hmap = unsafe { &*self.sets.get() };
        let Some(&data) = hmap.get(sdata) else {
            let hmap = unsafe { &mut *self.sets.get() };
            let vector = unsafe { &mut *self.data.get() };
            let rcd: Rc<str> = Rc::from(sdata);
            vector.push(rcd.clone());
            let newid = unsafe { NonZeroUsize::new_unchecked(vector.len()) };
            _ = hmap.insert(rcd, newid);
            return StringRef {
                _inner: newid,
                _parent: PhantomData,
            };
        };
        return StringRef {
            _inner: data,
            _parent: PhantomData,
        };
    }
    fn resolve<'a>(&'a self, sref: StringRef<'a, Self>) -> Self::T<'a> {
        unsafe { (&*self.data.get()).get_unchecked(sref._inner.get() - 1).clone() }
    }
}
#[repr(align(64))]
pub struct MultiThreadedStringStore {
    countgen: AtomicUsize,
    vect: DashMap<NonZeroUsize, Arc<str>, rapidhash::fast::RandomState>,
    sets: DashMap<Arc<str>, NonZeroUsize, rapidhash::fast::RandomState>,
}
impl MultiThreadedStringStore {
    pub fn new() -> Self {
        Self {
            countgen: AtomicUsize::new(0),
            sets: Default::default(),
            vect: Default::default(),
        }
    }
}
impl StringStore for MultiThreadedStringStore {
    type T<'a> = Arc<str> where Self: 'a;
    fn resolve<'a>(&'a self, sref: StringRef<'a, Self>) -> Self::T<'a> {
        self.vect
            .get(&sref._inner)
            .expect("Since a StringRef was earlier made - this should not be empty")
            .clone()
    }
    fn matchval<'a, Data: AsRef<str>>(&'a self, data: Data) -> StringRef<'a, Self> {
        let sdata = data.as_ref();
        if let Some(dt) = self.sets.get(sdata) {
            return StringRef {
                _inner: *dt,
                _parent: PhantomData,
            };
        }
        let value: Arc<str> = Arc::from(sdata);
        let new_idx = self
            .sets
            .entry(value.clone())
            .or_insert_with(|| {
                let raw_idx = self.countgen.fetch_add(1, Ordering::AcqRel) + 1;
                let idx = unsafe { NonZeroUsize::new_unchecked(raw_idx) };
                self.vect.insert(idx, value);
                idx
            });
        StringRef {
            _inner: *new_idx,
            _parent: PhantomData,
        }
    }
}
pub trait StringStore {
    type T<'a>: AsRef<str> + 'a where Self: 'a;
    fn matchval<'a, Data: AsRef<str>>(&'a self, data: Data) -> StringRef<'a, Self>;
    fn resolve<'a>(&'a self, sref: StringRef<'a, Self>) -> Self::T<'a>;
}
pub struct StringRef<'a, T: StringStore + ?Sized> {
    _inner: NonZeroUsize,
    _parent: PhantomData<&'a T>,
}
#[automatically_derived]
impl<'a, T: ::core::fmt::Debug + StringStore + ?Sized> ::core::fmt::Debug
for StringRef<'a, T> {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::debug_struct_field2_finish(
            f,
            "StringRef",
            "_inner",
            &self._inner,
            "_parent",
            &&self._parent,
        )
    }
}
impl<'a, T: StringStore + ?Sized> Hash for StringRef<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self._inner.hash(state);
    }
}
impl<'a, T: StringStore + ?Sized> PartialEq for StringRef<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self._inner.eq(&other._inner)
    }
}
impl<'a, T: StringStore + ?Sized> Eq for StringRef<'a, T> {}
impl<'a, T: StringStore + ?Sized> Clone for StringRef<'a, T> {
    fn clone(&self) -> Self {
        Self {
            _inner: self._inner,
            _parent: PhantomData,
        }
    }
}
impl<'a, T: StringStore + ?Sized> Copy for StringRef<'a, T> {}
