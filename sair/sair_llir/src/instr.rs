use sart::ctr::*;

use crate::{
  flags::{
    AtomicOp, AtomicOrdering, AtomicRmwOp, BitOp, CmpOp, Count, CountOp, JifIntent, MinMaxOp,
    RegBitmask, RotOp, ScratchClass, ShiftOp, SpawnFlags, TaskSubOp, VAddFlags, VCopyMemFlags,
    VFCastOp, VFmaMemFlags, VfopSubOp, VMulFlags,
  },
  instloader,
  loc::LocSrc,
  types::{FloatTy, IntTy, Width},
};

instloader! {
  /// Vectored copy operation (load / store / memcpy)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vcopy { count: Count, memflags: VCopyMemFlags } (src) -> (target)
  lower { INSTRUCTION_VCOPY } (|buf: &mut Vec<u8>, count: &Count, memflags: &VCopyMemFlags, src: &LocSrc, target: &LocSrc| {
    let counttag = count.is_r1();
    buf.push(memflags.lower(counttag));
    buf.push((src.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend(count.raw_value().to_le_bytes());
    buf.extend((src.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Register-to-register move
  Mov { } (src) -> (target)
  lower { INSTRUCTION_MOV } (|buf: &mut Vec<u8>, src: &LocSrc, target: &LocSrc| {
    buf.push((src.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
  })

  /// Write the pointer to largepad into register r1
  LargepadPtr { } () -> ()
  lower { INSTRUCTION_MOV } (|buf: &mut Vec<u8>| {
    buf.push((12 << 4) | 12);
  })

  /// Write the pointer to global RW data into register r1
  GlobalRwPtr { } () -> ()
  lower { INSTRUCTION_MOV } (|buf: &mut Vec<u8>| {
    buf.push((13 << 4) | 13);
  })

  /// Load immediate constant into a register / location
  Reg { width: Width, offset: u8, value: u64 } () -> (target)
  lower { INSTRUCTION_REG } (|buf: &mut Vec<u8>, width: &Width, offset: &u8, value: &u64, target: &LocSrc| {
    buf.push(((*width as u8) << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(*offset);
    buf.extend(value.to_le_bytes());
  })

  /// Bytecode resolver mark / JIT guidance marker
  Mark { marker: u64 } () -> ()
  lower { INSTRUCTION_MARK } (|buf: &mut Vec<u8>, marker: &u64| {
    buf.extend(marker.to_le_bytes());
  })

  /// Unconditional jump to a 64-bit target marker
  Jmp { marker: u64 } () -> ()
  lower { INSTRUCTION_JMP } (|buf: &mut Vec<u8>, marker: &u64| {
    buf.extend(marker.to_le_bytes());
  })

  /// Conditional jump (Jump-If-Zero / Jump-If-Not-Zero)
  ///
  /// *Note: Uses `i32` displacement offset (slated for phase-out).*
  Jif { intent: JifIntent, width: Width, marker: u64 } (loc) -> ()
  lower { INSTRUCTION_JIF } (|buf: &mut Vec<u8>, intent: &JifIntent, width: &Width, marker: &u64, loc: &LocSrc| {
    buf.push(((*intent as u8) << 7) | ((*width as u8) << 5) | (loc.get_loc_bits() & 0x0F));
    buf.extend((loc.offset as i32).to_le_bytes());
    buf.extend(marker.to_le_bytes());
  })

  /// Vectored comparison (integral & float)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vcmp { width: Width, op: CmpOp, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VCMP } (|buf: &mut Vec<u8>, width: &Width, op: &CmpOp, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*width as u8) << 5) | (op.as_u8() & 0x1F));
    buf.push((src1.get_loc_bits() << 4) | (src2.get_loc_bits() & 0x0F));
    buf.push(target.get_loc_bits() << 4);
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored integer addition (ADC, saturating, standard)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vadd { ty: IntTy, flags: VAddFlags, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VADD } (|buf: &mut Vec<u8>, ty: &IntTy, flags: &VAddFlags, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(flags.lower());
    buf.push(0u8);
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored floating-point addition
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vaddf { ty: FloatTy, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VADDF } (|buf: &mut Vec<u8>, ty: &FloatTy, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored integer subtraction (SBB, saturating, standard)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vsub { ty: IntTy, flags: VAddFlags, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VSUB } (|buf: &mut Vec<u8>, ty: &IntTy, flags: &VAddFlags, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(flags.lower());
    buf.push(0u8);
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored floating-point subtraction
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vsubf { ty: FloatTy, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VSUBF } (|buf: &mut Vec<u8>, ty: &FloatTy, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored integer multiplication (low, high, wide)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vmul { ty: IntTy, flags: VMulFlags, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VMUL } (|buf: &mut Vec<u8>, ty: &IntTy, flags: &VMulFlags, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(flags.lower());
    buf.push(0u8);
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored floating-point multiplication
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vmulf { ty: FloatTy, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VMULF } (|buf: &mut Vec<u8>, ty: &FloatTy, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Integer division
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Div { ty: IntTy } (src1, src2) -> (target)
  lower { INSTRUCTION_DIV } (|buf: &mut Vec<u8>, ty: &IntTy, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Integer remainder
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Rem { ty: IntTy } (src1, src2) -> (target)
  lower { INSTRUCTION_REM } (|buf: &mut Vec<u8>, ty: &IntTy, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored floating-point division
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vdivf { ty: FloatTy, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VDIVF } (|buf: &mut Vec<u8>, ty: &FloatTy, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Scalar type cast (sextend, uextend, ireduce, fdemote, fpromote, fcvt)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Cast { src_ty: IntTy, dst_ty: IntTy } (src) -> (target)
  lower { INSTRUCTION_CAST } (|buf: &mut Vec<u8>, src_ty: &IntTy, dst_ty: &IntTy, src: &LocSrc, target: &LocSrc| {
    buf.push(((*src_ty as u8) << 4) | (*dst_ty as u8 & 0x0F));
    buf.push((src.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend((src.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored integer / float negation
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vneg { ty: IntTy, count: u32 } (src) -> (target)
  lower { INSTRUCTION_VNEG } (|buf: &mut Vec<u8>, ty: &IntTy, count: &u32, src: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src.get_loc_bits() & 0x0F));
    buf.push(target.get_loc_bits() << 4);
    buf.extend(count.to_le_bytes());
    buf.extend((src.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored absolute value
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vabs { ty: IntTy, count: u32 } (src) -> (target)
  lower { INSTRUCTION_VABS } (|buf: &mut Vec<u8>, ty: &IntTy, count: &u32, src: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src.get_loc_bits() & 0x0F));
    buf.push(target.get_loc_bits() << 4);
    buf.extend(count.to_le_bytes());
    buf.extend((src.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored floating-point operation (ceil, floor, trunc, nearest, sqrt)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vfop { float_ty: FloatTy, sub_op: VfopSubOp, count: u32 } (src) -> (target)
  lower { INSTRUCTION_VFOP } (|buf: &mut Vec<u8>, float_ty: &FloatTy, sub_op: &VfopSubOp, count: &u32, src: &LocSrc, target: &LocSrc| {
    buf.push(src.get_loc_bits() & 0x0F);
    buf.push((target.get_loc_bits() << 4) | ((*float_ty as u8) << 3) | (*sub_op as u8 & 0x07));
    buf.extend(count.to_le_bytes());
    buf.extend((src.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored float-integer cast
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vfcast { op: VFCastOp, float_ty: FloatTy, int_ty: IntTy, count: u32 } (src) -> (target)
  lower { INSTRUCTION_VFCAST } (|buf: &mut Vec<u8>, op: &VFCastOp, float_ty: &FloatTy, int_ty: &IntTy, count: &u32, src: &LocSrc, target: &LocSrc| {
    buf.push(((*op as u8) << 4) | ((*float_ty as u8) << 3) | (*int_ty as u8 & 0x07));
    buf.push((src.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend(count.to_le_bytes());
    buf.extend((src.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored bitwise operations (and, or, xor, not, etc.)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vbit { width: Width, op: BitOp, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VBIT } (|buf: &mut Vec<u8>, width: &Width, op: &BitOp, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*width as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(*op as u8);
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored bit rotation (rotl, rotr)
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vrot { ty: IntTy, op: RotOp, count: u32 } (src1, amount_src) -> (target)
  lower { INSTRUCTION_VROT } (|buf: &mut Vec<u8>, ty: &IntTy, op: &RotOp, count: &u32, src1: &LocSrc, amount_src: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((amount_src.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(*op as u8);
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((amount_src.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Vectored bit shift (shl, shr)
  ///
  /// Compact encoding: Uses standard `i8` count offsets.
  Vsh { ty: IntTy, op: ShiftOp, count: u32 } (src1, amount_src) -> (target)
  lower { INSTRUCTION_VSH } (|buf: &mut Vec<u8>, ty: &IntTy, op: &ShiftOp, count: &u32, src1: &LocSrc, amount_src: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8 & 0x07) << 5) | ((*op as u8 & 0x01) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((amount_src.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.extend(count.to_le_bytes());
    buf.push(src1.offset as i8 as u8);
    buf.push(amount_src.offset as i8 as u8);
    buf.push(target.offset as i8 as u8);
    buf.push(0u8);
  })

  /// Vectored count operation (popcnt, clz, cls, ctz)
  ///
  /// Compact encoding: Uses standard `i8` count offsets.
  Vcnt { width: Width, op: CountOp, count: u32 } (src) -> (target)
  lower { INSTRUCTION_VCNT } (|buf: &mut Vec<u8>, width: &Width, op: &CountOp, count: &u32, src: &LocSrc, target: &LocSrc| {
    buf.push(((*width as u8) << 4) | (src.get_loc_bits() & 0x0F));
    buf.push((target.get_loc_bits() << 4) | (*op as u8 & 0x0F));
    buf.extend(count.to_le_bytes());
    buf.push(src.offset as i8 as u8);
    buf.push(target.offset as i8 as u8);
  })

  /// Vectored min/max operation
  ///
  /// Compact encoding: Uses standard `i8` count offsets.
  Vminimax { ty: IntTy, op: MinMaxOp, count: u32 } (src1, src2) -> (target)
  lower { INSTRUCTION_VMINIMAX } (|buf: &mut Vec<u8>, ty: &IntTy, op: &MinMaxOp, count: &u32, src1: &LocSrc, src2: &LocSrc, target: &LocSrc| {
    buf.push(((*ty as u8) << 4) | (src1.get_loc_bits() & 0x0F));
    buf.push((src2.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(*op as u8);
    buf.extend(count.to_le_bytes());
    buf.push(src1.offset as i8 as u8);
    buf.push(src2.offset as i8 as u8);
    buf.push(target.offset as i8 as u8);
    buf.push(0u8);
  })

  /// Vectored floating fused-multiply-add: target = (src1 * src2) + src3
  ///
  /// *Note: Uses `i32` displacement offsets (slated for phase-out).*
  Vfma { float_ty: FloatTy, memflags: VFmaMemFlags, count: u32 } (src1, src2, src3) -> (target)
  lower { INSTRUCTION_VFMA } (|buf: &mut Vec<u8>, float_ty: &FloatTy, memflags: &VFmaMemFlags, count: &u32, src1: &LocSrc, src2: &LocSrc, src3: &LocSrc, target: &LocSrc| {
    buf.push((src1.get_loc_bits() << 4) | (src2.get_loc_bits() & 0x0F));
    buf.push((src3.get_loc_bits() << 4) | (target.get_loc_bits() & 0x0F));
    buf.push(((memflags.lower() & 0x7F) << 1) | (*float_ty as u8 & 0x01));
    buf.extend(count.to_le_bytes());
    buf.extend((src1.offset as i32).to_le_bytes());
    buf.extend((src2.offset as i32).to_le_bytes());
    buf.extend((src3.offset as i32).to_le_bytes());
    buf.extend((target.offset as i32).to_le_bytes());
  })

  /// Synchronous function call into an identified section
  Synccall { regignore: RegBitmask, section_id: u64 } () -> ()
  lower { INSTRUCTION_SYNCCALL } (|buf: &mut Vec<u8>, regignore: &RegBitmask, section_id: &u64| {
    buf.push(regignore.raw());
    buf.extend(section_id.to_le_bytes());
  })

  /// Spawn thread / asynchronous task
  Spawn { section_id: u64, flags: SpawnFlags } () -> ()
  lower { INSTRUCTION_SPAWN } (|buf: &mut Vec<u8>, section_id: &u64, flags: &SpawnFlags| {
    buf.extend(section_id.to_le_bytes());
    buf.push(flags.lower());
  })

  /// Task control operations (detach, join, is_complete, unpark, yield, sleep, park)
  Task { sub_op: TaskSubOp, def: u8, marker: u64 } () -> ()
  lower { INSTRUCTION_TASK } (|buf: &mut Vec<u8>, sub_op: &TaskSubOp, def: &u8, marker: &u64| {
    buf.push(((*sub_op as u8) << 4) | (*def & 0x0F));
    buf.extend(marker.to_le_bytes());
  })

  /// Atomic memory operations (CAS, LOAD, RMW, STORE)
  ///
  /// Compact encoding: Uses standard `i8` pointer & operand offsets.
  Atomic { op: AtomicOp, ty: IntTy, ordering: AtomicOrdering, ordering2: AtomicOrdering, rmw_op: AtomicRmwOp } (ptr, val, expected) -> (target)
  lower { INSTRUCTION_ATOMIC } (|buf: &mut Vec<u8>, op: &AtomicOp, ty: &IntTy, ordering: &AtomicOrdering, ordering2: &AtomicOrdering, rmw_op: &AtomicRmwOp, ptr: &LocSrc, val: &LocSrc, expected: &LocSrc, target: &LocSrc| {
    buf.push(((*op as u8) << 6) | ((*ty as u8 & 0x07) << 3) | (*ordering as u8 & 0x07));
    buf.push(ptr.offset as i8 as u8);
    buf.push(val.offset as i8 as u8);
    buf.push(expected.offset as i8 as u8);
    buf.push(target.offset as i8 as u8);
    buf.push(((*ordering2 as u8) << 5) | (*rmw_op as u8 & 0x1F));
    buf.push((target.get_loc_bits() << 4) | (expected.get_loc_bits() & 0x0F));
    buf.push((val.get_loc_bits() << 4) | (ptr.get_loc_bits() & 0x0F));
  })

  /// Scratchpad management protocols (allocate, deallocate)
  Scratch { class: ScratchClass, size_reg: u8, align_reg: u8 } () -> ()
  lower { INSTRUCTION_SCRATCH } (|buf: &mut Vec<u8>, class: &ScratchClass, size_reg: &u8, align_reg: &u8| {
    let word = ((*class as u16) << 14) | (((*size_reg as u16) & 0x0F) << 4) | ((*align_reg as u16) & 0x0F);
    buf.extend(word.to_le_bytes());
  })
}
