pub mod block;
pub mod builder;
pub mod flags;
pub mod format;
pub mod function;
pub mod instr;
#[macro_use]
pub mod macros;
pub mod loc;
pub mod module;
pub mod types;

// Re-export pastey for declarative macros
pub use pastey;

// Re-export key types
pub use block::LLBlock;
pub use builder::LLBuilder;
pub use flags::*;
pub use format::LLFormat;
pub use function::LLFunction;
pub use instr::{LLInstruction, llprelude};
pub use loc::{LocSrc, VMLoc};
pub use module::LLModule;
pub use types::{FloatTy, IntTy, Width};

/// The sair_llir prelude brings the most common builder types and instruction constructors into scope.
pub mod prelude {
  pub use crate::block::LLBlock;
  pub use crate::builder::LLBuilder;
  pub use crate::flags::*;
  pub use crate::format::LLFormat;
  pub use crate::function::LLFunction;
  pub use crate::instr::llprelude::*;
  pub use crate::instr::LLInstruction;
  pub use crate::loc::{LocSrc, VMLoc};
  pub use crate::module::LLModule;
  pub use crate::types::{FloatTy, IntTy, Width};
}

#[cfg(test)]
mod tests {
  use super::prelude::*;
  use sart::ctr::*;

  #[test]
  fn test_all_32_opcodes_lower_and_opcode_ids() {
    let r1 = LocSrc::r1();
    let r2 = LocSrc::r2();
    let r3 = LocSrc::r3();
    let r4 = LocSrc::r4();

    // 01: Vcopy
    let inst = LLInstruction::inst_vcopy(Count::abs(32), VCopyMemFlags::default(), r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VCOPY);
    let bytes = inst.to_bytes();
    assert_eq!(bytes[0], INSTRUCTION_VCOPY);

    // 02: Mov
    let inst = LLInstruction::inst_mov(r1, r2);
    assert_eq!(inst.opcode(), INSTRUCTION_MOV);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_MOV);

    // 02 special: LargepadPtr & GlobalRwPtr
    let inst = LLInstruction::inst_largepad_ptr();
    assert_eq!(inst.opcode(), INSTRUCTION_MOV);
    let inst = LLInstruction::inst_global_rw_ptr();
    assert_eq!(inst.opcode(), INSTRUCTION_MOV);

    // 03: Reg
    let inst = LLInstruction::inst_reg(Width::W64, 0, 42, r1);
    assert_eq!(inst.opcode(), INSTRUCTION_REG);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_REG);

    // 04: Mark
    let inst = LLInstruction::inst_mark(100);
    assert_eq!(inst.opcode(), INSTRUCTION_MARK);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_MARK);

    // 05: Jmp
    let inst = LLInstruction::inst_jmp(100);
    assert_eq!(inst.opcode(), INSTRUCTION_JMP);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_JMP);

    // 06: Jif
    let inst = LLInstruction::inst_jif(JifIntent::JZ, Width::W64, 100, r1);
    assert_eq!(inst.opcode(), INSTRUCTION_JIF);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_JIF);

    // 07: Vcmp
    let inst = LLInstruction::inst_vcmp(Width::W64, CmpOp::Eq, 16, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VCMP);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VCMP);

    // 08: Vadd
    let inst = LLInstruction::inst_vadd(IntTy::U64, VAddFlags::none(), 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VADD);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VADD);

    // 09: Vaddf
    let inst = LLInstruction::inst_vaddf(FloatTy::F64, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VADDF);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VADDF);

    // 10: Vsub
    let inst = LLInstruction::inst_vsub(IntTy::I64, VAddFlags::none(), 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VSUB);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VSUB);

    // 11: Vsubf
    let inst = LLInstruction::inst_vsubf(FloatTy::F64, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VSUBF);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VSUBF);

    // 12: Vmul
    let inst = LLInstruction::inst_vmul(IntTy::U64, VMulFlags::low(), 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VMUL);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VMUL);

    // 13: Vmulf
    let inst = LLInstruction::inst_vmulf(FloatTy::F64, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VMULF);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VMULF);

    // 14: Div
    let inst = LLInstruction::inst_div(IntTy::U64, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_DIV);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_DIV);

    // 15: Rem
    let inst = LLInstruction::inst_rem(IntTy::U64, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_REM);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_REM);

    // 16: Vdivf
    let inst = LLInstruction::inst_vdivf(FloatTy::F64, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VDIVF);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VDIVF);

    // 17: Cast
    let inst = LLInstruction::inst_cast(IntTy::U32, IntTy::U64, r1, r2);
    assert_eq!(inst.opcode(), INSTRUCTION_CAST);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_CAST);

    // 18: Vneg
    let inst = LLInstruction::inst_vneg(IntTy::I64, 8, r1, r2);
    assert_eq!(inst.opcode(), INSTRUCTION_VNEG);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VNEG);

    // 19: Vabs
    let inst = LLInstruction::inst_vabs(IntTy::I64, 8, r1, r2);
    assert_eq!(inst.opcode(), INSTRUCTION_VABS);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VABS);

    // 20: Vfop
    let inst = LLInstruction::inst_vfop(FloatTy::F64, VfopSubOp::Sqrt, 8, r1, r2);
    assert_eq!(inst.opcode(), INSTRUCTION_VFOP);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VFOP);

    // 21: Vfcast
    let inst = LLInstruction::inst_vfcast(VFCastOp::FloatToInt, FloatTy::F64, IntTy::I64, 8, r1, r2);
    assert_eq!(inst.opcode(), INSTRUCTION_VFCAST);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VFCAST);

    // 22: Vbit
    let inst = LLInstruction::inst_vbit(Width::W64, BitOp::Xor, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VBIT);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VBIT);

    // 23: Vrot
    let inst = LLInstruction::inst_vrot(IntTy::U64, RotOp::RotL, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VROT);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VROT);

    // 24: Vsh
    let inst = LLInstruction::inst_vsh(IntTy::U64, ShiftOp::Shl, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VSH);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VSH);

    // 25: Vcnt
    let inst = LLInstruction::inst_vcnt(Width::W64, CountOp::Popcnt, 8, r1, r2);
    assert_eq!(inst.opcode(), INSTRUCTION_VCNT);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VCNT);

    // 26: Vminimax
    let inst = LLInstruction::inst_vminimax(IntTy::U64, MinMaxOp::Min, 8, r1, r2, r3);
    assert_eq!(inst.opcode(), INSTRUCTION_VMINIMAX);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VMINIMAX);

    // 27: Vfma
    let inst = LLInstruction::inst_vfma(FloatTy::F64, VFmaMemFlags::none(), 8, r1, r2, r3, r4);
    assert_eq!(inst.opcode(), INSTRUCTION_VFMA);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_VFMA);

    // 28: Synccall
    let inst = LLInstruction::inst_synccall(RegBitmask::empty(), 0x1234);
    assert_eq!(inst.opcode(), INSTRUCTION_SYNCCALL);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_SYNCCALL);

    // 29: Spawn
    let inst = LLInstruction::inst_spawn(0x5678, SpawnFlags::new(true, Some(r1)));
    assert_eq!(inst.opcode(), INSTRUCTION_SPAWN);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_SPAWN);

    // 30: Task
    let inst = LLInstruction::inst_task(TaskSubOp::SyncYield, 0, 100);
    assert_eq!(inst.opcode(), INSTRUCTION_TASK);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_TASK);

    // 31: Atomic
    let inst = LLInstruction::inst_atomic(
      AtomicOp::Cas,
      IntTy::U64,
      AtomicOrdering::SeqCst,
      AtomicOrdering::SeqCst,
      AtomicRmwOp::Add,
      r1,
      r2,
      r3,
      r4,
    );
    assert_eq!(inst.opcode(), INSTRUCTION_ATOMIC);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_ATOMIC);

    // 32: Scratch
    let inst = LLInstruction::inst_scratch(ScratchClass::Alloc, 1, 2);
    assert_eq!(inst.opcode(), INSTRUCTION_SCRATCH);
    assert_eq!(inst.to_bytes()[0], INSTRUCTION_SCRATCH);
  }

  #[test]
  fn test_builder_and_module() {
    let mut builder = LLBuilder::new_function(0x1000, "vector_dot_product");
    let r1 = LocSrc::r1();
    let r2 = LocSrc::r2();
    let r3 = LocSrc::r3();
    let r4 = LocSrc::r4();

    builder
      .iconst64(64, r1)
      .vadd_std(IntTy::U64, 8, r2, r3, r4)
      .vbit(Width::W64, BitOp::And, 8, r4, r1, r2)
      .mark(0x8000_0000_0000_0001);

    let func = builder.finish();
    let mut module = LLModule::new("test_module");
    module.add_function(func);

    let bytes = module.to_bytes();
    assert!(!bytes.is_empty());

    let formatted = format!("{module}");
    assert!(formatted.contains("vector_dot_product"));
    assert!(formatted.contains("vadd"));
  }
}
