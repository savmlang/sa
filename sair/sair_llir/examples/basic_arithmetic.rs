use sair_llir::prelude::*;

fn main() {
  println!("=== SaVM LLIR Example: Basic & Vector Arithmetic ===");

  let mut builder = LLBuilder::new_function(0x1000, "arithmetic_kernel");
  let r1 = LocSrc::r1();
  let r2 = LocSrc::r2();
  let r3 = LocSrc::r3();
  let r4 = LocSrc::r4();
  let r5 = LocSrc::r5();
  let r6 = LocSrc::r6();
  let r7 = LocSrc::r7();
  let r8 = LocSrc::r8();

  // 1. Loading constants
  builder
    .iconst64(100, r1)
    .iconst64(25, r2)
    .iconst32(8, r3);

  // 2. Scalar / Vector addition and subtraction
  builder
    .vadd_std(IntTy::U64, 1, r1, r2, r4) // r4 = 100 + 25 = 125
    .vsub_std(IntTy::U64, 1, r1, r2, r5); // r5 = 100 - 25 = 75

  // 3. Saturating & Carry addition
  builder
    .vadd(IntTy::U8, VAddFlags::saturating(), 16, r1, r2, r6)
    .vadd(IntTy::U64, VAddFlags::carry(), 1, r1, r2, r7);

  // 4. Integer multiplication (Low, High, Wide)
  builder
    .vmul(IntTy::U64, VMulFlags::low(), 4, r1, r2, r8)
    .vmul(IntTy::U64, VMulFlags::wide(), 4, r1, r2, r6);

  // 5. Integer Division and Remainder
  builder
    .div(IntTy::U64, r1, r2, r3) // r3 = 100 / 25 = 4
    .rem(IntTy::U64, r1, r2, r4); // r4 = 100 % 25 = 0

  // 6. Bitwise operations (AND, OR, XOR, NOT, BITREV, BSWAP)
  builder
    .vbit(Width::W64, BitOp::And, 4, r1, r2, r3)
    .vbit(Width::W64, BitOp::Or, 4, r1, r2, r4)
    .vbit(Width::W64, BitOp::Xor, 4, r1, r2, r5)
    .vbit(Width::W64, BitOp::Not, 4, r1, r1, r6)
    .vbit(Width::W64, BitOp::BSwap, 1, r1, r1, r7);

  // 7. Shifts and Rotations
  builder
    .vsh(IntTy::U64, ShiftOp::Shl, 4, r1, r3, r4)
    .vsh(IntTy::U64, ShiftOp::Shr, 4, r1, r3, r5)
    .vrot(IntTy::U64, RotOp::RotL, 4, r1, r3, r6)
    .vrot(IntTy::U64, RotOp::RotR, 4, r1, r3, r7);

  // 8. Vector Min/Max and Count operations
  builder
    .vminimax(IntTy::U64, MinMaxOp::Min, 4, r1, r2, r3)
    .vminimax(IntTy::U64, MinMaxOp::Max, 4, r1, r2, r4)
    .vcnt(Width::W64, CountOp::Popcnt, 4, r1, r5)
    .vcnt(Width::W64, CountOp::Clz, 4, r1, r6);

  // 9. Negation and Absolute values
  builder
    .vneg(IntTy::I64, 4, r1, r2)
    .vabs(IntTy::I64, 4, r2, r3);

  let func = builder.finish();
  let mut module = LLModule::new("arithmetic_demo");
  module.add_function(func);

  println!("--- Generated LLIR Representation ---");
  println!("{}", module);

  let bytecode = module.to_bytes();
  println!("--- Bytecode Emission ---");
  println!("Total bytecode bytes emitted: {}", bytecode.len());
  println!("Hex dump (first 64 bytes): {:02x?}", &bytecode[..bytecode.len().min(64)]);
}
