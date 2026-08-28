use sair_llir::prelude::*;

fn main() {
  println!("=== SaVM LLIR Example: Floating Point & Fused Multiply-Add (FMA) ===");

  let mut builder = LLBuilder::new_function(0x2000, "float_matrix_kernel");
  let r1 = LocSrc::r1();
  let r2 = LocSrc::r2();
  let r3 = LocSrc::r3();
  let r4 = LocSrc::r4();
  let r5 = LocSrc::r5();
  let r6 = LocSrc::r6();
  let r7 = LocSrc::r7();

  // 1. Vector Floating Point Arithmetic
  builder
    .vaddf(FloatTy::F64, 8, r1, r2, r3)
    .vsubf(FloatTy::F64, 8, r1, r2, r4)
    .vmulf(FloatTy::F64, 8, r1, r2, r5)
    .vdivf(FloatTy::F64, 8, r1, r2, r6);

  // 2. F32 Single Precision Vector Ops
  builder
    .vaddf(FloatTy::F32, 16, r1, r2, r3)
    .vmulf(FloatTy::F32, 16, r1, r2, r4);

  // 3. Fused-Multiply-Add (FMA: r7 = (r1 * r2) + r3)
  // Memory flags encode 4-container mixed-radix alignment (16B, 32B, 64B)
  let fma_aligned_flags = VFmaMemFlags {
    align_src1: AlignData::B64,
    align_src2: AlignData::B64,
    align_src3: AlignData::B64,
    align_target: AlignData::B64,
  };

  builder
    .vfma(FloatTy::F64, VFmaMemFlags::none(), 8, r1, r2, r3, r7)
    .vfma(FloatTy::F32, fma_aligned_flags, 16, r1, r2, r3, r7);

  // 4. Transcendental & Rounding Floating Ops (sqrt, ceil, floor, trunc, nearest)
  builder
    .vfop(FloatTy::F64, VfopSubOp::Sqrt, 8, r1, r2)
    .vfop(FloatTy::F64, VfopSubOp::Ceil, 8, r1, r3)
    .vfop(FloatTy::F64, VfopSubOp::Floor, 8, r1, r4)
    .vfop(FloatTy::F64, VfopSubOp::Trunc, 8, r1, r5)
    .vfop(FloatTy::F64, VfopSubOp::Nearest, 8, r1, r6);

  // 5. Type Conversions & Casts
  builder
    .cast(IntTy::F64, IntTy::I64, r1, r2) // scalar f64 -> i64
    .vfcast(VFCastOp::FloatToInt, FloatTy::F64, IntTy::I64, 8, r1, r3) // vector f64 -> i64
    .vfcast(VFCastOp::IntToFloat, FloatTy::F32, IntTy::I32, 16, r4, r5); // vector i32 -> f32

  // 6. Floating Point Comparisons
  builder
    .vcmp(Width::W64, CmpOp::FLt, 8, r1, r2, r3)
    .vcmp(Width::W64, CmpOp::FOrd, 8, r1, r2, r4)
    .vcmp(Width::W64, CmpOp::FEq, 8, r1, r2, r5);

  let func = builder.finish();
  let mut module = LLModule::new("float_demo");
  module.add_function(func);

  println!("--- Generated LLIR Representation ---");
  println!("{}", module);

  let bytecode = module.to_bytes();
  println!("--- Bytecode Emission ---");
  println!("Total bytecode bytes emitted: {}", bytecode.len());
}
