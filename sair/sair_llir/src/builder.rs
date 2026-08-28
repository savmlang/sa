use crate::{
  block::{LLBlock, JIT_HOT_BIT},
  flags::{
    AtomicOp, AtomicOrdering, AtomicRmwOp, BitOp, CmpOp, Count, CountOp, JifIntent,
    MinMaxOp, RegBitmask, RotOp, ScratchClass, ShiftOp, SpawnFlags, TaskSubOp, VAddFlags,
    VCopyMemFlags, VFCastOp, VFmaMemFlags, VfopSubOp, VMulFlags,
  },
  function::LLFunction,
  instr::LLInstruction,
  loc::LocSrc,
  types::{FloatTy, IntTy, Width},
};

/// Fluent builder for constructing SaVM LLIR instructions, blocks, and functions.
pub struct LLBuilder {
  pub function: LLFunction,
  pub current_block_idx: usize,
}

impl LLBuilder {
  pub fn new(function: LLFunction) -> Self {
    let mut builder = Self {
      function,
      current_block_idx: 0,
    };
    if builder.function.blocks.is_empty() {
      // Entry block (id = 0) has no mark emitted
      let entry_block = LLBlock::with_name(0, "entry", false);
      builder.function.add_block(entry_block);
    }
    builder
  }

  pub fn new_function(section_id: u64, name: impl Into<String>) -> Self {
    Self::new(LLFunction::new(section_id, name))
  }

  /// Creates a new basic block identified by its return index (`usize`).
  ///
  /// - `hot` (`jit_jmp`): When `true`, the 63rd bit (sign bit `< 0`) is set on the marker ID,
  ///   signaling a hot loop / JIT check to SaVM's JIT and OSR runtime.
  /// - For `id == 0` (entry), no `mark` instruction is emitted.
  /// - For `id > 0`, a `mark(marker_id)` instruction is automatically prepended to the block.
  pub fn create_block(&mut self, name: Option<&str>, hot: bool) -> usize {
    let idx = self.function.blocks.len();
    let mut block = match name {
      Some(n) => LLBlock::with_name(idx as u64, n, hot),
      None => LLBlock {
        id: idx as u64,
        name: None,
        hot,
        instructions: Vec::new(),
      },
    };

    if idx > 0 {
      let marker_id = block.marker_id();
      block.push(LLInstruction::inst_mark(marker_id));
    }

    self.function.add_block(block);
    self.current_block_idx = idx;
    idx
  }

  /// Creates a standard (cold) basic block.
  pub fn block(&mut self, name: impl Into<String>) -> usize {
    self.create_block(Some(&name.into()), false)
  }

  /// Creates a hot loop basic block with JITUp / OSR check entitlement enabled.
  pub fn hot_block(&mut self, name: impl Into<String>) -> usize {
    self.create_block(Some(&name.into()), true)
  }

  /// Returns the computed 64-bit runtime marker ID for a given block index and hot flag.
  #[inline]
  pub fn marker_of(block_idx: usize, hot: bool) -> u64 {
    if block_idx == 0 {
      0
    } else if hot {
      (block_idx as u64) | JIT_HOT_BIT
    } else {
      block_idx as u64
    }
  }

  /// Returns the marker ID of an existing block in this function.
  #[inline]
  pub fn block_marker(&self, block_idx: usize) -> u64 {
    if block_idx < self.function.blocks.len() {
      self.function.blocks[block_idx].marker_id()
    } else {
      block_idx as u64
    }
  }

  pub fn position_at_block(&mut self, idx: usize) {
    assert!(idx < self.function.blocks.len(), "Block index out of range");
    self.current_block_idx = idx;
  }

  pub fn current_block(&self) -> &LLBlock {
    &self.function.blocks[self.current_block_idx]
  }

  pub fn current_block_mut(&mut self) -> &mut LLBlock {
    &mut self.function.blocks[self.current_block_idx]
  }

  #[inline]
  pub fn emit(&mut self, inst: LLInstruction) -> &mut Self {
    self.function.push_instruction(self.current_block_idx, inst);
    self
  }

  // --- High-level block jump methods ---

  /// Emit unconditional jump to target block index
  pub fn jmp_to(&mut self, target_block_idx: usize, hot: bool) -> &mut Self {
    let marker = Self::marker_of(target_block_idx, hot);
    self.jmp(marker)
  }

  /// Emit Jump-If-Zero targeting a block index
  pub fn jz_to(&mut self, width: Width, target_block_idx: usize, hot: bool, loc: LocSrc) -> &mut Self {
    let marker = Self::marker_of(target_block_idx, hot);
    self.jz(width, marker, loc)
  }

  /// Emit Jump-If-Not-Zero targeting a block index
  pub fn jnz_to(&mut self, width: Width, target_block_idx: usize, hot: bool, loc: LocSrc) -> &mut Self {
    let marker = Self::marker_of(target_block_idx, hot);
    self.jnz(width, marker, loc)
  }

  // --- Low-level instruction builders for all 32 SaVM opcodes ---

  /// Emit Vcopy (Vectored copy / load / store / memcpy)
  pub fn vcopy(&mut self, count: Count, memflags: VCopyMemFlags, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vcopy(count, memflags, src, target))
  }

  /// Emit Vcopy with absolute count and default flags
  pub fn vcopy_abs(&mut self, count: u32, src: LocSrc, target: LocSrc) -> &mut Self {
    self.vcopy(Count::abs(count), VCopyMemFlags::default(), src, target)
  }

  /// Emit Vcopy with count read from register r1
  pub fn vcopy_r1(&mut self, src: LocSrc, target: LocSrc) -> &mut Self {
    self.vcopy(Count::from_r1(), VCopyMemFlags::default(), src, target)
  }

  /// Emit Mov (Register-to-register move)
  pub fn mov(&mut self, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_mov(src, target))
  }

  /// Emit LargepadPtr (writes largepad pointer to register r1)
  pub fn largepad_ptr(&mut self) -> &mut Self {
    self.emit(LLInstruction::inst_largepad_ptr())
  }

  /// Emit GlobalRWPtr (writes global read-write memory pointer to register r1)
  pub fn global_rw_ptr(&mut self) -> &mut Self {
    self.emit(LLInstruction::inst_global_rw_ptr())
  }

  /// Emit Reg (Load constant immediate value into register)
  pub fn reg(&mut self, width: Width, offset: u8, value: u64, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_reg(width, offset, value, target))
  }

  /// Emit 64-bit integer constant load
  pub fn iconst64(&mut self, value: u64, target: LocSrc) -> &mut Self {
    self.reg(Width::W64, 0, value, target)
  }

  /// Emit 32-bit integer constant load
  pub fn iconst32(&mut self, value: u32, target: LocSrc) -> &mut Self {
    self.reg(Width::W32, 0, value as u64, target)
  }

  /// Emit Mark (Bytecode resolver mark / JIT guidance marker)
  pub fn mark(&mut self, marker: u64) -> &mut Self {
    self.emit(LLInstruction::inst_mark(marker))
  }

  /// Emit Jmp (Unconditional jump)
  pub fn jmp(&mut self, marker: u64) -> &mut Self {
    self.emit(LLInstruction::inst_jmp(marker))
  }

  /// Emit Jif (Conditional jump)
  pub fn jif(&mut self, intent: JifIntent, width: Width, marker: u64, loc: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_jif(intent, width, marker, loc))
  }

  /// Emit JZ (Jump if zero)
  pub fn jz(&mut self, width: Width, marker: u64, loc: LocSrc) -> &mut Self {
    self.jif(JifIntent::JZ, width, marker, loc)
  }

  /// Emit JNZ (Jump if not zero)
  pub fn jnz(&mut self, width: Width, marker: u64, loc: LocSrc) -> &mut Self {
    self.jif(JifIntent::JNZ, width, marker, loc)
  }

  /// Emit Vcmp (Vectored comparison)
  pub fn vcmp(&mut self, width: Width, op: CmpOp, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vcmp(width, op, count, src1, src2, target))
  }

  /// Emit Vadd (Vectored integer addition)
  pub fn vadd(&mut self, ty: IntTy, flags: VAddFlags, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vadd(ty, flags, count, src1, src2, target))
  }

  /// Emit standard Vadd (no carry / sat flags)
  pub fn vadd_std(&mut self, ty: IntTy, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.vadd(ty, VAddFlags::none(), count, src1, src2, target)
  }

  /// Emit Vaddf (Vectored float addition)
  pub fn vaddf(&mut self, ty: FloatTy, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vaddf(ty, count, src1, src2, target))
  }

  /// Emit Vsub (Vectored integer subtraction)
  pub fn vsub(&mut self, ty: IntTy, flags: VAddFlags, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vsub(ty, flags, count, src1, src2, target))
  }

  /// Emit standard Vsub (no flags)
  pub fn vsub_std(&mut self, ty: IntTy, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.vsub(ty, VAddFlags::none(), count, src1, src2, target)
  }

  /// Emit Vsubf (Vectored float subtraction)
  pub fn vsubf(&mut self, ty: FloatTy, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vsubf(ty, count, src1, src2, target))
  }

  /// Emit Vmul (Vectored integer multiplication)
  pub fn vmul(&mut self, ty: IntTy, flags: VMulFlags, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vmul(ty, flags, count, src1, src2, target))
  }

  /// Emit Vmulf (Vectored float multiplication)
  pub fn vmulf(&mut self, ty: FloatTy, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vmulf(ty, count, src1, src2, target))
  }

  /// Emit Div (Integer division)
  pub fn div(&mut self, ty: IntTy, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_div(ty, src1, src2, target))
  }

  /// Emit Rem (Integer remainder)
  pub fn rem(&mut self, ty: IntTy, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_rem(ty, src1, src2, target))
  }

  /// Emit Vdivf (Vectored float division)
  pub fn vdivf(&mut self, ty: FloatTy, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vdivf(ty, count, src1, src2, target))
  }

  /// Emit Cast (Scalar type conversion)
  pub fn cast(&mut self, src_ty: IntTy, dst_ty: IntTy, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_cast(src_ty, dst_ty, src, target))
  }

  /// Emit Vneg (Vectored negation)
  pub fn vneg(&mut self, ty: IntTy, count: u32, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vneg(ty, count, src, target))
  }

  /// Emit Vabs (Vectored absolute value)
  pub fn vabs(&mut self, ty: IntTy, count: u32, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vabs(ty, count, src, target))
  }

  /// Emit Vfop (Vectored float operation: ceil, floor, trunc, nearest, sqrt)
  pub fn vfop(&mut self, float_ty: FloatTy, sub_op: VfopSubOp, count: u32, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vfop(float_ty, sub_op, count, src, target))
  }

  /// Emit Vfcast (Vectored float-int conversion)
  pub fn vfcast(&mut self, op: VFCastOp, float_ty: FloatTy, int_ty: IntTy, count: u32, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vfcast(op, float_ty, int_ty, count, src, target))
  }

  /// Emit Vbit (Vectored bitwise operations: and, or, xor, not, etc.)
  pub fn vbit(&mut self, width: Width, op: BitOp, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vbit(width, op, count, src1, src2, target))
  }

  /// Emit Vrot (Vectored bit rotation: rotl, rotr)
  pub fn vrot(&mut self, ty: IntTy, op: RotOp, count: u32, src1: LocSrc, amount_src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vrot(ty, op, count, src1, amount_src, target))
  }

  /// Emit Vsh (Vectored bit shift: shl, shr)
  pub fn vsh(&mut self, ty: IntTy, op: ShiftOp, count: u32, src1: LocSrc, amount_src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vsh(ty, op, count, src1, amount_src, target))
  }

  /// Emit Vcnt (Vectored bit count: popcnt, clz, cls, ctz)
  pub fn vcnt(&mut self, width: Width, op: CountOp, count: u32, src: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vcnt(width, op, count, src, target))
  }

  /// Emit Vminimax (Vectored min/max)
  pub fn vminimax(&mut self, ty: IntTy, op: MinMaxOp, count: u32, src1: LocSrc, src2: LocSrc, target: LocSrc) -> &mut Self {
    self.emit(LLInstruction::inst_vminimax(ty, op, count, src1, src2, target))
  }

  /// Emit Vfma with explicit `VFmaMemFlags` bitflags
  pub fn vfma(
    &mut self,
    float_ty: FloatTy,
    memflags: VFmaMemFlags,
    count: u32,
    src1: LocSrc,
    src2: LocSrc,
    src3: LocSrc,
    target: LocSrc,
  ) -> &mut Self {
    self.emit(LLInstruction::inst_vfma(float_ty, memflags, count, src1, src2, src3, target))
  }

  /// Emit standard Vfma without alignment constraints
  pub fn vfma_std(&mut self, float_ty: FloatTy, count: u32, src1: LocSrc, src2: LocSrc, src3: LocSrc, target: LocSrc) -> &mut Self {
    self.vfma(float_ty, VFmaMemFlags::none(), count, src1, src2, src3, target)
  }

  /// Emit Synccall with typed `RegBitmask`
  pub fn synccall(&mut self, regignore: RegBitmask, section_id: u64) -> &mut Self {
    self.emit(LLInstruction::inst_synccall(regignore, section_id))
  }

  /// Emit Synccall without ignored registers
  pub fn synccall_all(&mut self, section_id: u64) -> &mut Self {
    self.synccall(RegBitmask::empty(), section_id)
  }

  /// Emit Spawn (Thread / async task spawn)
  pub fn spawn(&mut self, section_id: u64, flags: SpawnFlags) -> &mut Self {
    self.emit(LLInstruction::inst_spawn(section_id, flags))
  }

  /// Emit Task (Task control operation)
  pub fn task(&mut self, sub_op: TaskSubOp, def: u8, marker: u64) -> &mut Self {
    self.emit(LLInstruction::inst_task(sub_op, def, marker))
  }

  /// Emit Atomic (Atomic CAS / LOAD / STORE / RMW)
  pub fn atomic(
    &mut self,
    op: AtomicOp,
    ty: IntTy,
    ordering: AtomicOrdering,
    ordering2: AtomicOrdering,
    rmw_op: AtomicRmwOp,
    ptr: LocSrc,
    val: LocSrc,
    expected: LocSrc,
    target: LocSrc,
  ) -> &mut Self {
    self.emit(LLInstruction::inst_atomic(op, ty, ordering, ordering2, rmw_op, ptr, val, expected, target))
  }

  /// Emit Scratch (Scratchpad allocate / deallocate)
  pub fn scratch(&mut self, class: ScratchClass, size_reg: u8, align_reg: u8) -> &mut Self {
    self.emit(LLInstruction::inst_scratch(class, size_reg, align_reg))
  }

  /// Finish builder and return the constructed `LLFunction`
  pub fn finish(self) -> LLFunction {
    self.function
  }
}
