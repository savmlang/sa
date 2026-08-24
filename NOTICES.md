# SaVM Codebase Audit & Defect Notices

This document details all invalid logic, guaranteed-wrong code, undefined behavior (UB), crash/panic conditions, bitfield decoding mismatches, and unsound pointer operations identified across `savm/src/**/*.rs`.

---

## Table of Contents
1. [Critical Memory Safety & Undefined Behavior (UB)](#1-critical-memory-safety--undefined-behavior-ub)
2. [Cranelift JIT Backend Defects](#2-cranelift-jit-backend-defects)
3. [LLVM JIT Backend Defects](#3-llvm-jit-backend-defects)
4. [Pickle IR & Bytecode Reader Defects](#4-pickle-ir--bytecode-reader-defects)
5. [Management & Runtime Subsystem Defects](#5-management--runtime-subsystem-defects)
6. [Summary Matrix](#6-summary-matrix)

---

## 1. Critical Memory Safety & Undefined Behavior (UB)

### 1.1 Zero-Capacity Layout Undefined Behavior in `FixedVec`
- **Location**: [`savm/src/acaot/acdag/fixedvec.rs:17-24`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/acdag/fixedvec.rs#L17-L24) and [`savm/src/acaot/acdag/fixedvec.rs:91`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/acdag/fixedvec.rs#L91)
- **Severity**: **Critical (UB)**
- **Description**:
  `FixedVec::new(cap)` invokes `alloc::alloc::alloc(layout)` with `layout = Layout::array::<T>(cap)`. When `cap == 0`, `layout.size() == 0`.
  In Rust's standard allocator API (`std::alloc::alloc` and `std::alloc::dealloc`), passing a zero-sized layout is **instant Undefined Behavior (UB)**.
- **Triggers**: Directly instantiated with `FixedVec::new(0)` in [`savm/src/acaot/acdag/mod.rs:69-71`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/acdag/mod.rs#L69-L71), [`line 109`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/acdag/mod.rs#L109), and [`line 208`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/acdag/mod.rs#L208).
- **Remediation**:
  When `cap == 0` or `size_of::<T>() == 0`, set `data = ptr::NonNull::dangling().as_ptr()` without calling `alloc` or `dealloc`.

```rust
// Fix in savm/src/acaot/acdag/fixedvec.rs:
pub fn new(cap: usize) -> Self {
  if cap == 0 || size_of::<T>() == 0 {
    return Self {
      cap: 0,
      len: 0,
      data: ptr::NonNull::dangling().as_ptr(),
    };
  }
  let layout = Layout::array::<T>(cap).unwrap();
  let data = unsafe { alloc::alloc::alloc(layout) as *mut T };
  Self { cap, len: 0, data }
}
```

---

### 1.2 Scratchpad Buffer Aliasing & Memory Corruption Across Frames
- **Location**: [`savm/src/sync/mod.rs:108-113`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/sync/mod.rs#L108-L113)
- **Severity**: **Critical (Memory Corruption)**
- **Description**:
  In `VMState::init()`, 50 distinct 24-quad scratchpads are allocated such that each task frame `ts[i]` owns its own disjoint memory slice.
  In `fncall_prep`:
  ```rust
  pub fn fncall_prep(vmstat: *mut VMState, oldtsk: *mut VMTaskState) {
    unsafe {
      (*vmstat).cindex += 1;
      ptr::write((*vmstat).ts.as_mut_ptr().add((*vmstat).cindex), *oldtsk);
    }
  }
  ```
  `ptr::write(..., *oldtsk)` byte-copies the caller's `VMTaskState`. This copies `oldtsk.scratchpad` into `ts[cindex].scratchpad`, overwriting the pre-initialized unique scratchpad pointer for task index `cindex`.
- **Impact**: Caller (`ts[cindex - 1]`) and callee (`ts[cindex]`) now point to the exact same scratchpad buffer. Nested function scratchpad writes clobber the caller's live scratchpad state.
- **Remediation**: Preserve the destination task's pre-allocated scratchpad pointer when copying:
  ```rust
  pub fn fncall_prep(vmstat: *mut VMState, oldtsk: *mut VMTaskState) {
    unsafe {
      assert!((*vmstat).cindex + 1 < 50, "VM call stack overflow");
      (*vmstat).cindex += 1;
      let target_ts = (*vmstat).ts.as_mut_ptr().add((*vmstat).cindex);
      let saved_scratchpad = (*target_ts).scratchpad;
      ptr::write(target_ts, *oldtsk);
      (*target_ts).scratchpad = saved_scratchpad;
    }
  }
  ```

---

### 1.3 Missing Bounds Checks on Call Depth `cindex`
- **Location**: [`savm/src/sync/mod.rs:108-125`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/sync/mod.rs#L108-L125)
- **Severity**: **High (Buffer Overflow / Underflow)**
- **Description**:
  - In `fncall_prep`: `(*vmstat).cindex += 1;` lacks an upper bounds check. Depth $\ge 50$ writes past the `ts` buffer via `ptr::write`.
  - In `fncall_out`: `(*vmstat).cindex -= 1;` lacks an underflow check. If `cindex == 0`, it wraps to `usize::MAX` in release mode.
- **Remediation**: Add bounds assertions before incrementing and decrementing `cindex`.

---

### 1.4 State Corruption on Nested/Re-entrant `fncall` Calls
- **Location**: [`savm/src/sync/mod.rs:139-159`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/sync/mod.rs#L139-L159) and [`lines 208-216`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/sync/mod.rs#L208-L216)
- **Severity**: **High (Logic / State Corruption)**
- **Description**:
  `prepare_interpreter_loop` overwrites `(*t).ws.relocmap` and `(*t).ws.jmp`. When section A executes a nested call `self.fncall(section_b, ...)`, `prepare_interpreter_loop` is called for section B, replacing `(*t).ws.relocmap` and `jmp`. When section B returns and section A resumes, `relocmap` is now section B's relocmap instead of section A's.
- **Remediation**: Save and restore `(*vmstat).ws.jmp` and `(*vmstat).ws.relocmap` across `fncall`.

---

### 1.5 Unchecked `unwrap_unchecked()` on Missing Stencil Relocation
- **Location**: [`savm/src/management/cinder/mod.rs:146-157`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/management/cinder/mod.rs#L146-L157)
- **Severity**: **High (UB / Crash)**
- **Description**:
  Calling `.find(...).unwrap_unchecked().unwrap_unchecked()` on `resolved.iter()` triggers instant undefined behavior if any stencil relocation symbol is not present in `resolved`.
- **Remediation**: Use `.expect("Missing required relocation symbol in StencilMap")`.

---

## 2. Cranelift JIT Backend Defects

### 2.1 Unconditional Slice-to-Integer Conversion Panic in `libcall.rs`
- **Location**: [`savm/src/acaot/native/cranelift/irgen/almu/libcall.rs:37`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/cranelift/irgen/almu/libcall.rs#L37)
- **Severity**: **Critical (Guaranteed Panic)**
- **Description**:
  `readws!(meta, start = 0, stop = 4, u64)` evaluates `<u64>::from_ne_bytes(meta.ws[0..4].try_into().unwrap())`. Converting a 4-byte slice into an 8-byte array (`[u8; 8]`) panics 100% of the time at compile time.
- **Remediation**:
  Change to `readws!(meta, start = 0, stop = 4, u32) as u64` or expand slice range to `stop = 8`.

---

### 2.2 Inverted `MOV` Register Assignment
- **Location**: [`savm/src/acaot/native/cranelift/irgen/mod.rs:158-165`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/cranelift/irgen/mod.rs#L158-L165)
- **Severity**: **High (Logic Inversion)**
- **Description**:
  ```rust
  let src = resolve_reg(builder, meta, source);
  let tgt = resolve_reg(builder, meta, target);
  let tgt = builder.use_var(tgt);
  builder.def_var(src, tgt); // Inverted! Sets source = target
  ```
  `MOV source -> target` defines `src` with `tgt`'s value instead of defining `tgt` with `src`'s value.
- **Remediation**:
  ```rust
  let src = resolve_reg(builder, meta, source);
  let src_val = builder.use_var(src);
  let tgt = resolve_reg(builder, meta, target);
  builder.def_var(tgt, src_val);
  ```

---

### 2.3 Inverted Wide Multiplication vs Lossy High Flag
- **Location**: [`savm/src/acaot/native/cranelift/irgen/almu/mod.rs:351-352`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/cranelift/irgen/almu/mod.rs#L351-L352)
- **Severity**: **High (Logic Inversion)**
- **Description**:
  `let wide = (eflags & 0x03) == 1;`
  SaVM opcode specification: `01` is Lossy High Multiplication, and `1x` (value $\ge 2$) is Wide Multiplication. Testing `== 1` executes Lossy High as Wide Multiplication, and Wide Multiplication as Lossy Low Multiplication.
- **Remediation**:
  `let wide = (eflags & 0x02) != 0;`

---

### 2.4 Missing Store Arm for Register `r3` (`locsrc == 11`)
- **Location**: [`savm/src/acaot/native/cranelift/irgen/reg/resolve.rs:246-248`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/cranelift/irgen/reg/resolve.rs#L246-L248)
- **Severity**: **High (Compiler Panic)**
- **Description**:
  `resolve_location_src_store_assumedwdt` handles `10` (`r2`) but omits `11` (`r3`), falling through to `unreachable!()`. Any store operation targeting `r3` panics the compiler.
- **Remediation**: Add arm `11 => { let r3 = resolve_reg(builder, meta, 2); let ptr = builder.use_var(r3); ... }`.

---

### 2.5 64-Bit Integer Bitshift Overflow in Mask Generation
- **Location**: [`savm/src/acaot/native/cranelift/irgen/reg/vector.rs:27 & 40`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/cranelift/irgen/reg/vector.rs#L27)
- **Severity**: **High (Panic / Invalid Bitmask)**
- **Description**:
  `let mask = ((1u64 << (single_elem_width * 8)) - 1) << ...;`
  When `single_elem_width == 8` (64-bit integer / `f64`), `single_elem_width * 8 == 64`. `1u64 << 64` panics with overflow in debug builds and is zero in release builds.
- **Remediation**:
  Guard with `let mask = if single_elem_width >= 8 { u64::MAX } else { ... };`.

---

### 2.6 Alignment Shift Overflow on Large Offsets
- **Location**: [`savm/src/acaot/native/cranelift/irgen/reg/resolve.rs:405-417`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/cranelift/irgen/reg/resolve.rs#L405-L417)
- **Severity**: **Medium (Incorrect Alignment / Panic)**
- **Description**:
  In `get_max_alignment`, `let offset_align = 1 << byteoffset.trailing_zeros();` infers `offset_align` as `u8`. For offsets where `trailing_zeros() >= 8` (e.g. 256, 512), `1u8 << 8` overflows to `0`.
- **Remediation**: Compute as `u32` before clamping: `(1u32 << byteoffset.trailing_zeros().min(31)) as u32`.

---

### 2.7 Missing Offset Addition on Pointer Register Loads
- **Location**: [`savm/src/acaot/native/cranelift/irgen/reg/stackload.rs:37-46`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/cranelift/irgen/reg/stackload.rs#L37-L46)
- **Severity**: **Medium (Incorrect Address Arithmetic)**
- **Description**:
  For `locsrc == 10` (`r2`), `offset` is ignored and `builder.use_var(r2)` is returned directly without adding `offset`.
- **Remediation**: Add `let ptr = builder.ins().iadd_imm_u(ptr, offset as i64);`.

---

## 3. LLVM JIT Backend Defects

### 3.1 Unconditional Panic on Pointer-to-Pointer Fast Path in `vcopy`
- **Location**: [`savm/src/acaot/native/llvm_compiler/irgen/almu/vcopy.rs:41-53`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/llvm_compiler/irgen/almu/vcopy.rs#L41-L53)
- **Severity**: **Critical (Guaranteed Panic)**
- **Description**:
  ```rust
  let is_ptr = matches!(store, StoreResolver::Ptr(_, _, _)) && matches!(src, SrcType::RegMap { .. });
  if is_ptr {
    let SrcType::Pointer { .. } = src else { unreachable!(); }; // Guaranteed to panic!
  ```
  `is_ptr` asserts that `src` is `RegMap`. Entering `if is_ptr` then matches `src` against `SrcType::Pointer`, which always fails and hits `unreachable!()`.
- **Remediation**:
  `let is_ptr = matches!(store, StoreResolver::Ptr(_, _, _)) && matches!(src, SrcType::Pointer { .. });`

---

### 3.2 Inverted Wide Multiplication vs Lossy High Flag
- **Location**: [`savm/src/acaot/native/llvm_compiler/irgen/almu/mod.rs:384, 390`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/llvm_compiler/irgen/almu/mod.rs#L384)
- **Severity**: **High (Logic Inversion)**
- **Description**:
  `let wide = (eflags & 0x03) == 1;`
  Same inversion bug as Cranelift backend (Lossy High is treated as Wide; Wide is treated as Lossy).
- **Remediation**:
  `let wide = (eflags & 0x02) != 0;`

---

### 3.3 Invalid `SExt`/`ZExt` Type Width Assertion Failure
- **Location**: [`savm/src/acaot/native/llvm_compiler/irgen/almu/mod.rs:396-405`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/llvm_compiler/irgen/almu/mod.rs#L396-L405)
- **Severity**: **High (LLVM Crash)**
- **Description**:
  In `handle_mul`:
  ```rust
  let wide_vector_type = LLVMTypeOf(src1); // BUG! Same type as src1
  LLVMBuildSExt(meta.builder, src1, wide_vector_type, LLVM_VAR_NAME.0);
  ```
  `wide_vector_type` is set to the source type. Calling `LLVMBuildSExt`/`ZExt` with destination bitwidth equal to source bitwidth triggers an LLVM assertion crash.
- **Remediation**: Set `wide_vector_type` to `if count == 1 { wide_elem_type } else { LLVMVectorType(wide_elem_type, count) }`.

---

### 3.4 Invalid Funnel Shift Arguments in Rotate Intrinsic
- **Location**: [`savm/src/acaot/native/llvm_compiler/irgen/almu/vbit.rs:84 & 97`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/llvm_compiler/irgen/almu/vbit.rs#L84)
- **Severity**: **High (LLVM Crash)**
- **Description**:
  1. `let typ = LLVMTypeOrWidth::Width(typetag);` triggers `unreachable!()` for signed types ($\ge 4$).
  2. `llvm.fshl` and `llvm.fshr` require 3 arguments `(a, a, shift)` for rotate operations. Passing 2 arguments `&mut [src1, src2]` causes LLVM to abort.
- **Remediation**: Use `LLVMTypeOrWidth::Type(typetag)` and pass `&mut [src1, src1, src2]`.

---

### 3.5 Invalid LLVM Intrinsic Names and Argument Counts in `vcnt`
- **Location**: [`savm/src/acaot/native/llvm_compiler/irgen/almu/vcnt.rs:40-48`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/llvm_compiler/irgen/almu/vcnt.rs#L40-L48)
- **Severity**: **High (LLVM Crash)**
- **Description**:
  - `llvm.clrsb` does not exist as an LLVM intrinsic.
  - `llvm.ctz` does not exist (the intrinsic name is `llvm.cttz`).
  - `llvm.ctlz` and `llvm.cttz` require 2 arguments `(src, is_zero_poison)`. Passing 1 argument fails.

---

### 3.6 Calling Instruction as Intrinsic in `vfop.rs`
- **Location**: [`savm/src/acaot/native/llvm_compiler/irgen/almu/vfop.rs:80`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/llvm_compiler/irgen/almu/vfop.rs#L80)
- **Severity**: **High (LLVM Crash)**
- **Description**:
  `meta.call_intrinsic("llvm.fneg", ...)` fails because `fneg` is an LLVM instruction, not an intrinsic function.
- **Remediation**: Use `LLVMBuildFNeg(meta.builder, src1, LLVM_VAR_NAME.0)`.

---

### 3.7 Pointer Loaded as Sub-64-bit Integer in Atomic Operations
- **Location**: [`savm/src/acaot/native/llvm_compiler/irgen/almu/atomic.rs:49, 76, 112, 167`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/native/llvm_compiler/irgen/almu/atomic.rs#L49)
- **Severity**: **High (LLVM Type Error / Invalid Address)**
- **Description**:
  Pointers are loaded using the atomic data type (`LLVMTypeOrWidth::Type(typedata)`). When `typedata` is `u8` (width 1), the memory pointer is loaded as an 8-bit integer, corrupting address calculations.
- **Remediation**: Load pointer operands using pointer / 64-bit integer type (`LLVMTypeOrWidth::Type(0)`).

---

## 4. Pickle IR & Bytecode Reader Defects

### 4.1 Inverted Boolean Return in Atomic CAS
- **Location**: [`savm/src/acaot/pickle/implementation/almu/atomic.rs:196-198`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/implementation/almu/atomic.rs#L196-L198)
- **Severity**: **High (Logic Inversion)**
- **Description**:
  ```rust
  let [out, succ] = <$b>::from_ptr(pt)
    .compare_exchange_weak(expected, stored, order1, order2)
    .map_or_else(|e| [e, !0], |x| [x, 0]);
  ```
  Returns `!0` (TRUE) when `compare_exchange` fails (`Err(e)`), and `0` (FALSE) when it succeeds (`Ok(x)`), exactly inverting CAS success and failure.
- **Remediation**:
  `map_or_else(|e| [e, 0], |x| [x, !0])`

---

### 4.2 Severe Bitfield Decoding Errors in `parse_vfcast`
- **Location**: [`savm/src/acaot/pickle/reader/cast.rs:64-75`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/reader/cast.rs#L64-L75)
- **Severity**: **High (Bytecode Corrupted)**
- **Description**:
  - `let type_int = (flags >> 8) as u8 & 0x03;` uses a 2-bit mask on a 3-bit field (`0..7`), stripping the signed bit and corrupting all signed conversions (`i64`, `i32`, `i16`, `i8`).
  - `type_float` and `(type_initial, type_final)` both read `(flags >> 9) & 0x01` (middle bit of `type_int`), ignoring `f width` at bit 11 and `op` at bit 12.
- **Remediation**:
  ```rust
  let type_int = ((flags >> 8) as u8) & 0x07;
  let type_float = match ((flags >> 11) as u8) & 0x01 { 0 => 8, 1 => 9, _ => unreachable!() };
  let (type_initial, type_final) = match ((flags >> 12) as u8) & 0x01 {
    0 => (type_float, type_int),
    1 => (type_int, type_float),
    _ => unreachable!(),
  };
  ```

---

### 4.3 Float Type Decoded from Register Index in `parse_vfma`
- **Location**: [`savm/src/acaot/pickle/reader/fp.rs:93-99`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/reader/fp.rs#L93-L99)
- **Severity**: **High (Logic Error)**
- **Description**:
  `let fptype = ((flags >> 12) & 0x01) as u8;`
  Bits 12..15 of `flags` are `Src1` register index. If `Src1` is an odd register (r1, r3, r5, r7), `fptype` is forced to `f32`; if even, `f64`, completely ignoring `pickle.u3`.
- **Remediation**:
  `let fptype = pickle.u3 & 0x01;`

---

### 4.4 Register Field Misalignment in `vbit`, `vrot`, and `vsh`
- **Location**: [`savm/src/acaot/pickle/reader/vbit.rs:49-66 & 121-138`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/reader/vbit.rs#L49) and [`savm/src/acaot/pickle/reader/vsh.rs:24-26`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/reader/vsh.rs#L24-L26)
- **Severity**: **High (Bytecode Decoding Error)**
- **Description**:
  In `parse_vbit`, `parse_vrot`, and `parse_vsh`, `src1` is extracted from bits 0..3 (`Target1`), and `tgt` is extracted from bits 12..15 (`Width`/`TypeTag`). `Src1` at bits 8..11 is never read.
- **Remediation**:
  ```rust
  let src1 = ((flags >> 8) as u8) & 0x0F;
  let src2 = ((flags >> 4) as u8) & 0x0F;
  let tgt = (flags as u8) & 0x0F;
  ```

---

### 4.5 Out-of-Bounds Working Set Read in `parse_vdataop`
- **Location**: [`savm/src/acaot/pickle/reader/vfop.rs:21-22`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/reader/vfop.rs#L21-L22)
- **Severity**: **High (OOB Read / Panic)**
- **Description**:
  `let of_tgt = wspickle!(meta, start = 12, stop = 16, i32);`
  `vdataop` only extracts 12 bytes into the working set (`0..4` count, `4..8` of_src1, `8..12` of_tgt). Reading `12..16` reads past the payload buffer.
- **Remediation**:
  `let of_tgt = wspickle!(meta, start = 8, stop = 12, i32);`

---

### 4.6 Corrupted `countbit` Mask in `vneg` and `vabs`
- **Location**: [`savm/src/acaot/pickle/implementation/almu/vops.rs:22-30`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/implementation/almu/vops.rs#L22-L30)
- **Severity**: **High (Incorrect Operand)**
- **Description**:
  `let countbit = ((flags >> 4) & 0x01) as u8;`
  Reads bit 4 (bit 0 of `Target1`) instead of bit 3 (`count bit`). When `Target1` is an odd register, `count` is read from `(*task).r1.u32` instead of the working set payload.
- **Remediation**:
  `let countbit = ((flags >> 3) & 0x01) as u8;`

---

### 4.7 Shift Amount Treated as Vector (Out-of-Bounds Reads)
- **Location**: [`savm/src/acaot/pickle/implementation/almu/vsh.rs:22-38`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/implementation/almu/vsh.rs#L22-L38)
- **Severity**: **High (OOB Memory Read)**
- **Description**:
  `s2` (shift amount) is read inside the loop via `s2.add(idx)`. Because shift amount is a scalar, when `count > 1`, this reads out-of-bounds past `src2`.
- **Remediation**:
  Read `s2` once as a scalar outside the loop: `let $b = ptr::read_unaligned(s2);`.

---

### 4.8 Count Leading Sign Bits (`cls`) Implemented as `leading_ones()`
- **Location**: [`savm/src/acaot/pickle/implementation/almu/vcnt.rs:60-62`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/implementation/almu/vcnt.rs#L60-L62)
- **Severity**: **Medium (Logic Mismatch)**
- **Description**:
  `cls` is implemented as `a.leading_ones() as _`. Count Leading Sign bits is different from Count Leading Ones (for positive numbers, `leading_ones()` returns 0).
- **Remediation**: Count matching consecutive bits following the sign bit.

---

### 4.9 Pointer Stride Mismatch in `call_vcmp`
- **Location**: [`savm/src/acaot/pickle/implementation/mod.rs:508-521`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/implementation/mod.rs#L508-L521)
- **Severity**: **High (Memory Layout Corruption)**
- **Description**:
  In `call_vcmp`, pointers are advanced via `src1.add(additive as _)` where `src1` is `*mut QuadPackedData` (8 bytes). For sub-64-bit types (e.g. `u8`), it advances by 8 bytes per iteration instead of 1 byte, skipping elements.
- **Remediation**: Advance pointers by `additive * size_of::<T>()`.

---

### 4.10 Bytecode Desynchronization in `handle_atomic` and `handle_vcnt`
- **Location**: [`savm/src/acaot/pickle/mod.rs:104-121`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/mod.rs#L104-L121) and [`lines 220-236`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/acaot/pickle/mod.rs#L220-L236)
- **Severity**: **High (Bytecode Extraction Desync)**
- **Description**:
  - In `handle_atomic`: `flags_offset_v0_v1` reads 4 bytes, shifting `ordering2` and all offset bytes (`of_v0`, `of_v1`, `of_v2`, `of_v3`) out of alignment between `pickle/mod.rs` and `reader/mod.rs`.
  - In `handle_vcnt`: The 1-byte alignment field is omitted from extraction before `count` (4 bytes), throwing off all downstream bytecode extraction offsets.

---

## 5. Management & Runtime Subsystem Defects

### 5.1 Dropped Stop Signals on Full Channel
- **Location**: [`savm/src/management/schedule.rs:68-88`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/management/schedule.rs#L68-L88)
- **Severity**: **High (Thread Leak / Deadlock)**
- **Description**:
  `_ = tx_critical.try_send((0, 0, true));`
  If the channel is full, `try_send` silently drops the shutdown message. Worker threads may miss the signal and stay alive indefinitely, preventing runtime shutdown.
- **Remediation**: Track shutdown state per queue and guarantee delivery when queue is exhausted.

---

### 5.2 Initial Worker Queue Tier Scheduling Bug
- **Location**: [`savm/src/management/jit.rs:36 & 64`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/management/jit.rs#L36)
- **Severity**: **Medium (Scheduling Bug)**
- **Description**:
  `tx.try_send((*x, compilers_len - 1, false))`
  Pre-populating worker queues hardcodes tier `compilers_len - 1` (top tier) for the first 20 items in fastlane and public queues, bypassing tiered compilation.
- **Remediation**: Pass the initial tier index (0) into worker spawn functions.

---

### 5.3 32 KiB Thread Stack Allocation for JIT Management
- **Location**: [`savm/src/lib.rs:308-313`](file:///E:/GitHub/ahqrt-asm-sa/savm/src/lib.rs#L308-L313)
- **Severity**: **High (Stack Overflow Risk)**
- **Description**:
  `stack_size(32 * 1024)` assigns 32 KiB for `"JIT Management"`. `management_main` executes rayon parallel iterators, ahash maps, and complex JIT scheduling loops; 32 KiB easily causes stack overflows on Windows.
- **Remediation**: Increase stack size to at least 512 KiB or 1 MiB (`1024 * 1024`).

---

## 6. Summary Matrix

| ID | Module / File | Line Range | Severity | Defect Description |
|---|---|---|---|---|
| **1.1** | `acaot/acdag/fixedvec.rs` | 17–24, 91 | **Critical** | Zero-capacity allocation / deallocation triggers Rust allocator UB |
| **1.2** | `sync/mod.rs` | 108–113 | **Critical** | `fncall_prep` clobbers scratchpad pointer; caller & callee alias memory |
| **1.3** | `sync/mod.rs` | 108–125 | **High** | Missing bounds checks on call depth `cindex` (overflow / underflow) |
| **1.4** | `sync/mod.rs` | 139–159, 208 | **High** | Nested `fncall` corrupts caller's `WorkingSet.relocmap` and `jmp` |
| **1.5** | `management/cinder/mod.rs` | 146–157 | **High** | `unwrap_unchecked()` on `None` for missing relocation symbols |
| **2.1** | `cranelift/irgen/almu/libcall.rs` | 37 | **Critical** | 4-byte slice conversion into `u64` panics unconditionally |
| **2.2** | `cranelift/irgen/mod.rs` | 158–165 | **High** | `MOV` defines source with target value (inverted assignment) |
| **2.3** | `cranelift/irgen/almu/mod.rs` | 351–352 | **High** | Inverted Wide multiplication vs Lossy High flag decoding |
| **2.4** | `cranelift/irgen/reg/resolve.rs` | 246–248 | **High** | Missing store handler for register `r3` (`locsrc == 11`) |
| **2.5** | `cranelift/irgen/reg/vector.rs` | 27, 40 | **High** | 64-bit integer mask bitshift overflow (`1u64 << 64`) |
| **2.6** | `cranelift/irgen/reg/resolve.rs` | 405–417 | **Medium** | Alignment bitshift overflow on offsets $\ge 256$ |
| **2.7** | `cranelift/irgen/reg/stackload.rs` | 37–46 | **Medium** | Missing offset addition for pointer register `locsrc == 10` |
| **3.1** | `llvm_compiler/irgen/almu/vcopy.rs` | 41–53 | **Critical** | `is_ptr` fast path hits `unreachable!()` unconditionally |
| **3.2** | `llvm_compiler/irgen/almu/mod.rs` | 384, 390 | **High** | Inverted Wide multiplication vs Lossy High flag decoding |
| **3.3** | `llvm_compiler/irgen/almu/mod.rs` | 396–405 | **High** | Invalid `SExt`/`ZExt` destination width equal to source width |
| **3.4** | `llvm_compiler/irgen/almu/vbit.rs` | 84, 97 | **High** | Rotate intrinsic argument count & `TypeOrWidth::Width` panic |
| **3.5** | `llvm_compiler/irgen/almu/vcnt.rs` | 40–48 | **High** | Non-existent LLVM intrinsic names (`llvm.clrsb`, `llvm.ctz`) |
| **3.6** | `llvm_compiler/irgen/almu/vfop.rs` | 80 | **High** | `llvm.fneg` called as intrinsic instead of LLVM instruction |
| **3.7** | `llvm_compiler/irgen/almu/atomic.rs` | 49, 76, 112 | **High** | Pointer loaded as 8-bit integer in atomic operations |
| **4.1** | `pickle/implementation/almu/atomic.rs` | 196–198 | **High** | Atomic CAS returns inverted boolean result |
| **4.2** | `pickle/reader/cast.rs` | 64–75 | **High** | 2-bit mask on 3-bit int type & wrong bit shifts for float conversions |
| **4.3** | `pickle/reader/fp.rs` | 93–99 | **High** | Float type decoded from `Src1` register bits instead of `pickle.u3` |
| **4.4** | `pickle/reader/vbit.rs` & `vsh.rs` | 49–66, 24–26 | **High** | Register field misalignment in `vbit`, `vrot`, and `vsh` |
| **4.5** | `pickle/reader/vfop.rs` | 21–22 | **High** | Out-of-bounds working set slice read in `parse_vdataop` |
| **4.6** | `pickle/implementation/almu/vops.rs` | 22–30 | **High** | Corrupted `countbit` mask in `vneg` and `vabs` |
| **4.7** | `pickle/implementation/almu/vsh.rs` | 22–38 | **High** | Shift amount treated as vector, causing out-of-bounds reads |
| **4.8** | `pickle/implementation/almu/vcnt.rs` | 60–62 | **Medium** | Count leading sign bits (`cls`) implemented as `leading_ones()` |
| **4.9** | `pickle/implementation/mod.rs` | 508–521 | **High** | Pointer stride in `call_vcmp` advances by 8B regardless of element size |
| **4.10** | `pickle/mod.rs` | 104–121, 220 | **High** | Bytecode desynchronization in `handle_atomic` and `handle_vcnt` |
| **5.1** | `management/schedule.rs` | 68–88 | **High** | Dropped stop signals on full channel leading to thread leaks |
| **5.2** | `management/jit.rs` | 36, 64 | **Medium** | Initial worker queue items compiled at top tier |
| **5.3** | `savm/src/lib.rs` | 308–313 | **High** | 32 KiB thread stack allocation risks stack overflow |
