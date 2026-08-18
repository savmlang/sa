# Specification: Function-Wide Backtracking Register Allocator

## 1. System Architecture & Constraints

- **Physical Registers:** `R1` through `R8` (64-bit / 8-Byte width per register).
- **Scratchpad Space:** Fixed at **192 Bytes**. Referenced via frame/stack pointer conventions.
- **Largepad Space:** Variable-sized spill area for allocations exceeding Scratchpad capacity.
- **MIR Representation:** Both inputs and outputs are structured as MIR structs to simplify allocation tracking.
- **MIR:** The Register allocator has to be written to allocate the registers in MIR and lower to LocSrc under MIR

---

## 2. Calling Convention & ABI Rules

### Inputs

- **`size(input) <= 16B`:** Passed directly in registers `R7` and `R8`.
  - `R7` alignment is guaranteed up to **8 Bytes max**.
- **`size(input) > 16B`:** Placed in the **Scratchpad**. `R7` holds the pointer address to this memory block.

### Outputs

- **`size(output) <= 16B`:** Always returned in registers `R7` and `R8`.

---

## 3. Allocation Strategy & Optimizations

The register allocator must implement a **function-wide backtracking strategy** incorporating the following four optimizations:

### Optimization A: Intelligent Intra-Register Fitting

Multiples of smaller data types must be packed into a single physical register using offset-based addressing.

- **Offset Definition:** The number of units of the target type's width preceding the value within the register layout.
- **Packing Example (`[I32][I16][I8][I8]` packed into `R1`):**
  - `I32` $\rightarrow$ `R1` at **Offset 0** (0 $\times$ 32-bit units preceding).
  - `I16` $\rightarrow$ `R1` at **Offset 2** (2 $\times$ 16-bit units preceding).
  - `I8` $\rightarrow$ `R1` at **Offset 6** (6 $\times$ 8-bit units preceding).
  - `I8` $\rightarrow$ `R1` at **Offset 7** (7 $\times$ 8-bit units preceding).

### Optimization B: Liveness Tracking & Value Elimination

- Perform full liveness analysis across basic blocks.
- SSA values that are **not** explicitly passed as block arguments AND are **not** live-out to subsequent blocks must be evicted or overwritten to reuse register space.

### Optimization C: Partial Spilling

- Values with long live ranges under high register pressure may be partially spilled to memory for a sub-range of their lifetime and reloaded into physical registers only when needed.

### Optimization D: Tiered Memory Fallback

- Allocations must prioritize placement in **Physical Registers** $\rightarrow$ **Scratchpad (<= 192B)** $\rightarrow$ **Largepad**, in that strict order. Largepad must only be used if Scratchpad capacity is fully exhausted.

---

## 4. Deliverables

1. **Allocator Implementation:** Fully implement the backtracking register allocator adhering to the ABI and optimization guidelines above.
2. **Examples (`examples/regalloc.rs`):** Completely replace `examples/regalloc.rs` with **12 comprehensive test cases** covering:
   - Basic ABI parameter passing (<=16B vs >16B).
   - Intra-register sub-word packing and offset calculations.
   - Liveness-based register reuse across basic block boundaries.
   - Partial spilling during heavy register pressure.
   - Scratchpad-to-Largepad fallback transitions.
