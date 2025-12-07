use core::ffi::c_void;
use std::{mem::ManuallyDrop, ptr::null_mut};

use crate::{boxed::RTSafeBoxWrapper, map::HeapStructure};

/// the first argument actually is a reference to the VM Struct lol
pub type DispatchFn = extern "C" fn(*mut c_void, *mut VMTaskState, u64);

/// If the pointer goes NULL
/// You must use the 1st unsigned integer to get the module
/// The second integer represents the `.text` block
pub union Instruction {
  pub module: u64,
  pub fn_: (u64, DispatchFn),
}

#[repr(C)]
pub union MutableOrHeap {
  pub ptr: *mut HeapStructure,
  pub data: ManuallyDrop<HeapStructure>,
}

impl MutableOrHeap {
  #[inline(always)]
  pub fn nullify(&mut self) {
    self.ptr = null_mut();
  }

  #[inline(always)]
  pub fn heap(&mut self) -> &mut HeapStructure {
    unsafe { &mut self.data }
  }
}

/// This is the program registry state
/// This is created for each thread executed
/// under the Runtime
///
/// PLEASE DO NOT CHANGE THE LAYOUT, IT MAY BREAK
#[repr(C)]
pub struct VMTaskState {
  // Immutables or OWNED data
  pub r1: HeapStructure, // Dangerous, may hold dangling data + it is a pointer
  pub r2: HeapStructure, // Dangerous, may hold dangling data + it is a pointer
  pub r3: HeapStructure, // Dangerous, may hold dangling data + it is a pointer
  pub r4: HeapStructure,
  // Mutables or Immutables or OWNED data like r1, r2, r3, r4
  pub r5: MutableOrHeap,        // Dangerous, may hold dangling data
  pub r6: MutableOrHeap,        // Dangerous, may hold dangling data
  pub super_: *mut VMTaskState, // If this is NULL, this means this itself is the super task
  pub curline: usize,
}

const _CHECK_REGISTER_SET_SIZE: usize =
  size_of::<VMTaskState>() - 1 * size_of::<usize>() - size_of::<*mut VMTaskState>();
pub const REGISTER_SET_SIZE: usize = 3 * size_of::<HeapStructure>()
  + 2 * size_of::<MutableOrHeap>()
  + size_of::<*mut RTSafeBoxWrapper>();

const _OUT: bool = REGISTER_SET_SIZE == _CHECK_REGISTER_SET_SIZE;

const _ENSURE_VMTASKSTATE_IS_VALID: () = debug_assert!(_OUT);

unsafe impl Send for VMTaskState {}

macro_rules! instruction {
  (
    $(
      $data:expr => $name:ident
    ),*$(,)?
  ) => {
    pastey::paste! {
      $(
        pub const [<INSTRUCTION_ $name:upper>]: u8 = $data;

        #[allow(non_upper_case_globals, dead_code)]
        const [<_FORFIXING_ $data>]: u8 = 0;
      )*

      pub fn parse_instrution(inst: &str) -> Option<u8> {
        match inst {
          $(
            stringify!($name) => {
              Some($data)
            }
          )*
          _ => None
        }
      }
    }
  };
}

// instruction! {
//   0x01 => mark,
//   0x02 => clr,
//   0x03 => clrs,
//   0x04 => alloc,
//   0x05 => aralc,
//   0x06 => load,
//   0x07 => free,
//   0x08 => own,
//  // Math (r1, r2 are treated as 64-bit unsigned values)
//   0x09 => add,
//   0x0A => sub,
//   0x0B => mul,
//   0x0C => div,
//   0x0D => rem,
//   // Math Mut (r1 is 64-bit unsigned, r6 is output pointer)
//   0x0E => add_mut,
//   0x0F => sub_mut,
//   0x10 => mul_mut,
//   0x11 => div_mut,
//   0x12 => rem_mut,

//   // Bitwise (r1, r2 are treated as 64-bit unsigned values)
//   0x13 => and,
//   0x14 => or,
//   0x15 => xor,
//   // Bitwise Mut (r6 is output pointer)
//   0x16 => and_mut,
//   0x17 => or_mut,
//   0x18 => xor_mut,
//   0x19 => cmp, // Compare (Original Opcode)

//   // Bitshift (Original Opcodes)
//   0x1A => shl,
//   0x1B => shr,
//   // Bitshift Mut (Original Opcodes)
//   0x1C => shl_mut,
//   0x1D => shr_mut,

//   // Control
//   0x1E => jmp,
//   0x1F => jz,
//   0x20 => jnz,
//   0x21 => ret,

//   // FFI
//   0x22 => libcall,
//   // Threading (Future)
//   0x23 => spawn,
//   0x24 => join,
//   0x25 => yield,
//   0x26 => await,

//   // ALU: Pointer Arithmetic (Original Opcodes)
//   // Treat r1, r2 as pointers
//   0x27 => add_ptr,
//   0x28 => sub_ptr,
//   0x29 => mul_ptr,
//   0x2A => div_ptr,

//   // Library only instructions
//   0x2B => super_mov,
//   0x2C => super_alloc,
//   0x2D => super_aralc,
//   0x2E => super_load,
//   0x2F => super_free,
//   0x30 => super_own,

//   // --- NEW INSTRUCTIONS (Assigned Opcode 0x33 onwards) ---

//   // Bitwise Pointer (ALU)
//   // Treat r1, r2 as pointers
//   0x31 => and_ptr,
//   0x32 => or_ptr,
//   0x33 => xor_ptr,

//   // Bitshift Pointer (ALU)
//   // Treat r1 as pointer, r2 as shift amount
//   0x34 => shl_ptr,
//   0x35 => shr_ptr,

//   // remainer (but as pointer)
//   0x36 => rem_ptr,

//   // Put value in register r1 or r2 or r3
//   0x37 => put_reg,
//   // Copy a 64-bit value to r1 or r2 or r3
//   0x38 => copy,
//   0x39 => cmp_ptr
// }

instruction! {
    // --- I. Memory Management (Heap/Stack) ---
    // (r1, r2, r3, r4, r5, r6 typically used for operands/pointers/sizes)
    0x01 => alloc, // Allocate data from a register and move it into memory. The register should be treated as containing invalid data
    0x02 => aralc, // TODO
    0x03 => load,  // Load data from heap, `load 0u8 <addr>` to load from heap
    0x04 => free,  // Drop the associated complex data from the heap. Running on primitive types is undefined behaviour
    0x05 => own,   // Move value from Heap to r<n>
    0x06 => mark,  // Mark for jump instructions
    0x07 => clr,   // Clear a register/value. NOTE: This does not drop associated complex heap value
    0x08 => clrs,  // Clear all the registers/values. NOTE: This does not drop associated complex heap value

    // --- II. Basic Data & Register Operations ---
    0x09 => put_reg, // Put immediate value in any register. `put_reg <register u8> <bytes u8; can it 1/2/4/8> <bytes>`
    0x0A => copy,    // Copy 64-bit (primitive values / pointer) value to any register

    // --- III. Control Flow ---
    0x0B => jmp,     // Unconditional jump
    0x0C => jz,      // Jump if zero
    0x0D => jnz,     // Jump if not zero
    0x0E => ret,     // Return from procedure
    0x0F => cmp,     // Compare 64-bit unsigned values (r1, r2)
    0x10 => cmp_ptr, // Compare pointers (r1, r2)

    // --- IV. Arithmetic (r1, r2 are 64-bit unsigned inputs) ---
    0x11 => add,
    0x12 => sub,
    0x13 => mul,
    0x14 => div,
    0x15 => rem, // Remainder

    // --- V. Arithmetic Mutating (r6 is output pointer) ---
    0x16 => add_mut,
    0x17 => sub_mut,
    0x18 => mul_mut,
    0x19 => div_mut,
    0x1A => rem_mut,

    // --- VI. Bitwise (r1, r2 are 64-bit unsigned inputs) ---
    0x1B => and,
    0x1C => or,
    0x1D => xor,

    // --- VII. Bitwise Mutating (r6 is output pointer) ---
    0x1E => and_mut,
    0x1F => or_mut,
    0x20 => xor_mut,

    // --- VIII. Bitshift (r1 is value, r2 is shift amount) ---
    0x21 => shl, // Shift left
    0x22 => shr, // Shift right

    // --- IX. Bitshift Mutating (r6 is output pointer) ---
    0x23 => shl_mut,
    0x24 => shr_mut,

    // --- X. Pointer Arithmetic (r1, r2 treated as pointers and deferenced to get 64-bit unsigned memory) ---
    0x25 => add_ptr,
    0x26 => sub_ptr,
    0x27 => mul_ptr,
    0x28 => div_ptr,
    0x29 => rem_ptr,

    // --- XI. Pointer Bitwise & Bitshift (ALU, r1, r2 are dereferenced to get 64-bit unsigned numbers and then acted on) ---
    0x2A => and_ptr,
    0x2B => or_ptr,
    0x2C => xor_ptr,
    0x2D => shl_ptr, // r1 is pointer, r2 is shift amount
    0x2E => shr_ptr, // r1 is pointer, r2 is shift amount

    // --- XII. System & Threading ---
    0x2F => libcall,
    0x30 => spawn,   // Threading: Create new thread
    0x31 => join,    // Threading: Wait for thread
    0x32 => yield,   // Threading: Give up time
    0x33 => await,   // Threading: Asynchronous wait

    // --- XIII. Library-Only Instructions (Super Privileged) ---
    0x34 => super_mov,
    0x35 => super_alloc,
    0x36 => super_aralc,
    0x37 => super_load,
    0x38 => super_free,
    0x39 => super_own,
}
