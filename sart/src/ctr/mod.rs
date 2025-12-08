use core::ffi::c_void;
use std::ptr::null_mut;

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
#[derive(Clone, Copy)]
pub union MutableOrHeap {
  pub ptr: *mut HeapStructure,
  pub data: HeapStructure,
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

instruction! {
    // --- I. Memory Management (Heap/Stack) ---
    // (r1, r2, r3, r4, r5, r6 typically used for operands/pointers/sizes)
    0x01 => alloc, // Allocate data from a register and move it into memory. The register should be treated as containing invalid data
    0x02 => aralc, // TODO: FUTURE SPEC
    0x03 => load,  // Load data from heap, `load 0u8 <addr>` to load from heap
    0x04 => free,  // Drop the associated complex data from the heap. Running on primitive types is undefined behaviour
    0x05 => own,   // Move value from Heap to r<n>
    0x06 => mark,  // Mark for jump instructions
    0x07 => clr,   // Clear a register/value. NOTE: This does not drop associated complex heap value
    0x08 => clrs,  // Clear all the registers/values. NOTE: This does not drop associated complex heap value

    // --- II. Basic Data & Register Operations ---
    0x09 => put_reg, // TODO: FUTURE SPEC; Put immediate value in any register. `put_reg <register u8> <total bytes u8; can it 1/2/4/8> <bytes>`

    // --- III. Control Flow ---
    0x0A => jmp,     // Unconditional jump
    0x0B => jz,      // Jump if zero
    0x0C => jnz,     // Jump if not zero
    0x0D => ret,     // TODO: FUTURE SPEC; Return from procedure
    0x0E => cmp,     // Compare 64-bit unsigned values (r1, r2)
    // Mov data b/w registers
    0x0F => mov,

    // --- IV. Arithmetic (r1, r2 are 64-bit unsigned inputs) ---
    0x10 => add,
    0x11 => sub,
    0x12 => mul,
    0x13 => div,
    0x14 => rem, // Remainder

    // --- V. Arithmetic Mutating (r6 is output pointer) ---
    0x15 => add_mut,
    0x16 => sub_mut,
    0x17 => mul_mut,
    0x18 => div_mut,
    0x19 => rem_mut,

    // --- VI. Bitwise (r1, r2 are 64-bit unsigned inputs) ---
    0x1A => and,
    0x1B => or,
    0x1C => xor,

    // --- VII. Bitwise Mutating (r6 is output pointer) ---
    0x1D => and_mut,
    0x1E => or_mut,
    0x1F => xor_mut,

    // --- VIII. Bitshift (r1 is value, r2 is shift amount) ---
    0x20 => shl, // Shift left
    0x21 => shr, // Shift right

    // --- IX. Bitshift Mutating (r6 is output pointer) ---
    0x22 => shl_mut,
    0x23 => shr_mut,

    // --- X. Pointer Arithmetic (r1, r2 treated as pointers and deferenced to get 64-bit unsigned memory) ---
    0x24 => add_ptr,
    0x25 => sub_ptr,
    0x26 => offset_ptr,

    // --- XII. System & Threading ---
    0x27 => libcall,
    0x28 => spawn,   // Threading: Create new thread
    0x29 => join,    // TODO: FUTURE SPEC; Threading: Wait for thread
    0x2A => yield,   // Threading: Give up time
    0x2B => await,   // Threading: Asynchronous wait

    // --- XIII. Library-Only Instructions (Accesses Super Context) ---
    0x2C => super_mov, // Load register data from the caller. Usage `super_mov <target register> <load from register>`
}
