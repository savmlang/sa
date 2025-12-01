use std::os::raw::c_void;

use crate::boxed::{RTSafeBoxWrapper, drop_rtbox};

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
pub union RegistryValue {
  pub ptr: *const c_void,
  pub data: u64,
}

/// This is the program registry state
/// This is created for each thread executed
/// under the Runtime
///
/// PLEASE DO NOT CHANGE THE LAYOUT, IT MAY BREAK
#[repr(C)]
pub struct VMTaskState {
  // Immutables
  // All of these are secretely `*const c_void` as well
  // You better know the types
  pub r1: RegistryValue, // Dangerous, may hold dangling data + it is a pointer
  pub r2: RegistryValue, // Dangerous, may hold dangling data + it is a pointer
  pub r3: RegistryValue, // Dangerous, may hold dangling data + it is a pointer
  // Mutables
  pub r4: *mut c_void, // Dangerous, may hold dangling data
  pub r5: *mut c_void, // Dangerous, may hold dangling data
  // Owned Data
  // RTSafeBoxWrapper is a *mut c_void like type that theretically transfers ownership
  pub r6: *mut RTSafeBoxWrapper,
  pub super_: *mut VMTaskState, // If this is NULL, this means this itself is the super task
  pub curline: usize,
}

impl VMTaskState {
  pub unsafe fn set_r6(&mut self, data: *mut RTSafeBoxWrapper) {
    unsafe {
      if !self.r6.is_null() {
        drop_rtbox(self.r6);
      }

      self.r6 = data;
    }
  }
}

impl Drop for VMTaskState {
  fn drop(&mut self) {
    unsafe {
      if !self.r6.is_null() {
        drop_rtbox(self.r6);
      }
    }
  }
}

const _CHECK_REGISTER_SET_SIZE: usize =
  size_of::<VMTaskState>() - 1 * size_of::<usize>() - size_of::<*mut VMTaskState>();
pub const REGISTER_SET_SIZE: usize = 3 * size_of::<RegistryValue>()
  + 2 * size_of::<*mut c_void>()
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
  0x02 => clr,
  0x03 => clrs,
  0x04 => alloc,
  0x05 => aralc,
  0x06 => load,
  0x07 => free,
  0x08 => own,
  // Math
  // r1, r2 are treated as 64-bit unsigned values
  0x09 => add,
  0x0A => sub,
  0x0B => mul,
  0x0C => div,
  0x0D => rem,
  // Math Mut
  // r1 is treated as 64-bit unsigned values
  // r6 is a pointer where the output is finally stored
  0x0E => add_mut,
  0x0F => sub_mut,
  0x10 => mul_mut,
  0x11 => div_mut,
  0x12 => rem_mut,
  // Bitwise
  0x13 => and,
  0x14 => or,
  0x15 => xor,
  // Bitwise Mut
  0x16 => and_mut,
  0x17 => or_mut,
  0x18 => xor_mut,
  // Compare
  0x19 => cmp,
  // Bitshift
  0x1A => shl,
  0x1B => shr,
  // Bitshift Mut
  0x1C => shl_mut,
  0x1D => shr_mut,
  // Control
  0x1E => jmp,
  0x1F => jz,
  0x20 => jnz,
  0x21 => ret,
  // FFI
  0x22 => libcall,
  // Threading
  0x23 => fork,
  0x24 => join,
  0x25 => yield,
  0x26 => await,
  // ALU:
  // Treat r1, r2 as pointers
  0x27 => add_ptr,
  0x28 => sub_ptr,
  0x29 => mul_ptr,
  0x2A => div_ptr,
  // Library only instructions
  // These are equal to `load`, `free`, `own` but they
  // access the super context to do all of them

  // These instructions are only available under the library context
  0x2B => super_mov,
  0x2C => super_clr,
  0x2D => super_clrs,
  0x2E => super_alloc,
  0x2F => super_aralc,
  0x30 => super_load,
  0x31 => super_free,
  0x32 => super_own,
}
