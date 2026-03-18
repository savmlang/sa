use crate::acaot::pickle::implementation::*;

/// Pickle is our own internal NE implementation
/// for converting variable width bytecode into pickle
#[repr(C)]
pub struct PickleInstruction {
  pub opcode: u8,
  pub u1: u8,
  pub u2: u8,
  pub u3: u8,
}

macro_rules! opcodes {
  (
    $(
      $id:expr => $opcode:ident
    ),*
  ) => {
    pastey::paste! {
      $(
        pub const [<PICKLE_OPCODE_ $opcode>]: u8 = $id;
      )*

      const TOTAL_ITEMS: usize = $(
        data(stringify!($opcode)) +
      )* 0;

      const fn data(_: &str) -> usize {
        1
      }

      pub(crate) const PICKLE_DISPATCH_TABLE: [ResolveFn; TOTAL_ITEMS] = [
        $(
          [<call_ $opcode:lower>]
        ),*
      ];
    }
  };
}

opcodes! {
  // Must be set before WS_PUT
  // it hints the JIT compiler about the instruction that comes after
  // WS_PUT
  //
  // u1 = opcode after WS_PUT
  // u2 = total numbers of WS_PUT
  0 => HINT,
  // Working Set put
  // Put 16 bites (u2, u3 in native-endian)
  // with offset specified by u1 in multiple of (16bits)
  //
  // This is meant to allow the interpreter to handle
  // Large Instructions that have more than 24-bits of
  // operands
  1 => WS_PUT,
  // u1 = source register id
  // u2 = target register id
  2 => MOV,
  // This is followed by 3 WS_PUT (to move the 48-bits)
  // in Native-Endian order
  3 => REG,
  // 4xWS_PUT
  // NE order
  // Hint ONLY!
  4 => MARK,
  // 3xWS_PUT
  // NE order
  5 => JMP,
  // 6xWS_PUT
  // NE order
  // [2x WS_PUT offset] [2xWS_PUT marker]
  6 => JIF,
  // 9xWS_PUT
  7 => VCMP,
  8 => SCRATCH,
  9 => VCOPY,
  10 => VADD,
  11 => VADDF,
  12 => VSUB,
  13 => VSUBF,
  14 => VMUL,
  15 => VMULF,
  16 => VDIVF,
  17 => DIV,
  18 => REM,
  19 => CAST,
  20 => VNEG,
  21 => VABS,
  22 => VFOP,
  23 => VFCAST,
  24 => VBIT,
  25 => VROT,
  26 => VSH,
  27 => VCNT,
  28 => VMINIMAX,
  29 => VFMA,
  30 => SYNCCALL,
  31 => ASYNCCALL,
  32 => SPAWN,
  33 => TASK,
  34 => ATOMIC
}
