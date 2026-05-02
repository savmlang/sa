use std::{
  collections::HashMap,
  io::{Read, Seek},
};

use sart::ctr::*;

use crate::acaot::pickle::def::{PickleInstruction, *};

pub mod def;
pub mod reader;

pub mod implementation;

pub struct PickleWorker<T: Seek + Read> {
  pub bytecode: T,
  pub out: Vec<PickleInstruction>,
  pub jump: HashMap<u64, usize, ahash::RandomState>,
}

trait Extract: Read + Sized {
  fn extract<const N: usize>(&mut self) -> [u8; N] {
    self.read_array::<N>().unwrap()
  }
}

trait ToNE {
  fn swap_if_be(self) -> Self;
}

impl<const N: usize> ToNE for [u8; N] {
  #[allow(unused)]
  fn swap_if_be(mut self) -> Self {
    #[cfg(target_endian = "big")]
    self.reverse();

    self
  }
}

impl<T: Read + Sized> Extract for T {}

impl<T: Seek + Read> PickleWorker<T> {
  pub fn pass1(&mut self) {
    while let Ok([opcode]) = self.bytecode.read_array::<1>() {
      match opcode {
        INSTRUCTION_MOV => self.handle_mov(),
        INSTRUCTION_REG => self.handle_reg(),
        INSTRUCTION_MARK => self.handle_mark(),
        INSTRUCTION_JMP => self.handle_jmp(),
        INSTRUCTION_JIF => self.handle_jif(),
        INSTRUCTION_VCMP => self.handle_vcmp(),
        INSTRUCTION_SCRATCH => self.handle_scratch(),
        INSTRUCTION_VCOPY => self.handle_vcopy(),
        INSTRUCTION_VADD => self.handle_vop(PICKLE_OPCODE_VADD),
        INSTRUCTION_VADDF => self.handle_vopf(PICKLE_OPCODE_VADDF),
        INSTRUCTION_VSUB => self.handle_vop(PICKLE_OPCODE_VSUB),
        INSTRUCTION_VSUBF => self.handle_vopf(PICKLE_OPCODE_VSUBF),
        INSTRUCTION_VMUL => self.handle_vop(PICKLE_OPCODE_VMUL),
        INSTRUCTION_VMULF => self.handle_vopf(PICKLE_OPCODE_VMULF),
        INSTRUCTION_VDIVF => self.handle_vopf(PICKLE_OPCODE_VDIVF),
        INSTRUCTION_DIV => self.handle_div_like(PICKLE_OPCODE_DIV),
        INSTRUCTION_REM => self.handle_div_like(PICKLE_OPCODE_REM),
        INSTRUCTION_CAST => self.handle_cast(),
        INSTRUCTION_VFCAST => self.handle_vdata_op(PICKLE_OPCODE_VFCAST),
        INSTRUCTION_VNEG => self.handle_vdata_op(PICKLE_OPCODE_VNEG),
        INSTRUCTION_VABS => self.handle_vdata_op(PICKLE_OPCODE_VABS),
        INSTRUCTION_VFOP => self.handle_vdata_op(PICKLE_OPCODE_VFOP),
        INSTRUCTION_VBIT => self.handle_vbit_op(PICKLE_OPCODE_VBIT),
        INSTRUCTION_VROT => self.handle_vrot(),
        INSTRUCTION_VSH => self.handle_vsh(),
        INSTRUCTION_VCNT => self.handle_vcnt(),
        INSTRUCTION_VMINIMAX => self.handle_vminimax(),
        INSTRUCTION_VFMA => self.handle_vfma(),
        INSTRUCTION_SYNCCALL => self.handle_synccall(),
        INSTRUCTION_ASYNCCALL => self.handle_asynccall(),
        INSTRUCTION_SPAWN => self.handle_spawn(),
        INSTRUCTION_TASK => self.handle_task(),
        INSTRUCTION_ATOMIC => self.handle_atomic(),
        _ => unreachable!(),
      }
    }
  }

  // [Sub Opcode (2-bits)] [type (3-bit)] [ordering (3-bits)] [offset v0 (i8)] [offset v1 (i8)]
  // [offset v2 (i8)] [offset v3 (i8)] [instruction defined (16-bit)]
  fn handle_atomic(&mut self) {
    let opcode = PICKLE_OPCODE_ATOMIC;

    let flags_offset_v0_v1 = self.bytecode.extract::<4>();

    let mut cp = [0; 6];
    cp[0..1].copy_from_slice(&[flags_offset_v0_v1[3]]);
    cp[1..3].copy_from_slice(&self.bytecode.extract::<2>());
    cp[3..5].copy_from_slice(&self.bytecode.extract::<2>().swap_if_be());
    self.emit_copy_bytes(opcode, cp);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags_offset_v0_v1[0],
      u2: flags_offset_v0_v1[1],
      u3: flags_offset_v0_v1[2],
    });
  }

  // `task <sub op (4-bits)> <def (4-bits)> <marker (64-bit)>`
  fn handle_task(&mut self) {
    let opcode = PICKLE_OPCODE_TASK;

    let [op] = self.bytecode.extract::<1>().swap_if_be();

    let marker = self.bytecode.extract::<8>().swap_if_be();
    self.emit_copy_bytes(opcode, marker);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: op >> 4,   // OP
      u2: op & 0x0F, // Def
      u3: 0,
    });
  }

  // `spawn <section id as u64> u16%[<flags (6-bits)> <scratchpad start index (5-bits)> <total to copy (5-bits)>]`
  fn handle_spawn(&mut self) {
    let opcode = PICKLE_OPCODE_SPAWN;

    let sectionid = self.bytecode.extract::<8>().swap_if_be();

    let [o0, o1] = self.bytecode.extract::<2>().swap_if_be();

    self.emit_copy_bytes(opcode, sectionid);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: o0,
      u2: o1,
      u3: 0,
    });
  }

  fn handle_asynccall(&mut self) {
    let opcode = PICKLE_OPCODE_ASYNCCALL;
    let sectionid = self.bytecode.extract::<8>().swap_if_be();
    let marker = self.bytecode.extract::<8>().swap_if_be();

    let mut copy = [0u8; 16];
    copy[0..8].copy_from_slice(&sectionid);
    copy[8..16].copy_from_slice(&marker);
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: 0,
      u2: 0,
      u3: 0,
    });
  }

  fn handle_synccall(&mut self) {
    let opcode = PICKLE_OPCODE_SYNCCALL;

    let [regignore] = self.bytecode.extract::<1>();
    let sectionid = self.bytecode.extract::<8>().swap_if_be();

    self.emit_copy_bytes(opcode, sectionid);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: regignore,
      u2: 0,
      u3: 0,
    });
  }

  // `vfma <flags as u16> <padding [8bits]> <count in u32> <base src1 as i32> <base src2 as i32> <base src3 as i32> <base target1 as i32>`
  fn handle_vfma(&mut self) {
    let opcode = PICKLE_OPCODE_VFMA;
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
    let [memflags] = self.bytecode.extract::<1>();

    let mut copy = [0u8; 20];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[16..20].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: memflags,
    });
  }

  // `vminimax <flags as u16> <padding (7-bits)> <Max (1-bit)> <count in u32> <base src1 as i32> <base src2 as i32> <base target1 as i32>`
  fn handle_vminimax(&mut self) {
    let opcode = PICKLE_OPCODE_VMINIMAX;
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
    let [maxbit] = self.bytecode.extract::<1>();

    let mut copy = [0u8; 8];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..5].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    copy[5..6].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    copy[6..7].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: maxbit,
    });
  }

  // `vcnt <flags as u16 [2 bytes]> <count in u32> <base src1 as i32> <base target1 as i32>`
  fn handle_vcnt(&mut self) {
    let opcode = PICKLE_OPCODE_VCNT;
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();

    let mut copy = [0u8; 6];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..5].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    copy[5..6].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: 0,
    });
  }

  // `vsh <flags as u16> <padding (6-bits)> <op bit (1-bit)> <count bit (1-bit)> <count in u32> <base src1 as i32> <amount i.e. src2 as i32> <base target1 as i32>`
  fn handle_vsh(&mut self) {
    let opcode = PICKLE_OPCODE_VSH;
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();

    let mut copy = [0u8; 8];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..5].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    copy[5..6].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    copy[6..7].copy_from_slice(&self.bytecode.extract::<1>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: 0,
    });
  }

  // `vrot <flags as u16> <padding (7-bits)> <rotation bit (1-bit)> <count in u32> <base src1 as i32> <amount src i.e. src2 as i32> <base target1 as i32>`
  fn handle_vrot(&mut self) {
    let opcode = PICKLE_OPCODE_VROT;
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
    let [rotation] = self.bytecode.extract::<1>();

    let mut copy = [0u8; 16];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: rotation,
    });
  }

  // `vb* <flags as u16> <padding (7-bits)> <count bit (1-bit)> <count in u32> <base src1 as i32> <base src2 as i32> <base target1 as i32>`
  fn handle_vbit_op(&mut self, opcode: u8) {
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();
    let [count] = self.bytecode.extract::<1>();

    let mut copy = [0u8; 16];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: count,
    });
  }

  // eg
  // `vneg <flags as u16 [2 bytes]> <count in u32> <base src1 as i32> <base target1 as i32>`
  fn handle_vdata_op(&mut self, opcode: u8) {
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();

    let mut copy = [0u8; 12];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: 0,
    });
  }

  // `cast <flags as u16> <base src1 as i32> <base target1 as i32>`
  fn handle_cast(&mut self) {
    let opcode = PICKLE_OPCODE_CAST;
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();

    let mut copy = [0u8; 8];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: 0,
    });
  }

  // `div <args as u16> <base src1 as i32> <base src2 as i32> <base target1 as i32>`
  fn handle_div_like(&mut self, opcode: u8) {
    let [args1, args2] = self.bytecode.extract::<2>().swap_if_be();

    let mut copy: [u8; 12] = [0; 12];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: args1,
      u2: args2,
      u3: 0,
    });
  }

  // `vaddf <flags as u16> <count in u32> <base src1 as i32> <base src2 as i32> <base target1 as i32>`
  fn handle_vopf(&mut self, opcode: u8) {
    let [flags1, flags2] = self.bytecode.extract::<2>().swap_if_be();

    let mut copy: [u8; 16] = [0; 16];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: flags1,
      u2: flags2,
      u3: 0,
    });
  }

  // `vadd <flags as u32 [4 bytes]> <count in u32> <base src1 as i32> <base src2 as i32> <base target1 as i32>`
  fn handle_vop(&mut self, opcode: u8) {
    let mut copy: [u8; 20] = [0; 20];
    //  Flags
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    // Count
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    // Src1
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    // Src2
    copy[12..16].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());

    // Target1
    let target1 = self.bytecode.extract::<4>().swap_if_be();
    copy[16..20].copy_from_slice(&target1);

    self.emit_copy_bytes(opcode, copy);

    self.out.push(PickleInstruction {
      opcode: opcode,
      u1: 0,
      u2: 0,
      u3: 0,
    });
  }

  fn handle_vcopy(&mut self) {
    let [mflags, src_flags] = self.bytecode.extract::<2>();

    let mut copy = [0u8; 12];
    copy[0..4].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[4..8].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    copy[8..12].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    self.emit_copy_bytes(PICKLE_OPCODE_VCOPY, copy);

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_VCOPY,
      u1: mflags,
      u2: src_flags,
      u3: 0,
    });
  }

  fn handle_scratch(&mut self) {
    let data = self.bytecode.extract::<2>().swap_if_be();

    // 1st 2bits are class
    let data = u16::from_ne_bytes(data);

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_SCRATCH,
      u1: (data >> 14) as _,          // class
      u2: ((data >> 8) as u8) & 0x3F, // High bits
      u3: (data & 0xFF) as u8,        // Low bits
    });
  }

  fn handle_vcmp(&mut self) {
    let [r0] = self.bytecode.extract::<1>();

    let operation = r0 & 0xFF;

    let mut total: [u8; 18] = [0; 18];
    total[0..2].copy_from_slice(&self.bytecode.extract::<2>().swap_if_be());
    total[2..6].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    total[6..10].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    total[10..14].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());
    total[14..18].copy_from_slice(&self.bytecode.extract::<4>().swap_if_be());

    self.emit_copy_bytes(PICKLE_OPCODE_VCMP, total);

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_VCMP,
      u1: operation & 0x1F,
      u2: operation >> 5,
      u3: 0,
    });
  }

  fn handle_jif(&mut self) {
    // cond = intent bit | width (2bit) | 1 bit padding | location src (4bit)
    let [cond] = self.bytecode.read_array::<1>().unwrap();
    let offset = i32::from_le_bytes(self.bytecode.read_array::<4>().unwrap()).to_ne_bytes();

    let marker = u64::from_le_bytes(self.bytecode.read_array::<8>().unwrap()).to_ne_bytes();

    let mut combined_array: [u8; 12] = [0; 12];
    combined_array[..4].copy_from_slice(&offset);
    combined_array[4..].copy_from_slice(&marker);

    self.emit_copy_bytes(PICKLE_OPCODE_JIF, combined_array);

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_JIF,
      u1: cond >> 7,          // Intent
      u2: cond & 0x0F,        // Location src
      u3: (cond >> 5) & 0x03, // Width bits
    });
  }

  fn handle_jmp(&mut self) {
    let data = u64::from_le_bytes(self.bytecode.read_array::<8>().unwrap()).to_ne_bytes();

    self.emit_copy_bytes::<6>(PICKLE_OPCODE_JMP, data[0..6].try_into().unwrap());

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_JMP,
      u1: data[6],
      u2: data[7],
      u3: 0,
    });
  }

  fn handle_mark(&mut self) {
    let marker = u64::from_le_bytes(self.bytecode.read_array::<8>().unwrap());

    let data = marker.to_ne_bytes();

    // This is a JIT Hint ONLY!
    self.emit_copy_bytes(PICKLE_OPCODE_MARK, data);

    // JIT Hint only
    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_MARK,
      u1: 0,
      u2: 0,
      u3: 0,
    });

    self.jump.insert(marker, self.out.len() - 1);
  }

  fn handle_reg(&mut self) {
    let [register] = self.bytecode.read_array().expect("");

    let data_ne: [u8; 8] =
      u64::from_le_bytes(self.bytecode.read_array::<8>().expect("")).to_ne_bytes();

    self.emit_copy_bytes(PICKLE_OPCODE_REG, data_ne);

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_REG,
      u1: register,
      u2: 0,
      u3: 0,
    });
  }

  fn emit_copy_bytes<const N: usize>(&mut self, opcode: u8, data: [u8; N]) {
    // Ensure N is even so we don't lose a byte in integer division
    debug_assert!(N % 2 == 0, "Payload must be word-aligned");
    debug_assert!(N <= 255, "Payload size {} exceeds u8 capacity", N);

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_HINT,
      u1: opcode,
      u2: (N / 4) as u8 + ((N % 4) / 2) as u8,
      // total bytes
      u3: N as u8,
    });

    let mut chunks_4 = data.chunks_exact(4);
    for chunk in chunks_4.by_ref() {
      self.out.push(PickleInstruction {
        opcode: chunk[0],
        u1: chunk[1],
        u2: chunk[2],
        u3: chunk[3],
      });
    }

    // 3. Process remaining 2-byte chunks (the "remainder")
    let chunks_2 = chunks_4.remainder().chunks_exact(2);
    for chunk in chunks_2 {
      self.out.push(PickleInstruction {
        opcode: chunk[0],
        u1: chunk[1],
        u2: 0,
        u3: 0,
      });
    }
  }

  fn handle_mov(&mut self) {
    let [registers] = self.bytecode.read_array().expect("");

    let source = registers >> 4;
    let target = registers & 0x0F;

    self.out.push(PickleInstruction {
      opcode: PICKLE_OPCODE_MOV,
      u1: source,
      u2: target,
      u3: 0,
    });
  }
}
