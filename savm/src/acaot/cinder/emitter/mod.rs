use core::slice;
use std::hint::unreachable_unchecked;

use ahash::HashMap;

use crate::{
  BytecodeResolver,
  acaot::{
    cinder::{
      CompilerMeta, INST_RETURN_P_ID, Resolved, StencilMap,
      emit::{
        Stencil, inst_call, inst_call_jmpable, inst_div, inst_mark, inst_nop, inst_rem, inst_vadd,
        inst_vaddf, inst_vdivf, inst_vmul, inst_vmulf, inst_vsub, inst_vsubf, inst_wsput,
      },
      stencilify,
    },
    pickle::{
      def::{
        PICKLE_OPCODE_DIV, PICKLE_OPCODE_HINT, PICKLE_OPCODE_JIF, PICKLE_OPCODE_JMP,
        PICKLE_OPCODE_MARK, PICKLE_OPCODE_REM, PICKLE_OPCODE_TASK, PICKLE_OPCODE_VADD,
        PICKLE_OPCODE_VADDF, PICKLE_OPCODE_VDIVF, PICKLE_OPCODE_VMUL, PICKLE_OPCODE_VMULF,
        PICKLE_OPCODE_VSUB, PICKLE_OPCODE_VSUBF, PickleInstruction,
      },
      reader::{
        au::{ARITH, DIVLIKE, parse_arith, parse_divlike},
        fp::{VFP, parse_vfp},
      },
    },
  },
  kvwrap::SaVMJumpWrapImpl,
};

pub fn emit<T: BytecodeResolver + Send + Sync + 'static>(
  comptime: &mut CompilerMeta,
  entries: &mut HashMap<u64, u64>,
) {
  let pickle = comptime.pickle;

  let mut idx = 0;
  let len = pickle.len();

  let mut ws: &[u8] = &[];
  while idx < len {
    let op = unsafe { pickle.get_unchecked(idx) };
    let opcode = op.opcode;

    if opcode == PICKLE_OPCODE_HINT {
      let bytes = op.u3 as usize;

      unsafe {
        ws = slice::from_raw_parts(pickle.as_ptr().add(idx + 1) as *const u8, bytes);
      };

      let total_inst = op.u2 as usize;
      idx += total_inst + 1;
      continue;
    }

    let (ws_idx, _) = comptime.entrymap.insert_full(Box::from(ws));

    let mut ignore = || {
      let index = comptime.mapping.len();
      comptime.mapping.push(stencilify(&[StencilMap {
        stencil: &inst_nop,
        resolve: stencilify(&[("NEXT", Resolved::NextMainID)]),
      }]));
      index
    };
    match opcode {
      // MARK is ignored
      // HINT is preprocessed
      PICKLE_OPCODE_HINT => {
        _ = ignore();
      }
      PICKLE_OPCODE_MARK => {
        let marker =
          u64::from_ne_bytes(unsafe { ws.get_unchecked(0..8).try_into().unwrap_unchecked() });

        let idx = if marker & (1 << 63) != 0 {
          let idx = comptime.mapping.len();
          comptime.mapping.push(stencilify(&[StencilMap {
            stencil: &inst_mark,
            resolve: stencilify(&[
              ("MARKER", Resolved::Immediate { imm: marker }),
              ("RETURN_RESUME", INST_RETURN_P_ID),
              ("NEXT", Resolved::NextMainID),
            ]),
          }]));
          idx
        } else {
          ignore()
        };
        _ = entries.insert(marker, idx as _);
      }

      PICKLE_OPCODE_JMP => {
        let marker =
          u64::from_ne_bytes(unsafe { ws.get_unchecked(0..8).try_into().unwrap_unchecked() });

        comptime.mapping.push(stencilify(&[StencilMap {
          stencil: &inst_nop,
          resolve: stencilify(&[("NEXT", Resolved::ResolveLaterStencilID { marker })]),
        }]));
      }

      PICKLE_OPCODE_VADD => emit_varith(ws, &inst_vadd, comptime),
      PICKLE_OPCODE_VSUB => emit_varith(ws, &inst_vsub, comptime),
      PICKLE_OPCODE_VMUL => emit_varith(ws, &inst_vmul, comptime),

      PICKLE_OPCODE_DIV => emit_divlike(op, ws, &inst_div, comptime),
      PICKLE_OPCODE_REM => emit_divlike(op, ws, &inst_rem, comptime),

      PICKLE_OPCODE_VADDF => emit_varith_vfp(op, ws, &inst_vaddf, comptime),
      PICKLE_OPCODE_VSUBF => emit_varith_vfp(op, ws, &inst_vsubf, comptime),
      PICKLE_OPCODE_VMULF => emit_varith_vfp(op, ws, &inst_vmulf, comptime),
      PICKLE_OPCODE_VDIVF => emit_varith_vfp(op, ws, &inst_vdivf, comptime),

      // JMP-ABLE
      PICKLE_OPCODE_JIF | PICKLE_OPCODE_TASK => {
        let marker = match opcode {
          PICKLE_OPCODE_JIF => {
            u64::from_ne_bytes(unsafe { ws.get_unchecked(4..12).try_into().unwrap_unchecked() })
          }

          PICKLE_OPCODE_TASK => {
            unimplemented!("Yet to implement tasks")
          }

          _ => unsafe { unreachable_unchecked() },
        };

        let pickle_verify = comptime.jumps.get(&marker).unwrap();
        comptime.mapping.push(stencilify(&[
          StencilMap {
            stencil: &inst_wsput,
            resolve: stencilify(&[
              ("SIZE", Resolved::Immediate { imm: ws.len() as _ }),
              ("SRC", Resolved::WorkingSetId { idx: ws_idx }),
              ("NEXT", Resolved::NextStencil),
            ]),
          },
          StencilMap {
            stencil: &inst_call_jmpable,
            resolve: stencilify(&[
              ("PICKLE_IDX", Resolved::Immediate { imm: idx as _ }),
              (
                "VERIFY",
                Resolved::Immediate {
                  imm: pickle_verify as u64,
                },
              ),
              ("NEXT", Resolved::NextMainID),
              (
                "CALL",
                Resolved::Immediate {
                  imm: unsafe { comptime.crt.as_ptr().add(opcode as _).addr() as _ },
                },
              ),
              ("TAKEN_JUMP", Resolved::ResolveLaterStencilID { marker }),
            ]),
          },
        ]));
      }

      opcode => {
        comptime.mapping.push(stencilify(&[
          StencilMap {
            stencil: &inst_wsput,
            resolve: stencilify(&[
              ("SIZE", Resolved::Immediate { imm: ws.len() as _ }),
              ("SRC", Resolved::WorkingSetId { idx: ws_idx }),
              ("NEXT", Resolved::NextStencil),
            ]),
          },
          StencilMap {
            stencil: &inst_call,
            resolve: stencilify(&[
              ("PICKLE_IDX", Resolved::Immediate { imm: idx as _ }),
              ("NEXT", Resolved::NextMainID),
              (
                "CALL",
                Resolved::Immediate {
                  imm: unsafe { comptime.crt.as_ptr().add(opcode as _).addr() as _ },
                },
              ),
            ]),
          },
        ]));
      }
    }

    ws = &[];
    idx += 1;
  }
}

#[inline(always)]
fn emit_varith_vfp(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let VFP {
    instdef,
    count,
    datatype,
    src1,
    src2,
    tgt,
    of_src1,
    of_src2,
    of_tgt,
  } = parse_vfp(pickle, ws);

  let dt_src1_src2_tgt_count = (datatype as u64)
    | ((src1 as u64) << 8)
    | ((src2 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1.cast_unsigned() as u64) | ((of_src2.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      (
        "INSTDEFINED",
        Resolved::Immediate {
          imm: instdef as u64,
        },
      ),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_tgt.cast_unsigned() as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

#[inline(always)]
fn emit_varith(ws: &[u8], stencil: &'static Stencil, comptime: &mut CompilerMeta) {
  let ARITH {
    datatype,
    count,
    instdefined,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_arith(ws);

  let dt_src1_src2_tgt_count = (datatype as u64)
    | ((src1 as u64) << 8)
    | ((src2 as u64) << 16)
    | ((tgt as u64) << 24)
    | ((count as u64) << 32);
  let of_src1_src2 = (of_src1.cast_unsigned() as u64) | ((of_src2.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT_COUNT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt_count,
        },
      ),
      (
        "INSTDEFINED",
        Resolved::Immediate {
          imm: instdefined as _,
        },
      ),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_tgt.cast_unsigned() as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}

#[inline(always)]
fn emit_divlike(
  pickle: &PickleInstruction,
  ws: &[u8],
  stencil: &'static Stencil,
  comptime: &mut CompilerMeta,
) {
  let DIVLIKE {
    datatype,
    src1,
    of_src1,
    src2,
    of_src2,
    tgt,
    of_tgt,
  } = parse_divlike(pickle, ws);

  let dt_src1_src2_tgt =
    (datatype as u64) | ((src1 as u64) << 8) | ((src2 as u64) << 16) | ((tgt as u64) << 24);
  let of_src1_src2 = (of_src1.cast_unsigned() as u64) | ((of_src2.cast_unsigned() as u64) << 32);

  comptime.mapping.push(stencilify(&[StencilMap {
    stencil,
    resolve: stencilify(&[
      (
        "DATATYPE_SRC1_SRC2_TGT",
        Resolved::Immediate {
          imm: dt_src1_src2_tgt,
        },
      ),
      ("OF_SRC1_SRC2", Resolved::Immediate { imm: of_src1_src2 }),
      (
        "OF_TGT",
        Resolved::Immediate {
          imm: of_tgt.cast_unsigned() as u64,
        },
      ),
      ("NEXT", Resolved::NextMainID),
    ]),
  }]));
}
