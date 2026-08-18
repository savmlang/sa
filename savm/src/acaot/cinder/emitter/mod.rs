use core::slice;
use std::hint::unreachable_unchecked;

use ahash::HashMap;

use crate::{
  BytecodeResolver,
  acaot::{
    cinder::{
      CompilerMeta, INST_RETURN_P_ID, Resolved, StencilMap,
      emit::{inst_call, inst_call_jmpable, inst_mark, inst_nop, inst_wsput},
      stencilify,
    },
    pickle::def::{
      PICKLE_OPCODE_HINT, PICKLE_OPCODE_JIF, PICKLE_OPCODE_JMP, PICKLE_OPCODE_MARK,
      PICKLE_OPCODE_TASK,
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

      // JMP-ABLE
      PICKLE_OPCODE_JMP | PICKLE_OPCODE_JIF | PICKLE_OPCODE_TASK => {
        let marker = match opcode {
          PICKLE_OPCODE_JMP => {
            u64::from_ne_bytes(unsafe { ws.get_unchecked(0..8).try_into().unwrap_unchecked() })
          }

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
