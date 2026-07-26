use std::{mem::offset_of, ptr::copy_nonoverlapping};

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::{
      almu::{handle_reg, mark_optflow},
      reg::{TypeOrWidth, resolve_reg},
    },
  },
  pickle::def::{
    PICKLE_OPCODE_ATOMIC, PICKLE_OPCODE_CAST, PICKLE_OPCODE_DIV, PICKLE_OPCODE_HINT,
    PICKLE_OPCODE_JIF, PICKLE_OPCODE_JMP, PICKLE_OPCODE_MARK, PICKLE_OPCODE_MOV, PICKLE_OPCODE_REG,
    PICKLE_OPCODE_REM, PICKLE_OPCODE_SCRATCH, PICKLE_OPCODE_SPAWN, PICKLE_OPCODE_SYNCCALL,
    PICKLE_OPCODE_VABS, PICKLE_OPCODE_VADD, PICKLE_OPCODE_VADDF, PICKLE_OPCODE_VBIT,
    PICKLE_OPCODE_VCMP, PICKLE_OPCODE_VCNT, PICKLE_OPCODE_VCOPY, PICKLE_OPCODE_VDIVF,
    PICKLE_OPCODE_VFCAST, PICKLE_OPCODE_VFMA, PICKLE_OPCODE_VFOP, PICKLE_OPCODE_VMINIMAX,
    PICKLE_OPCODE_VMUL, PICKLE_OPCODE_VMULF, PICKLE_OPCODE_VNEG, PICKLE_OPCODE_VROT,
    PICKLE_OPCODE_VSH, PICKLE_OPCODE_VSUB, PICKLE_OPCODE_VSUBF, PICKLE_OPCODE_WS_PUT,
    PickleInstruction,
  },
};
use cranelift::{
  codegen::ir::{InstBuilderBase, immediates::Imm64, types::I32},
  frontend::Switch,
  prelude::{
    FunctionBuilder, InstBuilder, MemFlagsData as MemFlags, TrapCode, isa::TargetIsa, types::I64,
  },
};
use sart::{
  ctr::{
    FLAGS::FLAG_JUMP_TO_RESUME,
    OPCODES::{OPCODE_JIT_CHECK, OPCODE_OK},
    VMTaskState,
  },
  structures::QuadPackedData,
};

mod almu;
mod reg;

pub fn compile<const SENDBACK: bool>(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: &[PickleInstruction],
  isa: &dyn TargetIsa,

  is_epitome: bool,
) {
  // Start from block
  builder.switch_to_block(meta.blockv0);

  let mut idx = 0usize;

  loop {
    if idx == pickle.len() {
      break;
    }

    let op = pickle[idx];

    match op.opcode {
      // Soak up all in ws
      PICKLE_OPCODE_HINT => {
        let bytes = op.u3 as usize;

        unsafe {
          copy_nonoverlapping(
            // the next instruction after the OPCODE_HINT is a bytestream
            pickle.as_ptr().add(idx + 1) as *const u8,
            meta.ws.as_mut_ptr(),
            bytes,
          );
        };

        let total_inst = op.u2 as usize;
        idx += total_inst;
      }
      PICKLE_OPCODE_MARK => {
        let marker = u64::from_ne_bytes(meta.ws[0..8].try_into().unwrap());

        let newblock = meta.blockmap.get(&marker).unwrap().current;
        builder.ins().jump(newblock, []);

        builder.switch_to_block(newblock);

        // Ignore this optflow if the compiler is EPITOME
        if !is_epitome && marker & (1 << 63) > 0 {
          mark_optflow(builder, meta, marker);
        }
      }
      PICKLE_OPCODE_JMP => {
        let marker = u64::from_ne_bytes(meta.ws[0..8].try_into().unwrap());

        // After this `JMP` we are required to create a whole new block
        builder
          .ins()
          .jump(meta.blockmap.get(&marker).unwrap().current, &[]);

        let newblock = builder.create_block();
        builder.switch_to_block(newblock);
      }
      PICKLE_OPCODE_REG => handle_reg(&op, meta, builder),
      PICKLE_OPCODE_JIF => {
        let intent = op.u1;
        let relocation_src = op.u2;
        let width = op.u3;

        let offset = i32::from_ne_bytes(unsafe { meta.ws[0..4].try_into().unwrap_unchecked() });
        let marker = u64::from_ne_bytes(unsafe { meta.ws[4..12].try_into().unwrap_unchecked() });

        let &[src] = reg::resolve_location_src_load(
          builder,
          meta,
          TypeOrWidth::Width(width),
          relocation_src,
          None,
          offset,
          1,
        )
        .as_ref() else {
          unreachable!();
        };

        let newblock = builder.create_block();

        let userblock = meta.blockmap.get(&marker).unwrap().current;

        let (then, other) = if intent == 0 {
          // Jump If Zero
          (newblock, userblock)
        } else {
          (userblock, newblock)
        };

        builder.ins().brif(src, then, [], other, []);
        builder.switch_to_block(newblock);
      }
      PICKLE_OPCODE_MOV => {
        let source = op.u1;
        let target = op.u2;

        // Special Pointers
        if source == target && source > 7 {
          let r1 = resolve_reg(builder, meta, 0);
          let val = match source {
            12 => builder.ins().load(
              I64,
              MemFlags::trusted(),
              meta.vmtaskstate,
              offset_of!(VMTaskState, largepad) as i32,
            ),
            _ => unreachable!(),
          };

          builder.def_var(r1, val);
        } else {
          let src = resolve_reg(builder, meta, source);

          let tgt = resolve_reg(builder, meta, target);
          let tgt = builder.use_var(tgt);

          // Do a MOV
          builder.def_var(src, tgt);
        }
      }

      // VCMP
      PICKLE_OPCODE_VCMP => almu::hwnd_vcmp(builder, meta, op),

      // Add, Sub, Mul, Div, Rem
      PICKLE_OPCODE_VADD => almu::hwnd_vadd(builder, meta, op),
      PICKLE_OPCODE_VSUB => almu::hwnd_vsub(builder, meta, op),
      PICKLE_OPCODE_VMUL => almu::hwnd_vmul(builder, meta, op),
      PICKLE_OPCODE_DIV => almu::hwnd_div(builder, meta, op),
      PICKLE_OPCODE_REM => almu::hwnd_rem(builder, meta, op),

      // F - Add, Sub, Mul, Div
      PICKLE_OPCODE_VADDF => almu::handle_vaddf(builder, meta, op),
      PICKLE_OPCODE_VSUBF => almu::handle_vsubf(builder, meta, op),
      PICKLE_OPCODE_VMULF => almu::handle_vmulf(builder, meta, op),
      PICKLE_OPCODE_VDIVF => almu::handle_vdivf(builder, meta, op),

      // FMA
      PICKLE_OPCODE_VFMA => almu::handle_vfma(builder, meta, op),

      // VDATA OP
      PICKLE_OPCODE_VABS => almu::hwnd_vabs(builder, meta, op),
      PICKLE_OPCODE_VNEG => almu::hwnd_vneg(builder, meta, op),
      PICKLE_OPCODE_VBIT => almu::hwnd_vbit(builder, meta, op),
      PICKLE_OPCODE_VROT => almu::hwnd_vrot(builder, meta, op),

      // VSH
      PICKLE_OPCODE_VSH => almu::hwnd_vsh(builder, meta, op),

      // MINIMAX
      PICKLE_OPCODE_VMINIMAX => almu::hwnd_vminimax(builder, meta, op),

      PICKLE_OPCODE_CAST => almu::hwnd_cast(builder, meta, op),
      PICKLE_OPCODE_VFCAST => almu::hwnd_vfcast(builder, meta, op),
      PICKLE_OPCODE_VCNT => almu::hwnd_vcnt(builder, meta, op),
      PICKLE_OPCODE_VFOP => almu::hwnd_vfop(builder, meta, op),
      PICKLE_OPCODE_VCOPY => almu::hwnd_vcopy(builder, meta, op),

      // Yeah - its a libcall. Full stop
      PICKLE_OPCODE_SCRATCH => almu::hwnd_scratch(builder, meta, &op),
      PICKLE_OPCODE_ATOMIC => almu::hwnd_atomic(builder, meta, op),

      PICKLE_OPCODE_SYNCCALL => almu::hwnd_libcall_sync(builder, meta, op),
      PICKLE_OPCODE_SPAWN => almu::hwnd_spawn(builder, meta, op),

      // No-Op
      PICKLE_OPCODE_WS_PUT => {}
      _ => {
        // Figure out impl for 35 other remaining opcodes
      }
    }

    idx += 1;
  }

  // Since this is the last block, jump to epilogue
  builder.ins().jump(meta.epilogue, []);

  // Write jumpmap
  {
    builder.switch_to_block(meta.jumpresolver);

    // Copy Registers
    {
      let vars = [
        (&meta.r1, offset_of!(VMTaskState, r1) as i32),
        (&meta.r2, offset_of!(VMTaskState, r2) as i32),
        (&meta.r3, offset_of!(VMTaskState, r3) as i32),
        (&meta.r4, offset_of!(VMTaskState, r4) as i32),
        (&meta.r5, offset_of!(VMTaskState, r5) as i32),
        (&meta.r6, offset_of!(VMTaskState, r6) as i32),
        (&meta.r7, offset_of!(VMTaskState, r7) as i32),
        (&meta.r8, offset_of!(VMTaskState, r8) as i32),
      ];

      for &(var, offset) in &vars {
        if let Some(r) = var {
          let r_val = builder
            .ins()
            .load(I64, MemFlags::trusted(), meta.vmtaskstate, offset);

          builder.def_var(*r, r_val);
        }
      }
    }

    // Copy Scratchpad
    {
      let scratchpad_ptr = builder.ins().load(
        I64,
        MemFlags::trusted(),
        meta.vmtaskstate,
        offset_of!(VMTaskState, scratchpad) as i32,
      );
      let size = builder.ins().build_imm_const(I64, Imm64::new(192), false);
      let ss = builder
        .ins()
        .stack_addr(isa.pointer_type(), meta.scratchpad, 0);
      builder.call_memcpy(isa.frontend_config(), ss, scratchpad_ptr, size);
    }

    let mut switch = Switch::new();

    for (k, v) in &meta.blockmap {
      switch.set_entry(*k as _, v.current);
    }

    let resume = builder.ins().load(
      I64,
      MemFlags::trusted(),
      meta.vmtaskstate,
      offset_of!(VMTaskState, curline_or_resume) as i32,
    );

    switch.emit(builder, resume, meta.trap);
  }

  // Write trap
  {
    builder.switch_to_block(meta.trap);

    builder.ins().trap(TrapCode::unwrap_user(30));
  }

  // Suspend Epilogue (SYNC)
  // This copies ~320B back to the VM
  {
    builder.switch_to_block(meta.suspend_epilogue);

    // Flush Registers
    {
      let vars = [
        (&meta.r1, offset_of!(VMTaskState, r1) as i32),
        (&meta.r2, offset_of!(VMTaskState, r2) as i32),
        (&meta.r3, offset_of!(VMTaskState, r3) as i32),
        (&meta.r4, offset_of!(VMTaskState, r4) as i32),
        (&meta.r5, offset_of!(VMTaskState, r5) as i32),
        (&meta.r6, offset_of!(VMTaskState, r6) as i32),
        (&meta.r7, offset_of!(VMTaskState, r7) as i32),
        (&meta.r8, offset_of!(VMTaskState, r8) as i32),
      ];

      for &(var, offset) in &vars {
        if let Some(r) = var {
          let r_val = builder.use_var(*r);

          builder
            .ins()
            .store(MemFlags::trusted(), r_val, meta.vmtaskstate, offset);
        }
      }
    }

    // Flush Scratchpad
    {
      let src = builder
        .ins()
        .stack_addr(isa.pointer_type(), meta.scratchpad, 0);
      let size = builder.ins().build_imm_const(
        I64,
        Imm64::new(24 * size_of::<QuadPackedData>() as i64),
        false,
      );

      let scratchpad_ptr = builder.ins().load(
        I64,
        MemFlags::trusted(),
        meta.vmtaskstate,
        offset_of!(VMTaskState, scratchpad) as i32,
      );

      builder.call_memcpy(isa.frontend_config(), scratchpad_ptr, src, size);
    }

    // Flush Largepad
    {
      let largepad_ptr = builder.use_var(meta.largepad);

      builder.ins().store(
        MemFlags::trusted(),
        largepad_ptr,
        meta.vmtaskstate,
        offset_of!(VMTaskState, largepad) as i32,
      );
    }

    // Opcode Sync + FLAGS Sync + Set next jump point
    {
      let opcode_jit_check =
        builder
          .ins()
          .build_imm_const(I32, Imm64::new(OPCODE_JIT_CHECK as _), false);
      builder.ins().store(
        MemFlags::trusted(),
        opcode_jit_check,
        meta.vmtaskstate,
        offset_of!(VMTaskState, opcode) as i32,
      );

      {
        let offset = offset_of!(VMTaskState, flags) as i32;
        let old_flags = builder
          .ins()
          .load(I32, MemFlags::trusted(), meta.vmtaskstate, offset);

        let flag_jump = builder
          .ins()
          .bor_imm_u(old_flags, Imm64::new(FLAG_JUMP_TO_RESUME as i64));
        builder
          .ins()
          .store(MemFlags::trusted(), flag_jump, meta.vmtaskstate, offset);
      }

      let jump_target = builder.block_params(meta.suspend_epilogue)[0];
      builder.ins().store(
        MemFlags::trusted(),
        jump_target,
        meta.vmtaskstate,
        offset_of!(VMTaskState, curline_or_resume) as i32,
      );
    }

    builder.ins().return_(&[]);
  }

  // Write the epilogue (SYNC)
  {
    builder.switch_to_block(meta.epilogue);

    let vars = if SENDBACK {
      &[
        (&meta.r1, offset_of!(VMTaskState, r1) as i32),
        (&meta.r2, offset_of!(VMTaskState, r2) as i32),
        (&meta.r3, offset_of!(VMTaskState, r3) as i32),
        (&meta.r4, offset_of!(VMTaskState, r4) as i32),
        (&meta.r5, offset_of!(VMTaskState, r5) as i32),
        (&meta.r6, offset_of!(VMTaskState, r6) as i32),
        (&meta.r7, offset_of!(VMTaskState, r7) as i32),
        (&meta.r8, offset_of!(VMTaskState, r8) as i32),
      ] as &[_]
    } else {
      &[
        (&meta.r7, offset_of!(VMTaskState, r7) as i32),
        (&meta.r8, offset_of!(VMTaskState, r8) as i32),
      ]
    };

    for &(var, offset) in vars {
      if let Some(r) = var {
        let r_val = builder.use_var(*r);

        builder
          .ins()
          .store(MemFlags::trusted(), r_val, meta.vmtaskstate, offset);
      }
    }

    let opcode_ok = builder
      .ins()
      .build_imm_const(I32, Imm64::new(OPCODE_OK as _), false);
    builder.ins().store(
      MemFlags::trusted(),
      opcode_ok,
      meta.vmtaskstate,
      offset_of!(VMTaskState, opcode) as i32,
    );

    builder.ins().return_(&[]);
  }

  builder.seal_all_blocks();
}
