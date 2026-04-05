use std::{mem::offset_of, ptr::copy_nonoverlapping};

use crate::acaot::{
  native::cranelift::{CompilerMeta, irgen::reg::TypeOrWidth},
  pickle::def::{
    PICKLE_OPCODE_HINT, PICKLE_OPCODE_JIF, PICKLE_OPCODE_JMP, PICKLE_OPCODE_MARK,
    PICKLE_OPCODE_REG, PickleInstruction,
  },
};
use cranelift::prelude::{
  Block, FunctionBuilder, InstBuilder, MemFlags, TrapCode,
  isa::TargetIsa,
  types::{I64, I64X8},
};
use sart::ctr::VMTaskState;

mod reg;

pub fn compile(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: &[PickleInstruction],
  isa: &dyn TargetIsa,
) {
  // Start from block
  builder.switch_to_block(meta.blockv0);

  let mut idx = 0usize;

  loop {
    println!("Idx rn - {idx}");

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
      PICKLE_OPCODE_REG => {
        let reg = op.u1;
        let marker = u64::from_ne_bytes(meta.ws[0..8].try_into().unwrap());

        // After this `JMP` we are required to create a whole new block
        let val = builder.ins().iconst(I64, marker.cast_signed());
        let var = reg::resolve_reg(builder, meta, reg);

        builder.def_var(var, val);
      }
      PICKLE_OPCODE_JIF => {
        let intent = op.u1;
        let relocation_src = op.u2;
        let width = op.u3;

        let offset = i32::from_ne_bytes(unsafe { meta.ws[0..4].try_into().unwrap_unchecked() });
        let marker = u64::from_ne_bytes(unsafe { meta.ws[4..12].try_into().unwrap_unchecked() });

        let src = reg::resolve_location_src_load(
          builder,
          meta,
          TypeOrWidth::Width(width),
          relocation_src,
          None,
          offset,
          1,
        );
      }
      _ => {}
    }

    idx += 1;
  }

  // Since this is the last block, jump to epilogue
  builder.ins().jump(meta.epilogue, []);

  // Write trap
  {
    builder.switch_to_block(meta.trap);

    builder.ins().trap(TrapCode::unwrap_user(30));
  }

  // Write the async-epilogue
  {
    builder.switch_to_block(meta.async_epilogue);

    println!("SaVM Warn : ASYNC Epilogue is UNFINISHED");
    // if let Some(r8) = meta.r8 {
    //   let r8_val = builder.use_var(r8);
    //   builder.ins().store(
    //     MemFlags::new().with_aligned(),
    //     meta.vmtaskstate,
    //     r8_val,
    //     offset_of!(VMTaskState, r8) as i32,
    //   );
    // }

    // Copy the whole of well - yeah - i had almost forgot
    // scratchpad (192-bytes at 64-byte alignment)
    let mf = MemFlags::new().with_aligned().with_can_move();

    let stack_scratchpad_addr = builder
      .ins()
      .stack_addr(isa.pointer_type(), meta.scratchpad, 0);
    let scratchpad_addr = builder.ins().load(
      isa.pointer_type(),
      mf,
      meta.vmtaskstate,
      offset_of!(VMTaskState, scratchpad) as i32,
    );

    for offset in [0, 64, 128] {
      let lr = builder.ins().load(I64X8, mf, stack_scratchpad_addr, offset);

      builder.ins().store(mf, lr, scratchpad_addr, offset);
    }

    builder.ins().return_(&[]);
  }

  // Write the epilogue (SYNC)
  {
    builder.switch_to_block(meta.epilogue);

    if let Some(r7) = meta.r7 {
      let r7_val = builder.use_var(r7);
      builder.ins().store(
        MemFlags::new().with_aligned(),
        r7_val,
        meta.vmtaskstate,
        offset_of!(VMTaskState, r7) as i32,
      );
    }

    if let Some(r8) = meta.r8 {
      let r8_val = builder.use_var(r8);
      builder.ins().store(
        MemFlags::new().with_aligned(),
        r8_val,
        meta.vmtaskstate,
        offset_of!(VMTaskState, r8) as i32,
      );
    }

    builder.ins().return_(&[]);
  }

  builder.seal_all_blocks();
}
