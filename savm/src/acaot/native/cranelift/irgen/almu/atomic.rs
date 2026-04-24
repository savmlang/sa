use cranelift::{
  codegen::ir::{AtomicRmwOp, Constant, ConstantData},
  prelude::{FunctionBuilder, InstBuilder, IntCC, MemFlags},
};

use crate::acaot::{
  native::cranelift::{
    CompilerMeta,
    irgen::reg::{TypeOrWidth, resolve_location_src_load, resolve_location_src_store},
  },
  pickle::{
    def::PickleInstruction,
    reader::{ATOMIC, ATOMICRmwOp, parse_atomic},
  },
};

pub fn hwnd_atomic(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  #[cfg(target_pointer_width = "64")]
  let pt = TypeOrWidth::Type(0);
  #[cfg(target_pointer_width = "32")]
  let pt = TypeOrWidth::Type(1);

  match parse_atomic(&pickle, &meta.ws) {
    ATOMIC::LOAD {
      typedata,
      ptr_loc,
      ptr_loc_of,
      load_loc,
      load_loc_of,
      ..
    } => {
      let ty = TypeOrWidth::Type(typedata);

      let &[pt_resolved] =
        resolve_location_src_load(builder, meta, pt, ptr_loc, None, ptr_loc_of as _, 1).as_ref()
      else {
        unreachable!();
      };

      let mut tostore =
        resolve_location_src_store(builder, meta, ty, load_loc, None, load_loc_of as _, 1);

      let val = builder
        .ins()
        .atomic_load(ty.clif_mapping().x1, MemFlags::trusted(), pt_resolved);

      tostore.store(builder, 0, val);
      tostore.synchronize(builder, meta);
    }

    ATOMIC::STORE {
      typedata,
      ptr_loc,
      ptr_loc_of,
      val_store_of,
      val_stored_loc,
      ..
    } => {
      let ty = TypeOrWidth::Type(typedata);

      let &[pt_resolved] =
        resolve_location_src_load(builder, meta, pt, ptr_loc, None, ptr_loc_of as _, 1).as_ref()
      else {
        unreachable!();
      };

      let &[tostore] = resolve_location_src_load(
        builder,
        meta,
        ty,
        val_stored_loc,
        None,
        val_store_of as _,
        1,
      )
      .as_ref() else {
        unreachable!();
      };

      builder
        .ins()
        .atomic_store(MemFlags::trusted(), tostore, pt_resolved);
    }

    ATOMIC::CAS {
      typedata,

      ptr_loc,
      ptr_loc_of,

      val_stored_loc,
      val_store_of,

      expected_loc,
      expected_of,

      ret_loc,
      ret_of,
      ..
    } => {
      let ty = TypeOrWidth::Type(typedata);

      let &[pt_resolved] =
        resolve_location_src_load(builder, meta, pt, ptr_loc, None, ptr_loc_of as _, 1).as_ref()
      else {
        unreachable!();
      };

      let &[expected] =
        resolve_location_src_load(builder, meta, ty, expected_loc, None, expected_of as _, 1)
          .as_ref()
      else {
        unreachable!();
      };

      let &[tostore] = resolve_location_src_load(
        builder,
        meta,
        ty,
        val_stored_loc,
        None,
        val_store_of as _,
        1,
      )
      .as_ref() else {
        unreachable!();
      };

      let mut out = resolve_location_src_store(builder, meta, ty, ret_loc, None, ret_of as _, 2);

      let valload = builder
        .ins()
        .atomic_cas(MemFlags::trusted(), pt_resolved, expected, tostore);

      let succ = builder.ins().icmp(IntCC::Equal, valload, expected);

      // Check if vectored
      if out.total() == 1 {
        let data = ConstantData::from(&[] as &[u8]);
        let data = data.expand_to(ty.clif_mapping().width() as usize * 2);

        let cnst = meta.constpool.insert(data);

        let val = builder
          .ins()
          .vconst(ty.clif_mapping().x1.by(2).unwrap(), cnst);

        let val = builder.ins().insertlane(val, valload, 0);
        let val = builder.ins().insertlane(val, succ, 1);

        out.store(builder, 0, val);
      } else {
        out.store(builder, 0, valload);
        out.store(builder, 1, succ);
      }

      out.synchronize(builder, meta);
    }

    ATOMIC::RMW {
      typedata,

      ptr_loc,
      ptr_loc_of,

      load_loc,
      load_loc_of,

      rhs_loc,
      rhs_loc_of,

      op,
      ..
    } => {
      let ty = TypeOrWidth::Type(typedata);

      let &[pt_resolved] =
        resolve_location_src_load(builder, meta, pt, ptr_loc, None, ptr_loc_of as _, 1).as_ref()
      else {
        unreachable!();
      };

      let &[rhs] =
        resolve_location_src_load(builder, meta, ty, rhs_loc, None, rhs_loc_of as _, 1).as_ref()
      else {
        unreachable!();
      };

      let mut target =
        resolve_location_src_store(builder, meta, ty, load_loc, None, load_loc_of as _, 1);

      let output = builder.ins().atomic_rmw(
        ty.clif_mapping().x1,
        MemFlags::trusted(),
        match op {
          ATOMICRmwOp::Add => AtomicRmwOp::Add,
          ATOMICRmwOp::And => AtomicRmwOp::And,
          ATOMICRmwOp::Sub => AtomicRmwOp::Sub,
          ATOMICRmwOp::Or => AtomicRmwOp::Or,
          ATOMICRmwOp::Xor => AtomicRmwOp::Xor,
          ATOMICRmwOp::Xchg => AtomicRmwOp::Xchg,
          ATOMICRmwOp::Nand => AtomicRmwOp::Nand,
          ATOMICRmwOp::Max => {
            if ty.clif_mapping().signed {
              AtomicRmwOp::Smax
            } else {
              AtomicRmwOp::Umax
            }
          }
          ATOMICRmwOp::Min => {
            if ty.clif_mapping().signed {
              AtomicRmwOp::Smin
            } else {
              AtomicRmwOp::Umin
            }
          }
        },
        pt_resolved,
        rhs,
      );

      target.store(builder, 0, output);
      target.synchronize(builder, meta);
    }
  }
}
