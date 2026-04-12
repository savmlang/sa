use cranelift::prelude::{FloatCC, FunctionBuilder, InstBuilder, IntCC, types::I64};

use crate::{
  acaot::{
    native::cranelift::{
      CompilerMeta,
      irgen::reg::{TypeOrWidth, resolve_location_src_load, resolve_location_src_store},
    },
    pickle::def::PickleInstruction,
  },
  readws,
};

pub fn hwnd_vcmp(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let op = pickle.u1;
  let width = pickle.u2;

  // OPS from 10 are Float Ops
  let float = op >= 10;

  let typ = TypeOrWidth::Width(width);

  let srcflags = readws!(meta, start = 0, stop = 2, u16);
  let count = readws!(meta, start = 2, stop = 6, u32);

  let src1 = {
    let offset = readws!(meta, start = 6, stop = 10, i32);
    let src = (srcflags >> 12) as u8 & 0xF;

    resolve_location_src_load(builder, meta, typ, src, None, offset, count)
  };

  let src2 = {
    let offset = readws!(meta, start = 10, stop = 14, i32);
    let src = (srcflags >> 8) as u8 & 0xF;

    resolve_location_src_load(builder, meta, typ, src, None, offset, count)
  };

  let mut target = {
    let offset = readws!(meta, start = 14, stop = 18, i32);
    let src = (srcflags >> 4) as u8 & 0xF;

    resolve_location_src_store(builder, meta, typ, src, None, offset, count)
  };

  src1
    .into_iter()
    .zip(src2.into_iter())
    .enumerate()
    .for_each(|(idx, (src1, src2))| {
      let val = if float {
        builder.ins().fcmp(
          match op {
            10 => FloatCC::Ordered,
            11 => FloatCC::Unordered,
            12 => FloatCC::Equal,
            13 => FloatCC::NotEqual,
            14 => FloatCC::OrderedNotEqual,
            15 => FloatCC::UnorderedOrEqual,
            16 => FloatCC::LessThan,
            17 => FloatCC::LessThanOrEqual,
            18 => FloatCC::GreaterThan,
            19 => FloatCC::GreaterThanOrEqual,
            20 => FloatCC::UnorderedOrLessThan,
            21 => FloatCC::UnorderedOrLessThanOrEqual,
            22 => FloatCC::UnorderedOrGreaterThan,
            23 => FloatCC::UnorderedOrGreaterThanOrEqual,
            _ => unreachable!(),
          },
          src1,
          src2,
        )
      } else {
        let srca = builder.func.dfg.value_type(src1);
        let srcb = builder.func.dfg.value_type(src2);

        builder.ins().icmp(
          match op {
            0 => IntCC::Equal,
            1 => IntCC::NotEqual,

            // LT
            2 => IntCC::SignedLessThan,
            3 => IntCC::UnsignedLessThan,
            4 => IntCC::SignedLessThanOrEqual,
            5 => IntCC::UnsignedLessThanOrEqual,

            // GT
            6 => IntCC::SignedGreaterThan,
            7 => IntCC::UnsignedGreaterThan,
            8 => IntCC::SignedGreaterThanOrEqual,
            9 => IntCC::UnsignedGreaterThanOrEqual,
            _ => unreachable!(),
          },
          src1,
          src2,
        )
      };

      let newtype = builder.func.dfg.value_type(src1);

      let val = builder.ins().sextend(newtype, val);

      target.store(builder, idx, val);
    });

  target.synchronize(builder, meta);
}
