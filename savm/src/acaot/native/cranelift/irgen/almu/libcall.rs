use std::mem::offset_of;

use cranelift::prelude::{
  MemFlagsData as MemFlags,
  isa::CallConv,
  types::{F32, F64, I8, I16, I32, I64},
  *,
};
use sart::{
  ctr::VMTaskState,
  structures::{
    QuadPackedData,
    ffi::{COut, CallSig, MapValue, VReg, VType},
  },
};

use crate::{
  FNCALL_DISPATCH,
  acaot::{
    LocSrc, SigStore,
    native::cranelift::{
      CompilerMeta,
      irgen::reg::{
        TypeOrWidth, resolve_location_src_load, resolve_location_src_store, resolve_reg,
      },
    },
    pickle::def::PickleInstruction,
  },
  readws,
};

pub fn hwnd_libcall_sync(
  builder: &mut FunctionBuilder,
  meta: &mut CompilerMeta,
  pickle: PickleInstruction,
) {
  let sectionid = readws!(meta, start = 0, stop = 4, u64);

  // bits 0..7 == dont push regs 0..7 through
  let regignore = pickle.u1;
  let regloadmap = (0..8).map(|n| regignore & (1 << n) == 0);

  let rel = meta.rel;
  let vmtskst = meta.vmtaskstate;

  let builder_ptr = builder as *mut _;
  let stdprelude = |real: Option<Value>| {
    let builder = unsafe { &mut *builder_ptr };
    if let Some(newtskst) = real {
      regloadmap.enumerate().for_each(|(regid, load)| {
        if load {
          let reg = resolve_reg(builder, meta, regid as _);
          let regval = builder.use_var(reg);

          builder.ins().store(
            MemFlags::trusted(),
            regval,
            newtskst,
            regid as i32 * size_of::<QuadPackedData>() as i32,
          );
        }
      });
      return;
    }
    // Load all SSA registers into a stack
    regloadmap.enumerate().for_each(|(regid, load)| {
      if load {
        let reg = resolve_reg(builder, meta, regid as _);
        let regval = builder.use_var(reg);

        builder.ins().stack_store(
          regval,
          meta.regspill,
          regid as i32 * size_of::<QuadPackedData>() as i32,
        );
      }
    });

    // Hydrate only engine_or_pt and ws_or_pt2
    let engine_or_pt = builder.ins().load(
      I64,
      MemFlags::trusted(),
      meta.vmtaskstate,
      offset_of!(VMTaskState, engine_or_pt) as i32,
    );
    let ws_or_pt2 = builder.ins().load(
      I64,
      MemFlags::trusted(),
      meta.vmtaskstate,
      offset_of!(VMTaskState, ws_or_pt2) as i32,
    );

    builder.ins().stack_store(
      engine_or_pt,
      meta.regspill,
      offset_of!(VMTaskState, engine_or_pt) as i32,
    );

    builder.ins().stack_store(
      ws_or_pt2,
      meta.regspill,
      offset_of!(VMTaskState, ws_or_pt2) as i32,
    );
  };

  let dispatch = FNCALL_DISPATCH
    .get()
    .expect("SaVM Critical Error : This just cannot be empty. Guaranteed by call routine");

  let Some((_, cdef)) = dispatch.get(&sectionid) else {
    let tskst = if rel {
      let newtskst = builder
        .ins()
        .iadd_imm(vmtskst, size_of::<VMTaskState>() as i64);

      stdprelude(Some(newtskst));

      newtskst
    } else {
      stdprelude(None);
      let vmtskst = builder.ins().stack_addr(I64, meta.regspill, 0);

      vmtskst
    };

    // Call SectionDispatch
    {
      let sign = if !meta.rel {
        *meta
          .sigref
          .entry(SigStore::SaVMLibcallDispatch)
          .or_insert_with(|| {
            let safficall = savmlibcall(meta.callconv, meta.isa.pointer_type());

            let re = builder.import_signature(safficall);

            re
          })
      } else {
        *meta.sigref.entry(SigStore::JITCall).or_insert_with(|| {
          let callconv = CallConv::Fast;
          let mut safficall = Signature::new(callconv);

          {
            let ptr = meta.isa.pointer_type();
            safficall.params.push(AbiParam::new(ptr));
          }

          let re = builder.import_signature(safficall);

          re
        })
      };

      let ffifn = meta.get_fn(
        builder,
        if meta.rel {
          LocSrc::SaLibCall(sectionid)
        } else {
          LocSrc::VMSectionDispatch
        },
        sign,
      );

      if meta.rel {
        builder.ins().call(ffifn, &[tskst]);
      } else {
        let sectionidval = builder.ins().iconst(I64, sectionid as i64);

        builder.ins().call(ffifn, &[tskst, sectionidval]);
      }
    }

    [6u8, 7].into_iter().for_each(|idx| {
      // Unload from stack
      let regval = builder.ins().load(
        I64,
        MemFlags::trusted(),
        tskst,
        idx as i32 * size_of::<QuadPackedData>() as i32,
      );

      let regvar = resolve_reg(builder, meta, idx);
      builder.def_var(regvar, regval);
    });
    return;
  };

  match cdef {
    CallSig::CDef(cdef) => {
      // FallBack
      if cdef
        .inargs
        .iter()
        .any(|x| matches!(x.vtype, VType::Bytes(_)))
      {
        let sig = *meta
          .sigref
          .entry(SigStore::SaVMLibcallDispatch)
          .or_insert_with(|| {
            let safficall = savmlibcall(meta.callconv, meta.isa.pointer_type());

            let re = builder.import_signature(safficall);

            re
          });

        // Load all SSA registers into a stack
        (0..8).for_each(|regid| {
          let reg = resolve_reg(builder, meta, regid);
          let regval = builder.use_var(reg);

          builder.ins().stack_store(
            regval,
            meta.regspill,
            regid as i32 * size_of::<QuadPackedData>() as i32,
          );
        });

        let scratchpadptr = builder.ins().stack_addr(I64, meta.scratchpad, 0);
        builder.ins().stack_store(
          scratchpadptr,
          meta.regspill,
          offset_of!(VMTaskState, scratchpad) as i32,
        );

        let stack_tsk = builder.ins().stack_addr(I64, meta.regspill, 0);
        let sectionidval = builder.ins().iconst(I64, sectionid as i64);

        let funcref = meta.get_fn(builder, LocSrc::VMLibcallSection, sig);

        builder.ins().call(funcref, &[stack_tsk, sectionidval]);

        [6u8, 7].into_iter().for_each(|idx| {
          // Unload from stack
          let regval = builder.ins().stack_load(
            I64,
            meta.regspill,
            idx as i32 * size_of::<QuadPackedData>() as i32,
          );

          let regvar = resolve_reg(builder, meta, idx);
          builder.def_var(regvar, regval);
        });
        return;
      }

      let sign = *meta
        .sigref
        .entry(SigStore::LibDefined(sectionid))
        .or_insert_with(|| {
          let callconv = meta.callconv;
          let mut safficall = Signature::new(callconv);

          {
            safficall
              .params
              .extend(cdef.inargs.iter().map(|x| AbiParam::new(clifmapval(x))));
          }

          safficall.returns.extend(mapclif_out(&cdef.out));

          let re = builder.import_signature(safficall);

          re
        });

      let ffifn = meta.get_fn(builder, LocSrc::NativeLibCall(sectionid), sign);

      let args = cdef
        .inargs
        .iter()
        .map(|x| {
          let typ = TypeOrWidth::Type(x.vtype.as_savmtype());
          let locsrc = x.vreg.as_locsrc();

          let &[o] =
            resolve_location_src_load(builder, meta, typ, locsrc, None, x.regof as _, 1).as_ref()
          else {
            unreachable!();
          };

          o
        })
        .collect::<Box<_>>();

      let fcall = builder.ins().call(ffifn, &args);

      let stores = match cdef.out {
        COut::Void => None,
        COut::Bits8 => Some(resolve_location_src_store(
          builder,
          meta,
          TypeOrWidth::Width(3),
          6, // r7
          None,
          0,
          1,
        )),
        COut::Bits16 => Some(resolve_location_src_store(
          builder,
          meta,
          TypeOrWidth::Width(2),
          6, // r7
          None,
          0,
          1,
        )),
        COut::Bits32 => Some(resolve_location_src_store(
          builder,
          meta,
          TypeOrWidth::Width(1),
          6, // r7
          None,
          0,
          1,
        )),
        COut::Bits64 => Some(resolve_location_src_store(
          builder,
          meta,
          TypeOrWidth::Width(0),
          6, // r7
          None,
          0,
          1,
        )),
        COut::Bits128 => Some(resolve_location_src_store(
          builder,
          meta,
          TypeOrWidth::Width(0),
          6, // r7+r8
          None,
          0,
          2,
        )),
      };

      if let Some(mut stores) = stores {
        let bldr_ptr = builder as *mut _;
        builder
          .inst_results(fcall)
          .iter()
          .enumerate()
          .for_each(|(regof, &val)| {
            let builder = unsafe { &mut *bldr_ptr };
            stores.store(builder, regof, val);
          });

        stores.synchronize(builder, meta);
      }
    }
    CallSig::SaFFI(saffi) => {
      stdprelude(None);

      // FFI Call
      {
        let sign = *meta.sigref.entry(SigStore::SaFFICall).or_insert_with(|| {
          let callconv = meta.callconv;
          let mut safficall = Signature::new(callconv);

          {
            let ptr = meta.isa.pointer_type();
            safficall.params.push(AbiParam::new(ptr));
          }

          let re = builder.import_signature(safficall);

          re
        });

        let ffifn = meta.get_fn(builder, LocSrc::NativeLibCall(sectionid), sign);

        let vmtskst = builder.ins().stack_addr(I64, meta.regspill, 0);
        builder.ins().call(ffifn, &[vmtskst]);
      }

      // Spill Clobbered data back!
      {
        [
          VReg::R1,
          VReg::R2,
          VReg::R3,
          VReg::R4,
          VReg::R5,
          VReg::R6,
          VReg::R7,
          VReg::R8,
        ]
        .into_iter()
        .enumerate()
        .for_each(|(idx, reg)| {
          // Unload from stack
          if saffi.clobbers(reg).unwrap() {
            let regval = builder.ins().stack_load(
              I64,
              meta.regspill,
              idx as i32 * size_of::<QuadPackedData>() as i32,
            );

            let regvar = resolve_reg(builder, meta, idx as _);
            builder.def_var(regvar, regval);
          }
        });
      }
    }
    _ => unreachable!("Synccall cannot be used to dispatch async methods"),
  }
}

fn mapclif_out(cout: &COut) -> Box<[AbiParam]> {
  match cout {
    COut::Void => Box::new([]),
    COut::Bits8 => Box::new([AbiParam::new(I8)]),
    COut::Bits16 => Box::new([AbiParam::new(I16)]),
    COut::Bits32 => Box::new([AbiParam::new(I32)]),
    COut::Bits64 => Box::new([AbiParam::new(I64)]),
    COut::Bits128 => Box::new([AbiParam::new(I64); 2]),
  }
}

fn clifmapval(x: &MapValue) -> Type {
  match x.vtype {
    VType::I8 | VType::U8 => I8,
    VType::I16 | VType::U16 => I16,
    VType::I32 | VType::U32 => I32,
    VType::ISize | VType::USize => {
      if cfg!(target_pointer_width = "64") {
        I64
      } else {
        I32
      }
    }
    VType::I64 | VType::U64 => I64,
    VType::F32 => F32,
    VType::F64 => F64,
    VType::Bytes(_) => unreachable!("Bailout must happen!"),
  }
}

fn savmlibcall(callconv: CallConv, ptr: Type) -> Signature {
  let mut safficall = Signature::new(callconv);

  {
    safficall.params.push(AbiParam::new(ptr));
    safficall.params.push(AbiParam::new(I64));
  }

  safficall
}
