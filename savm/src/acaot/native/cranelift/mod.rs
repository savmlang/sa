use crate::acaot::{
  native::{NativeCompiler, cranelift::irgen::compile},
  pickle::def::PickleInstruction,
};
use ahash::{HashMap, HashMapExt};
use cranelift::{
  codegen::{
    Context,
    cursor::Cursor,
    ir::{ArgumentPurpose, Function, StackSlot},
  },
  native::builder,
  prelude::{
    isa::{Builder, CallConv, TargetIsa},
    settings::Flags,
    types::I64,
    *,
  },
};
use sart::ctr::VMTaskState;
use std::{
  mem::offset_of,
  process::abort,
  sync::{Arc, OnceLock},
};

pub mod irgen;

static GLOBAL_ISA: OnceLock<Arc<dyn TargetIsa>> = OnceLock::new();
static GLOBAL_ISA_OPTIMIZED: OnceLock<Arc<dyn TargetIsa>> = OnceLock::new();

pub struct SaVMCranelift {
  pub abs8: bool,
  pub isa: Arc<dyn TargetIsa>,
}

impl SaVMCranelift {
  fn get_cached_isa(highspeed: bool) -> Arc<dyn TargetIsa> {
    if highspeed {
      &GLOBAL_ISA_OPTIMIZED
    } else {
      &GLOBAL_ISA
    }
    .get_or_init(|| {
      let mut settings = settings::builder();
      if highspeed {
        settings.set("opt_level", "speed");
        settings.set("regalloc_algorithm", "backtracking");
      } else {
        settings.set("regalloc_algorithm", "single_pass");
      }

      let mut flags = Flags::new(settings);

      if highspeed {
        flags.enable_alias_analysis();
      }

      builder()
        .expect("SaVM: Unsupported Host")
        .finish(flags)
        .expect("SaVM: Failed to finish ISA")
    })
    .clone()
  }

  fn new(highspeed: bool) -> Self {
    Self {
      abs8: true,
      isa: Self::get_cached_isa(highspeed),
    }
  }

  pub fn create_abs8() -> Box<dyn NativeCompiler>
  where
    Self: Sized,
  {
    #[cfg(any(
      target_arch = "x86_64",
      target_arch = "aarch64",
      target_arch = "riscv64"
    ))]
    return Box::new(Self::new(false));
  }

  pub fn create_rel_optimized() -> Box<dyn NativeCompiler>
  where
    Self: Sized,
  {
    let mut o = Self::new(true);
    o.abs8 = false;

    Box::new(o)
  }
}

impl NativeCompiler for SaVMCranelift {
  fn compiler_id(&self) -> super::CompilerId {
    super::CompilerId::Cranelift
  }

  fn codegen_internal_trampoline(&mut self) -> Box<[u8]> {
    let mut mainsig = Signature::new(self.isa.default_call_conv());
    // Pointer to launch
    mainsig.params.push(AbiParam::new(self.isa.pointer_type()));
    // VMTaskState pointer
    mainsig.params.push(AbiParam::new(self.isa.pointer_type()));

    let mut f = Function::new();
    f.signature = mainsig;

    let mut ctx = FunctionBuilderContext::new();

    {
      let mut builder = FunctionBuilder::new(&mut f, &mut ctx);

      let sig = {
        let mut s = Signature::new(CallConv::Fast);

        s.params.push(AbiParam::new(self.isa.pointer_type()));

        s
      };

      let entry = builder.create_block();
      builder.append_block_params_for_function_params(entry);
      builder.switch_to_block(entry);

      let [callee, argv0] = *builder.block_params(entry) else {
        abort();
      };

      let sig_ref = builder.import_signature(sig);

      builder.ins().call_indirect(sig_ref, callee, &[argv0]);
      builder.ins().return_(&[]);

      builder.seal_all_blocks();
      builder.finalize();
    }

    let mut ctx = Context::for_function(f);

    let comp = ctx
      .compile(self.isa.as_ref(), &mut Default::default())
      .unwrap_or_else(|_| abort());

    comp.code_buffer().into()
  }

  fn compile(
    &mut self,
    pickle: &[PickleInstruction],
    jmps: &std::collections::HashMap<u64, usize, ahash::RandomState>,
  ) -> crate::CacheData {
    let isa = self.isa.as_ref();

    let mainsig = {
      let mut sig = Signature::new(if self.abs8 {
        isa.default_call_conv()
      }
      // Use cranelift fast for PCRel callconv
      //
      // NOTE
      // Trampoline required!
      else {
        CallConv::Fast
      });

      sig.params.push(AbiParam::special(
        isa.pointer_type(),
        ArgumentPurpose::VMContext,
      ));

      sig
    };

    let mut ctx = FunctionBuilderContext::new();
    let mut f = Function::new();
    f.signature = mainsig;

    let mut builder = FunctionBuilder::new(&mut f, &mut ctx);

    // Data structures
    let pickle = pickle;
    let jumps = jmps;

    let mut ws = {
      let mut h = HashMap::new();
      let prologue = builder.create_block();
      builder.append_block_params_for_function_params(prologue);

      let blockv0 = builder.create_block();

      let (vmtaskstate, largepad) = {
        builder.switch_to_block(prologue);

        let vm_ctx = builder.create_global_value(GlobalValueData::VMContext);
        let glob = builder.ins().global_value(isa.pointer_type(), vm_ctx);

        let largepad = builder.declare_var(isa.pointer_type());

        let largepad_imm = builder.ins().load(
          isa.pointer_type(),
          MemFlags::trusted(),
          glob,
          offset_of!(VMTaskState, largepad) as i32,
        );
        builder.def_var(largepad, largepad_imm);

        builder.ins().jump(blockv0, []);

        (glob, largepad)
      };

      let mut jmps = jumps.iter().collect::<Vec<_>>();
      jmps.sort_by(|(_, a), (_, b)| a.cmp(b));

      let mut itr = jmps
        .into_iter()
        .map(|(marker, _)| {
          let blk = builder.create_block();

          (*marker, blk)
        })
        .peekable();

      // Write `blocks` for each
      while let Some((marker, block)) = itr.next() {
        let next = itr.peek().map(|(_, block)| *block);

        h.insert(
          marker,
          IBlock {
            current: block,
            next,
          },
        );
      }

      let trap = builder.create_block();

      let async_epilogue = builder.create_block();
      let epilogue = builder.create_block();

      CompilerMeta {
        scratchpad: builder.create_sized_stack_slot(StackSlotData::new(
          StackSlotKind::ExplicitDynamicSlot,
          192,
          6,
        )),
        regspill: builder.create_sized_stack_slot(StackSlotData::new(
          StackSlotKind::ExplicitDynamicSlot,
          64,
          6,
        )),
        blockmap: h,
        largepad,
        prologue,
        blockv0,
        epilogue,
        async_epilogue,
        vmtaskstate,
        trap,
        ws: [0u8; 20],
        r1: None,
        r2: None,
        r3: None,
        r4: None,
        r5: None,
        r6: None,
        r7: None,
        r8: None,
      }
    };

    compile(&mut builder, &mut ws, pickle.as_ref(), isa);

    // Compile
    builder.finalize();
    println!("Built this : {f:?}");
    {
      let mut ctx = Context::for_function(f);

      let compiled = ctx.compile(isa, &mut Default::default());

      let compiled = (|| {
        #[cfg(debug_assertions)]
        return compiled.unwrap();

        #[cfg(not(debug_assertions))]
        return compiled.unwrap_or_else(|_| abort());
      })();
    }

    // todo!()
    crate::CacheData::None
  }
}

#[derive(Debug, Clone)]
pub struct CompilerMeta {
  pub ws: [u8; 20],

  // Main Blocks
  pub prologue: Block,
  pub trap: Block,
  pub async_epilogue: Block,
  pub epilogue: Block,
  pub blockv0: Block,

  // PTR
  pub vmtaskstate: Value,

  // scratchpad
  pub scratchpad: StackSlot,
  pub regspill: StackSlot,

  // Largepad
  pub largepad: Variable,

  // R variables
  pub r1: Option<Variable>,
  pub r2: Option<Variable>,
  pub r3: Option<Variable>,
  pub r4: Option<Variable>,
  pub r5: Option<Variable>,
  pub r6: Option<Variable>,
  pub r7: Option<Variable>,
  pub r8: Option<Variable>,

  // Blockmaps
  pub blockmap: HashMap<u64, IBlock>,
}

#[derive(Debug, Clone, Copy)]
pub struct IBlock {
  pub current: Block,
  pub next: Option<Block>,
}
