#![allow(unused)]
use core::slice;
use std::{
  borrow::Cow,
  ffi::c_char,
  hint::black_box,
  marker::PhantomData,
  mem::zeroed,
  ops::Deref,
  ptr::null_mut,
  sync::{Arc, LazyLock},
};

use ahash::HashMap;
use llvm_sys::{
  analysis::{LLVMVerifierFailureAction, LLVMVerifyModule},
  core::{
    LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMArrayType2, LLVMBuildAlloca,
    LLVMContextCreate, LLVMCreateBuilderInContext, LLVMDisposeMessage, LLVMFunctionType,
    LLVMGetBufferSize, LLVMGetBufferStart, LLVMGetParam, LLVMInt8TypeInContext,
    LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMModuleCreateWithNameInContext,
    LLVMPointerTypeInContext, LLVMPositionBuilderAtEnd, LLVMPrintModuleToString, LLVMSetAlignment,
    LLVMSetDataLayout, LLVMSetTarget, LLVMVoidTypeInContext,
  },
  error::{LLVMConsumeError, LLVMDisposeErrorMessage},
  prelude::{
    LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMMemoryBufferRef, LLVMModuleRef,
    LLVMTypeRef, LLVMValueRef,
  },
  target::{LLVMCopyStringRepOfTargetData, LLVMIntPtrTypeInContext},
  target_machine::{
    LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetDataLayout,
    LLVMCreateTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetHostCPUFeatures,
    LLVMGetHostCPUName, LLVMGetTargetFromTriple, LLVMRelocMode,
    LLVMTargetMachineEmitToMemoryBuffer,
  },
  transforms::pass_builder::{
    LLVMCreatePassBuilderOptions, LLVMPassBuilderOptionsRef, LLVMRunPasses,
  },
};

use crate::{
  CacheData, CacheLevel, PickleJumpData, ThreadSafe,
  acaot::{
    JITReloc,
    native::{
      NativeCompiler,
      llvm_compiler::{
        dispose::{
          IRBuilder, LLVMBuffer, LLVMCtx, LLVMMsg, Module, OpaqueMachine, OpaqueTargetData,
          PassBuilderOptions,
        },
        irgen::compile,
        ssaupdater::{ReducedCompilerMeta, VMRegManager},
      },
    },
    pickle::def::PickleInstruction,
  },
  kvwrap::SaVMJumpWrapRef,
};

pub mod dispose;
pub mod irgen;
pub mod ssaupdater;

static JITRELOC_NONE: LazyLock<Arc<[JITReloc]>> = LazyLock::new(|| Arc::from([]));

static LLVMINIT: LazyLock<()> = LazyLock::new(|| unsafe {
  #[cfg(any(target_arch = "x86_64", target_arch = "x86"))]
  {
    use llvm_sys::target::{
      LLVMInitializeX86AsmParser, LLVMInitializeX86AsmPrinter, LLVMInitializeX86Target,
      LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC,
    };

    LLVMInitializeX86Target();
    LLVMInitializeX86AsmParser();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86TargetInfo();
  }

  #[cfg(target_arch = "riscv64")]
  {
    use llvm_sys::target::{
      LLVMInitializeRISCVAsmParser, LLVMInitializeRISCVAsmPrinter, LLVMInitializeRISCVTarget,
      LLVMInitializeRISCVTargetInfo, LLVMInitializeRISCVTargetMC,
    };

    LLVMInitializeRISCVTarget();
    LLVMInitializeRISCVAsmParser();
    LLVMInitializeRISCVAsmPrinter();
    LLVMInitializeRISCVTargetMC();
    LLVMInitializeRISCVTargetInfo();
  }

  #[cfg(target_arch = "powerpc64")]
  {
    use llvm_sys::target::{
      LLVMInitializePowerPCAsmParser, LLVMInitializePowerPCAsmPrinter, LLVMInitializePowerPCTarget,
      LLVMInitializePowerPCTargetInfo, LLVMInitializePowerPCTargetMC,
    };

    LLVMInitializePowerPCTarget();
    LLVMInitializePowerPCAsmParser();
    LLVMInitializePowerPCAsmPrinter();
    LLVMInitializePowerPCTargetMC();
    LLVMInitializePowerPCTargetInfo();
  }

  #[cfg(target_arch = "arm")]
  {
    use llvm_sys::target::{
      LLVMInitializeARMAsmParser, LLVMInitializeARMAsmPrinter, LLVMInitializeARMTarget,
      LLVMInitializeARMTargetInfo, LLVMInitializeARMTargetMC,
    };

    LLVMInitializeARMTarget();
    LLVMInitializeARMAsmParser();
    LLVMInitializeARMAsmPrinter();
    LLVMInitializeARMTargetMC();
    LLVMInitializeARMTargetInfo();
  }

  #[cfg(any(target_arch = "mips", target_arch = "mips64"))]
  {
    use llvm_sys::target::{
      LLVMInitializeMipsAsmParser, LLVMInitializeMipsAsmPrinter, LLVMInitializeMipsTarget,
      LLVMInitializeMipsTargetInfo, LLVMInitializeMipsTargetMC,
    };

    LLVMInitializeMipsTarget();
    LLVMInitializeMipsAsmParser();
    LLVMInitializeMipsAsmPrinter();
    LLVMInitializeMipsTargetMC();
    LLVMInitializeMipsTargetInfo();
  }

  #[cfg(target_arch = "aarch64")]
  {
    use llvm_sys::target::{
      LLVMInitializeAArch64AsmParser, LLVMInitializeAArch64AsmPrinter, LLVMInitializeAArch64Target,
      LLVMInitializeAArch64TargetInfo, LLVMInitializeAArch64TargetMC,
    };

    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64AsmParser();
    LLVMInitializeAArch64AsmPrinter();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64TargetInfo();
  }
});

static LLVM_CPU: LazyLock<ThreadSafe<LLVMMsg>> =
  LazyLock::new(|| unsafe { ThreadSafe(LLVMMsg(LLVMGetHostCPUName())) });

static LLVM_CPU_FEAT: LazyLock<ThreadSafe<LLVMMsg>> =
  LazyLock::new(|| unsafe { ThreadSafe(LLVMMsg(LLVMGetHostCPUFeatures())) });

thread_local! {
  static LLVM_CTX: LLVMCtx = unsafe {
    LLVMCtx(LLVMContextCreate())
  };

  static PASS_BUILDER_OPT: PassBuilderOptions = unsafe { PassBuilderOptions(LLVMCreatePassBuilderOptions()) };
}

pub struct SaVMLLVM {
  machine: OpaqueMachine,
  module: Module,
  layout: OpaqueTargetData,
  ctx: LLVMContextRef,
  cache: CacheLevel,
  passes: *const c_char,
  _dep: PhantomData<LLVMCtx>,
}

pub struct SaVMLLVMBuilder {}

impl SaVMLLVMBuilder {
  fn create(
    level: LLVMCodeGenOptLevel,
    reloc: LLVMRelocMode,
    codemodel: LLVMCodeModel,
    cache: CacheLevel,
    passes: *const c_char,
  ) -> Result<SaVMLLVM, Cow<'static, str>> {
    unsafe {
      black_box({
        LLVMINIT.deref();
      });

      let mut error = LLVMMsg(null_mut());

      let ctx = LLVM_CTX.with(|x| x.0);
      let module = Module(LLVMModuleCreateWithNameInContext(c"SaVMJIT".as_ptr(), ctx));

      let triple = LLVMMsg(LLVMGetDefaultTargetTriple());

      let mut target = zeroed();

      if LLVMGetTargetFromTriple(triple.0, &mut target, error.as_mut_ref()) != 0 {
        let out = Err(Cow::Owned(error.to_string_lossy().into_owned()));
        return out;
      }

      let machine = OpaqueMachine(LLVMCreateTargetMachine(
        target,
        triple.0,
        LLVM_CPU.0.0,
        LLVM_CPU_FEAT.0.0,
        level,
        reloc,
        codemodel,
      ));

      let layout = OpaqueTargetData(LLVMCreateTargetDataLayout(machine.0));
      let layout_str = LLVMMsg(LLVMCopyStringRepOfTargetData(layout.0));

      LLVMSetDataLayout(module.0, layout_str.0);
      LLVMSetTarget(module.0, triple.0);

      Ok(SaVMLLVM {
        machine,
        module,
        layout,
        ctx,
        cache,
        passes,
        _dep: PhantomData,
      })
    }
  }

  pub fn create_cinder<const T: bool>() -> Box<dyn NativeCompiler<T>> {
    Box::new(
      Self::create(
        LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
        LLVMRelocMode::LLVMRelocStatic,
        LLVMCodeModel::LLVMCodeModelLarge,
        CacheLevel::LLVMCinder,
        c"default<O1>".as_ptr(),
      )
      .expect("Unable to initialize LLVM"),
    )
  }

  pub fn create_crater<const T: bool>() -> Box<dyn NativeCompiler<T>> {
    Box::new(
      Self::create(
        LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
        LLVMRelocMode::LLVMRelocStatic,
        LLVMCodeModel::LLVMCodeModelLarge,
        CacheLevel::LLVMCrater,
        c"default<O2>".as_ptr(),
      )
      .expect("Unable to initialize LLVM"),
    )
  }

  pub fn create_epitome<const T: bool>() -> Box<dyn NativeCompiler<T>> {
    Box::new(
      Self::create(
        LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
        LLVMRelocMode::LLVMRelocPIC,
        LLVMCodeModel::LLVMCodeModelMedium,
        CacheLevel::LLVMEpitome,
        c"default<O3>".as_ptr(),
      )
      .expect("Unable to initialize LLVM"),
    )
  }
}

pub static LLVM_VAR_NAME: ThreadSafe<*const c_char> = ThreadSafe(c"".as_ptr());
pub static LLVM_FNN_NAME: ThreadSafe<*const c_char> = ThreadSafe(c"compiledlib".as_ptr());

impl<const T: bool> NativeCompiler<T> for SaVMLLVM {
  fn compile(
    &mut self,
    pickle: &[super::pickle::def::PickleInstruction],
    jmps: SaVMJumpWrapRef,
  ) -> crate::CacheData {
    unsafe {
      let ctx = self.ctx;
      let td = self.layout.0;
      let module = self.module.0;
      let fnname = LLVM_FNN_NAME.0;
      let globalname = LLVM_VAR_NAME.0;

      let mut params = [LLVMPointerTypeInContext(ctx, 0)];
      let func_ty = LLVMFunctionType(LLVMVoidTypeInContext(ctx), params.as_mut_ptr(), 1, 0);

      let function_val = LLVMAddFunction(module, fnname, func_ty);

      {
        let prologue = LLVMAppendBasicBlockInContext(ctx, function_val, c"prologue".as_ptr());

        let mut itr = jmps
          .0
          .into_iter()
          .map(|PickleJumpData { marker, .. }| {
            let blk = format!("blockid_marker_{}\0", *marker);
            let blk = LLVMAppendBasicBlockInContext(ctx, function_val, blk.as_ptr() as _);

            (*marker, blk)
          })
          .peekable();

        let mut blockmap = HashMap::default();
        // Register Each Block
        while let Some((marker, block)) = itr.next() {
          let next = itr.peek().map(|(_, block)| *block);

          blockmap.insert(
            marker,
            IBlock {
              current: block,
              next,
            },
          );
        }

        let vmctx = LLVMGetParam(function_val, 0);

        let builder_raii = IRBuilder(LLVMCreateBuilderInContext(ctx));
        let builder = builder_raii.0;

        LLVMPositionBuilderAtEnd(builder, prologue);

        let i64x24 = { LLVMArrayType2(LLVMInt64TypeInContext(ctx), 24) };
        let i64x16 = { LLVMArrayType2(LLVMInt64TypeInContext(ctx), 16) };

        // Compile IR Info
        let mut compilermeta = CompilerMeta {
          pickle,
          builder,
          vmctx,
          llvmctx: ctx,
          llvmmodule: module,
          llvmfn: function_val,
          rel: matches!(self.cache, CacheLevel::LLVMCinder | CacheLevel::LLVMCrater),
          ws: [0; 20],
          prologue,
          trap: LLVMAppendBasicBlockInContext(ctx, function_val, c"trap".as_ptr()),
          async_epilogue: LLVMAppendBasicBlockInContext(
            ctx,
            function_val,
            c"async_epilogue".as_ptr(),
          ),
          blockv0: LLVMAppendBasicBlockInContext(ctx, function_val, c"blockv0".as_ptr()),
          epilogue: LLVMAppendBasicBlockInContext(ctx, function_val, c"epilogue".as_ptr()),
          jumpresolver: LLVMAppendBasicBlockInContext(ctx, function_val, c"jumpresolver".as_ptr()),
          blockmap,

          regspill: LLVMBuildAlloca(builder, i64x16, c"regspill".as_ptr()),
          scratchpad: LLVMBuildAlloca(builder, i64x24, c"scratchpad".as_ptr()),

          scratchpad_ptr: null_mut(),

          regmnt: VMRegManager::new(ReducedCompilerMeta {
            builder,
            prologue,
            ctx,
            vmctx,
            fnval: function_val,
            i64: LLVMInt64TypeInContext(ctx),
            ptr: LLVMPointerTypeInContext(ctx, 0),
          }),

          i32: LLVMInt32TypeInContext(ctx),
          i64: LLVMInt64TypeInContext(ctx),
          i8: LLVMInt8TypeInContext(ctx),
          iptr: LLVMIntPtrTypeInContext(ctx, td),
          ptr: LLVMPointerTypeInContext(ctx, 0),
        };
        LLVMSetAlignment(compilermeta.regspill, 64);
        LLVMSetAlignment(compilermeta.scratchpad, 64);

        compile::<T>(&mut compilermeta);

        drop(builder_raii);
      }

      // Verify pipeline
      {
        let mut err: *mut c_char = null_mut();
        let status = LLVMVerifyModule(
          self.module.0,
          LLVMVerifierFailureAction::LLVMReturnStatusAction,
          &mut err,
        );

        if !err.is_null() {
          let err = LLVMMsg(err);

          if status != 0 {
            println!(
              "-------------------------------------\nSaVM ERR:\n{}",
              err.to_string_lossy()
            );
            return CacheData::None;
          }
        }
      }

      // Passes
      {
        let e = LLVMRunPasses(
          module,
          self.passes,
          self.machine.0,
          PASS_BUILDER_OPT.with(|x| x.0),
        );

        if !e.is_null() {
          println!("UNABLE TO RUN PASSES");
          LLVMConsumeError(e);
        }
      }

      // Print Module
      #[cfg(debug_assertions)]
      {
        let module = LLVMMsg({ LLVMPrintModuleToString(module) });

        println!("{}", module.to_str().unwrap());
      }

      // Compile Pipeline
      let mut buf: LLVMMemoryBufferRef = null_mut();
      {
        let mut err: *mut c_char = null_mut();
        let errbool = LLVMTargetMachineEmitToMemoryBuffer(
          self.machine.0,
          self.module.0,
          LLVMCodeGenFileType::LLVMObjectFile,
          &mut err,
          &mut buf,
        );

        if !err.is_null() {
          let err = LLVMMsg(err);

          if errbool != 0 {
            println!("SaVM ERR: {}", err.to_string_lossy());
            return CacheData::None;
          }
        }
      }

      if buf.is_null() {
        return CacheData::None;
      }

      let buf = LLVMBuffer(buf);

      return CacheData::JITCache {
        level: self.cache,
        binary: Arc::from(buf.deref()),
        reloc: JITRELOC_NONE.clone(),
      };

      CacheData::None
    }
  }
}

pub struct CompilerMeta<'a> {
  pub pickle: &'a [PickleInstruction],

  pub rel: bool,
  pub ws: [u8; 20],

  // LLVM Builder Ref
  pub builder: LLVMBuilderRef,
  pub llvmctx: LLVMContextRef,
  pub llvmmodule: LLVMModuleRef,
  pub llvmfn: LLVMValueRef,

  // Main Blocks
  pub prologue: LLVMBasicBlockRef,
  pub trap: LLVMBasicBlockRef,
  pub async_epilogue: LLVMBasicBlockRef,
  pub epilogue: LLVMBasicBlockRef,
  pub blockv0: LLVMBasicBlockRef,
  pub jumpresolver: LLVMBasicBlockRef,

  // Blockmap (also, resolved jumps)
  pub blockmap: HashMap<u64, IBlock>,

  // scratchpad
  pub scratchpad: LLVMValueRef,
  pub regspill: LLVMValueRef,

  // VM Registers
  pub regmnt: VMRegManager,

  // VM Context
  pub vmctx: LLVMValueRef,
  pub scratchpad_ptr: LLVMValueRef,

  // Few Types
  pub iptr: LLVMTypeRef,
  pub ptr: LLVMTypeRef,
  pub i64: LLVMTypeRef,
  pub i32: LLVMTypeRef,
  pub i8: LLVMTypeRef,
}

#[derive(Debug, Clone, Copy)]
pub struct IBlock {
  pub current: LLVMBasicBlockRef,
  pub next: Option<LLVMBasicBlockRef>,
}
