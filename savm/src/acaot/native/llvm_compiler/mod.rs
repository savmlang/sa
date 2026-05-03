use std::{
  borrow::Cow, ffi::CStr, hint::black_box, mem::zeroed, ops::Deref, ptr::null_mut, sync::LazyLock,
};

use llvm_sys::{
  LLVMContext, LLVMModule,
  core::{LLVMContextCreate, LLVMModuleCreateWithNameInContext, LLVMSetDataLayout, LLVMSetTarget},
  target::{
    LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter,
    LLVM_InitializeNativeDisassembler, LLVM_InitializeNativeTarget, LLVMCopyStringRepOfTargetData,
  },
  target_machine::{
    LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine,
    LLVMGetDefaultTargetTriple, LLVMGetHostCPUFeatures, LLVMGetHostCPUName,
    LLVMGetTargetFromTriple, LLVMOpaqueTargetMachine, LLVMRelocMode,
  },
};

use crate::acaot::native::{
  NativeCompiler,
  llvm_compiler::dispose::{LLVMCtx, LLVMMsg, Module, OpaqueMachine, OpaqueTargetData},
};

pub mod dispose;

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

pub struct SaVMLLVM {
  machine: OpaqueMachine,
  module: Module,
  ctx: LLVMCtx,
}

unsafe impl Send for SaVMLLVM {}

impl SaVMLLVM {
  fn create(
    level: LLVMCodeGenOptLevel,
    reloc: LLVMRelocMode,
    codemodel: LLVMCodeModel,
  ) -> Result<Self, Cow<'static, str>> {
    unsafe {
      black_box({
        LLVMINIT.deref();
      });

      let mut error = LLVMMsg(null_mut());

      let ctx = LLVMCtx(LLVMContextCreate());
      let module = Module(LLVMModuleCreateWithNameInContext(
        c"SaVMJIT".as_ptr(),
        ctx.0,
      ));

      let triple = LLVMMsg(LLVMGetDefaultTargetTriple());

      let mut target = zeroed();

      if LLVMGetTargetFromTriple(triple.0, &mut target, error.as_mut_ref()) != 0 {
        let out = Err(Cow::Owned(
          CStr::from_ptr(error.0 as _).to_string_lossy().into_owned(),
        ));
        return out;
      }

      let cpu = LLVMMsg(LLVMGetHostCPUName());
      let features = LLVMMsg(LLVMGetHostCPUFeatures());

      if cpu.0.is_null() || features.0.is_null() {
        return Err(Cow::Borrowed("CPU and Features could not be detected"));
      }

      let machine = OpaqueMachine(LLVMCreateTargetMachine(
        target, triple.0, cpu.0, features.0, level, reloc, codemodel,
      ));

      let layout = OpaqueTargetData(LLVMCreateTargetDataLayout(machine.0));
      let layout_str = LLVMMsg(LLVMCopyStringRepOfTargetData(layout.0));

      LLVMSetDataLayout(module.0, layout_str.0);
      LLVMSetTarget(module.0, triple.0);

      Ok(Self {
        ctx,
        machine,
        module,
      })
    }
  }

  pub fn create_cinder() -> Box<dyn NativeCompiler> {
    Box::new(
      Self::create(
        LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
        LLVMRelocMode::LLVMRelocStatic,
        LLVMCodeModel::LLVMCodeModelLarge,
      )
      .expect("Unable to initialize LLVM"),
    )
  }

  pub fn create_crater() -> Box<dyn NativeCompiler> {
    Box::new(
      Self::create(
        LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
        LLVMRelocMode::LLVMRelocStatic,
        LLVMCodeModel::LLVMCodeModelLarge,
      )
      .expect("Unable to initialize LLVM"),
    )
  }

  pub fn create_epitome() -> Box<dyn NativeCompiler> {
    Box::new(
      Self::create(
        LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
        LLVMRelocMode::LLVMRelocPIC,
        LLVMCodeModel::LLVMCodeModelDefault,
      )
      .expect("Unable to initialize LLVM"),
    )
  }
}

impl NativeCompiler for SaVMLLVM {
  fn compile(
    &mut self,
    pickle: &[super::pickle::def::PickleInstruction],
    jumps: &std::collections::HashMap<u64, usize, ahash::RandomState>,
  ) -> crate::CacheData {
    todo!()
  }
}
