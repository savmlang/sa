use std::sync::{Arc, OnceLock};

use crate::acaot::{native::NativeCompiler, pickle::def::PickleInstruction};
use ahash::HashMap;
use cranelift::{
  native::builder,
  prelude::{
    isa::{Builder, TargetIsa},
    settings::Flags,
    *,
  },
};

static GLOBAL_ISA: OnceLock<Arc<dyn TargetIsa>> = OnceLock::new();

pub struct SaVMCranelift {
  pub abs8: bool,
  pub isa: Arc<dyn TargetIsa>,
  pub pickle: Option<Arc<[PickleInstruction]>>,
  pub jmps: Option<Arc<HashMap<u64, usize>>>,
}

impl SaVMCranelift {
  fn get_cached_isa() -> Arc<dyn TargetIsa> {
    GLOBAL_ISA
      .get_or_init(|| {
        let settings = settings::builder();
        builder()
          .expect("SaVM: Unsupported Host")
          .finish(Flags::new(settings))
          .expect("SaVM: Failed to finish ISA")
      })
      .clone()
  }

  fn new() -> Self {
    Self {
      abs8: true,
      isa: Self::get_cached_isa(),
      jmps: None,
      pickle: None,
    }
  }
}

impl NativeCompiler for SaVMCranelift {
  fn create_abs8() -> Box<dyn NativeCompiler>
  where
    Self: Sized,
  {
    #[cfg(any(
      target_arch = "x86_64",
      target_arch = "aarch64",
      target_arch = "riscv64"
    ))]
    return Box::new(Self::new());
  }

  fn create_rel() -> Option<Box<dyn NativeCompiler>>
  where
    Self: Sized,
  {
    #[cfg(any(
      target_arch = "x86_64",
      target_arch = "aarch64",
      target_arch = "riscv64"
    ))]
    return Some({
      let mut o = Self::new();
      o.abs8 = false;

      Box::new(o)
    });

    #[allow(unreachable_code)]
    return None;
  }

  fn prime(
    &mut self,
    pickle: Arc<[PickleInstruction]>,
    jmps: Arc<std::collections::HashMap<u64, usize, ahash::RandomState>>,
  ) {
    self.pickle = Some(pickle);
    self.jmps = Some(jmps);
  }
}
