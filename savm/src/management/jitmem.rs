#[cfg(feature = "llvm")]
use std::borrow::Cow;
use std::{
  mem::forget,
  num::NonZeroU8,
  pin::Pin,
  ptr::{null, null_mut},
};

use sajit::relcar::RELCAR_BASIC;
use sajit::relocations::RelocKind;
#[cfg(feature = "llvm")]
use sajit::symbpool::LLVMSymbolPool;
use sajit::{
  Executable, MemoryExecutableApi, WriteFnResult, advanced::MemoryExecutable,
  relocations::Relocation,
};

use crate::{
  acaot::{
    ClirLC, JITReloc, LocSrc,
    pickle::reader::corevm::{
      jitcall_scratch_ffi, jitcall_vcopy_noalias, jitcall_vcopy_overlapping,
    },
  },
  executor::corevm_libcall,
  management::polyfills::{llvm::memcpy, *},
};

pub struct JITMemoryManager {
  #[cfg(feature = "llvm")]
  symbpool: LLVMSymbolPool,
  quick: Vec<Pin<Box<MemoryExecutable>>>,

  // Stores `epicenter` TEXT - our flagship
  // JIT + AoT tier
  epitier: Option<MemoryExecutable>,
}

impl JITMemoryManager {
  pub fn new() -> Self {
    // Get a 32MB Chunk for TEMP Storage
    let a = MemoryExecutable::new_slab(Some(NonZeroU8::new(2).unwrap()));

    Self {
      #[cfg(feature = "llvm")]
      symbpool: LLVMSymbolPool::new(),
      quick: vec![Box::pin(a)],
      epitier: None,
    }
  }

  pub fn gc(&mut self) {
    self
      .quick
      .extract_if(.., |x| unsafe { x.try_free() }.is_ok())
      .for_each(|x| {
        // SAFETY : try_drop has ran all the drop glue code
        // We must dealloc the Box and continue
        let innr = *Pin::into_inner(x);
        forget(innr);
      });
  }

  // Reserve a 1KB space for SaVM CoreData
  const SAVM_COREDATA: usize = 1 * 1024;

  fn alloc_sized(
    quick: &mut Vec<Pin<Box<MemoryExecutable>>>,
    size: usize,
  ) -> &mut Pin<Box<MemoryExecutable>> {
    let alc = (size + Self::SAVM_COREDATA).next_multiple_of(MemoryExecutable::DEFAULT_SLAB_SIZE);
    let amt = alc / MemoryExecutable::DEFAULT_SLAB_SIZE;

    if amt > u8::MAX as usize {
      panic!("SaVM [CRITICAL] : This codebase is not possible to be correctly handled!");
    }

    let m = MemoryExecutable::new_slab(Some(NonZeroU8::new(amt as u8).unwrap()));

    quick.push(Box::pin(m));
    let handle = quick.last_mut().expect("Infallible");
    handle
  }

  // Max executable JIT blob is ~<32MB (for sanity)
  const MAX_EXEC_SIZE: usize = 31 * 1024 * 1024 + 1023 * 1024;

  pub fn write_quick(
    &mut self,
    data: &[u8],
    relocs: &[Relocation],
  ) -> (*const Executable, *mut usize) {
    // Try to fit larger ones
    // optimistically
    //
    // If it pays off separately - we'll think
    let mut out = (null(), null_mut());
    let succ = self
      .quick
      .iter_mut()
      .any(|x| match x.write_fn(data, relocs, &RELCAR_BASIC) {
        WriteFnResult::Executable(ex) => {
          out = (ex, x.stored.as_ptr());
          true
        }
        _ => false,
      });

    if succ {
      return out;
    }

    // This means - we need a specialized system
    // aka, a SINGLE slab to rule it all
    if data.len() > Self::MAX_EXEC_SIZE {
      let size = data.len();

      let m = Self::alloc_sized(&mut self.quick, size);

      let out = match m.write_fn(data, relocs, &RELCAR_BASIC) {
        WriteFnResult::Executable(ex) => (ex, m.stored.as_ptr()),
        _ => panic!("Reached a position where calculation is not idompotent"),
      };

      return out;
    }

    return self.write_quick(data, relocs);
  }

  #[cfg(feature = "llvm")]
  pub fn write_llvm<T>(
    &mut self,
    data: &[u8],
    mut resolver: T,
  ) -> Result<(*const Executable, *mut usize), Cow<'static, [Cow<'static, str>]>>
  where
    T: FnMut(*const str) -> usize,
  {
    use sajit::LLVMDryRun;

    let guaranteed =
      || MemoryExecutable::sizecalc(data).expect("Unable to at all calculate size needed!");

    let size_needed = if prefer_jitlink() {
      MemoryExecutable::sizecalc_jitlink(&self.symbpool, data)
        .unwrap_or_else(guaranteed)
        .get() as usize
    } else {
      guaranteed().get() as _
    };

    let mut jitwrite = |mexec: &mut MemoryExecutable, _symbpool: &LLVMSymbolPool| {
      #[allow(unused_mut)]
      let mut resolver_full = |d: *const str| match unsafe { &*d } {
        "fmaf" => (llvm::fmaf as *const ()).addr(),
        "fma" => (llvm::fma as *const ()).addr(),
        "memcpy" => (memcpy as *const ()).addr(),
        "memmove" => (llvm::memmove as *const ()).addr(),
        _ => resolver(d),
      };

      #[cfg(all(windows, target_arch = "x86"))]
      return (|| {
        use sajit::coffr::loader::I686COFFRelocator;
        use std::collections::HashMap;

        let mut out = HashMap::new();

        unsafe {
          I686COFFRelocator::load(&data, mexec).map_err(|_| {
            Cow::Borrowed(&[Cow::Borrowed("Unable to parse COFF")] as &'static [Cow<'static, str>])
          })?.prepare(|d| {
            resolver_full(d) as u32
          }, |name, ptr| {
            use std::mem::transmute;

            _ = out.insert(Box::from(name) as Box<str>, transmute::<_, *const Executable>(ptr as usize));
          });
        };

        Ok(out)
      })();

      #[cfg(not(all(windows, target_arch = "x86")))]
      return (|| {
        if prefer_jitlink() {
          use sajit::LLVMJITLink;

          mexec.write_jitlink(_symbpool, data, resolver_full)
        } else {
          use sajit::LLVMRTDyld;
          use std::borrow::Cow;

          mexec.write_rtdyld(data, resolver_full).map_err(|_| {
            Cow::Borrowed(
              &[Cow::Borrowed("RTDyld was unable to relocate!")] as &'static [Cow<'static, str>]
            )
          })
        }
      })();
    };

    let memexec = self
      .quick
      .iter_mut()
      .find(|x| x.under_size(size_needed).unwrap_or(false));

    let mexec = match memexec {
      Some(m) => m,
      None => Self::alloc_sized(&mut self.quick, size_needed),
    };

    let (out, mexec) = {
      use sajit::MemorySizeInfo;

      let old = mexec.cursor();
      let out: Result<
        std::collections::HashMap<Box<str>, *const Executable>,
        Cow<'_, [Cow<'static, str>]>,
      > = jitwrite(mexec, &self.symbpool);
      let new = mexec.cursor();

      assert!((new - old) <= size_needed);

      Ok::<_, Cow<'_, [Cow<'static, str>]>>((out?, mexec.stored.as_ptr()))
    }?;

    let paths = ["compiledlib", "@compiledlib", "_compiledlib"];

    paths
      .iter()
      .find_map(|&name| out.get(name).map(|x| (*x, mexec)))
      .ok_or_else(|| {
        Cow::Borrowed(
          &[Cow::Borrowed("Could now get @compiledlib symbol")] as &'static [Cow<'static, str>]
        )
      })
  }
}

#[rustfmt::skip]
fn prefer_jitlink() -> bool {
  cfg!(
    any(
      all(
        target_os = "linux", 
        any(
          target_arch = "x86_64",
          target_arch = "aarch64",
          target_arch = "riscv64",
          target_arch = "powerpc64"
        )
      ),
      all(
        target_os = "macos",
        target_arch = "aarch64"
      )
    )
  )
}

impl Drop for JITMemoryManager {
  fn drop(&mut self) {
    self
      .quick
      .drain(..)
      .for_each(|x| {
        // Leak since its not dropped
        // Safety
        let x= Pin::into_inner(x);
        x.leak();

        #[cfg(debug_assertions)]
        panic!("Please ensure that SaVM is programmed correctly, the Drop method should not encounter stray Memory blobs!");
      });

    if let Some(x) = self.epitier.take() {
      // This should be leaked
      x.leak();
    }
  }
}

pub fn calculate_relocation_abs(reloc: &[JITReloc]) -> Box<[Relocation]> {
  reloc
    .iter()
    .map(|reloc| {
      let mut relocdata = Relocation {
        addend: reloc.addend,
        kind: (|| {
          #[cfg(target_pointer_width = "64")]
          return RelocKind::Abs8;

          #[cfg(target_pointer_width = "32")]
          return RelocKind::Abs4;
        })(),
        offset: reloc.offset,
        symbol_addr: (corevm_libcall as *const ()).addr() as _,
      };

      relocdata.symbol_addr = match &reloc.loc {
        LocSrc::VCopyNoAlias => (jitcall_vcopy_noalias as *const ()).addr() as _,
        LocSrc::VCopyOverlapping => (jitcall_vcopy_overlapping as *const ()).addr() as _,
        LocSrc::VMScratchAction => (jitcall_scratch_ffi as *const ()).addr() as _,
        LocSrc::CLIRLibCall(cir) => match cir {
          ClirLC::Ceil32 => (ceil32 as *const ()).addr() as _,
          ClirLC::Ceil64 => (ceil64 as *const ()).addr() as _,
          ClirLC::Floor32 => (floor32 as *const ()).addr() as _,
          ClirLC::Floor64 => (floor64 as *const ()).addr() as _,
          ClirLC::Fma32 => (fma32 as *const ()).addr() as _,
          ClirLC::Fma64 => (fma64 as *const ()).addr() as _,
          ClirLC::Nearest32 => (nearest32 as *const ()).addr() as _,
          ClirLC::Nearest64 => (nearest64 as *const ()).addr() as _,
          ClirLC::Trunc32 => (trunc32 as *const ()).addr() as _,
          ClirLC::Trunc64 => (trunc64 as *const ()).addr() as _,
          ClirLC::Memcpy => (memcpy as *const ()).addr() as _,
        },
        _ => relocdata.symbol_addr,
      };

      relocdata
    })
    .collect::<Box<_>>()
}
