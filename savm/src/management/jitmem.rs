#[cfg(feature = "llvm")]
use std::borrow::Cow;
use std::{num::NonZeroU8, pin::Pin, ptr::null};

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
    JITReloc, LocSrc,
    pickle::reader::corevm::{
      jitcall_scratch_ffi, jitcall_vcopy_noalias, jitcall_vcopy_overlapping,
    },
  },
  executor::corevm_libcall,
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

  pub fn alloc_quick_new(&mut self) {
    self
      .quick
      // Since its more slabs - use None i.e. default quanta = 16MiB
      .push(Box::pin(MemoryExecutable::new_slab(None)));
  }

  // Max executable JIT blob is ~<32MB (for sanity)
  const MAX_EXEC_SIZE: usize = 31 * 1024 * 1024 + 1023 * 1024;

  pub fn write_quick(&mut self, data: &[u8], relocs: &[Relocation]) -> *const Executable {
    // Try to fit larger ones
    // optimistically
    //
    // If it pays off separately - we'll think
    let mut out = null();
    let succ = self
      .quick
      .iter_mut()
      .any(|x| match x.write_fn(data, relocs, &RELCAR_BASIC) {
        WriteFnResult::Executable(ex) => {
          out = ex;
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

      let alc = size.next_multiple_of(MemoryExecutable::DEFAULT_SLAB_SIZE);
      let amt = alc / MemoryExecutable::DEFAULT_SLAB_SIZE;

      if amt > u8::MAX as usize {
        panic!("SaVM [CRITICAL] : This codebase is not possible to be correctly handled!");
      }

      let mut m = MemoryExecutable::new_slab(Some(NonZeroU8::new(amt as u8).unwrap()));

      let out = match m.write_fn(data, relocs, &RELCAR_BASIC) {
        WriteFnResult::Executable(ex) => ex,
        _ => panic!("Reached a position where calculation is not idompotent"),
      };

      self.quick.push(Box::pin(m));

      return out;
    }

    self.alloc_quick_new();
    return self.write_quick(data, relocs);
  }

  #[cfg(feature = "llvm")]
  pub fn write_llvm<T>(
    &mut self,
    data: &[u8],
    mut resolver: T,
  ) -> Result<*const Executable, Cow<'static, [Cow<'static, str>]>>
  where
    T: FnMut(*const str) -> usize,
  {
    use sajit::LLVMDryRun;

    let size_needed = MemoryExecutable::sizecalc(data).unwrap().get() as usize;

    println!("Needed : {size_needed}");

    let mut jitwrite = |mexec: &mut MemoryExecutable, symbpool: &LLVMSymbolPool| {
      if prefer_jitlink() {
        use sajit::LLVMJITLink;

        mexec.write_jitlink(symbpool, data, |d| {
          println!("Need to resolve : {}", unsafe { &*d });
          resolver(d)
        })
      } else {
        use sajit::LLVMRTDyld;
        use std::borrow::Cow;

        mexec
          .write_rtdyld(data, |d| {
            println!("Need to resolve : {}", unsafe { &*d });
            resolver(d)
          })
          .map_err(|_| {
            Cow::Borrowed(
              &[Cow::Borrowed("RTDyld was unable to relocate!")] as &'static [Cow<'static, str>]
            )
          })
      }
    };

    let memexec = self
      .quick
      .iter_mut()
      .find(|x| x.under_size(size_needed).unwrap_or(false));

    let out = if let Some(mexec) = memexec {
      use sajit::MemorySizeInfo;

      let old = mexec.cursor();
      let out = jitwrite(mexec, &self.symbpool);
      let new = mexec.cursor();

      println!("Written really : {}", new - old);

      out
    } else {
      todo!();
    }?;

    println!("Calculated: {size_needed}");

    out.get("compiledlib").map(|x| *x).ok_or_else(|| {
      Cow::Borrowed(
        &[Cow::Borrowed("Could now get @compiledlib symbol")] as &'static [Cow<'static, str>]
      )
    })
  }
}

#[rustfmt::skip]
fn prefer_jitlink() -> bool {
  false
  // cfg!(
  //   any(
  //     all(
  //       target_os = "linux", 
  //       any(
  //         target_arch = "x86_64",
  //         target_arch = "aarch64",
  //         target_arch = "riscv64",
  //         target_arch = "powerpc64"
  //       )
  //     ),
  //     target_os = "macos"
  //   )
  // )
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
        _ => relocdata.symbol_addr,
      };

      relocdata
    })
    .collect::<Box<_>>()
}
