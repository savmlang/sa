use std::{num::NonZeroU8, pin::Pin, ptr::null, sync::Arc};

use sajit::{
  Executable, MemoryExecutableApi, WriteFnResult, advanced::MemoryExecutable,
  relocations::Relocation,
};

use crate::acaot::JITReloc;

pub struct JITMemoryManager {
  quick: Vec<Pin<Box<MemoryExecutable>>>,

  // Stores `epicenter` TEXT - our flagship
  // JIT + AoT tier
  epicenter: Option<MemoryExecutable>,
}

impl JITMemoryManager {
  pub fn new() -> Self {
    // Get a 32MB Chunk for TEMP Storage
    let a = MemoryExecutable::new_slab(Some(NonZeroU8::new(2).unwrap()));

    Self {
      quick: vec![Box::pin(a)],
      epicenter: None,
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
      .any(|x| match x.write_fn(data, relocs) {
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

      let out = match m.write_fn(data, relocs) {
        WriteFnResult::Executable(ex) => ex,
        _ => panic!("Reached a position where calculation is not idompotent"),
      };

      self.quick.push(Box::pin(m));

      return out;
    }

    self.alloc_quick_new();
    return self.write_quick(data, relocs);
  }
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

    if let Some(x) = self.epicenter.take() {
      // This should be leaked
      x.leak();
    }
  }
}

pub fn calculate_relocation(rl: &[JITReloc]) -> Box<[Relocation]> {
  todo!()
}
