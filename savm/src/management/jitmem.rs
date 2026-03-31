use std::{num::NonZeroU8, ptr::null, sync::Arc};

use sajit::{
  Executable, MemoryExecutableApi, WriteFnResult, advanced::MemoryExecutable,
  relocations::Relocation,
};

pub struct JITMemoryManager {
  root: Box<str>,
  blockid: u8,
  quick: Vec<MemoryExecutable>,
  equilibrium: Vec<MemoryExecutable>,
}

impl JITMemoryManager {
  pub fn new(root: &str) -> Self {
    #[cfg(windows)]
    let path = format!("{root}\\jit_block_0");

    #[cfg(unix)]
    let path = format!("{root}/jit_block_0");

    // Get a 32MB Chunk for TEMP Storage
    let a = MemoryExecutable::new_slab(path, Some(NonZeroU8::new(2).unwrap()));

    Self {
      root: root.into(),
      blockid: 1,
      quick: vec![a],
      equilibrium: vec![],
    }
  }

  pub fn alloc_quick_new(&mut self) {
    #[cfg(windows)]
    let path = format!("{}\\jit_block_{}", &self.root, self.blockid);

    #[cfg(unix)]
    let path = format!("{}/jit_block_{}", &self.root, self.blockid);

    self.blockid += 1;

    self.quick.push(MemoryExecutable::new_slab(
      path,
      Some(NonZeroU8::new(2).unwrap()),
    ));
  }

  // Max executable JIT blob is ~<32MB (for sanity)
  const MAX_EXEC_SIZE: usize = 31 * 1024 * 1024 + 1023 * 1024;

  pub fn write_quick(&mut self, data: Arc<[u8]>, relocs: Arc<[Relocation]>) -> *const Executable {
    // Try to fit larger ones
    // optimistically
    //
    // If it pays off separately - we'll think
    let mut out = null();
    let succ = self
      .quick
      .iter_mut()
      .any(|x| match x.write_fn(&data, &relocs) {
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

      if alc > u8::MAX as usize {
        panic!("SaVM [CRITICAL] : This codebase is not possible to be correctly handled!");
      }

      #[cfg(windows)]
      let path = format!("{}\\jit_block_{}", &self.root, self.blockid);

      #[cfg(unix)]
      let path = format!("{}/jit_block_{}", &self.root, self.blockid);

      self.blockid += 1;

      let mut m = MemoryExecutable::new_slab(path, Some(NonZeroU8::new(alc as u8).unwrap()));

      let out = match m.write_fn(&data, &relocs) {
        WriteFnResult::Executable(ex) => ex,
        _ => panic!("Reached a position where calculation is not idompotent"),
      };

      self.quick.push(m);

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
      .chain(self.equilibrium.drain(..))
      .for_each(|x| {
        // Leak since its not dropped
        // Safety
        x.leak();

        #[cfg(debug_assertions)]
        panic!("Please ensure that SaVM is programmed correctly, the Drop method should not encounter stray Memory blobs!");
      });
  }
}
