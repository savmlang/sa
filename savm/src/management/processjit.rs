#[cfg(all(
  feature = "native",
  any(target_arch = "x86_64"),
  any(target_os = "windows", target_os = "linux")
))]
use crate::management::cinder;
#[cfg(all(feature = "native", feature = "cranelift"))]
use crate::management::jitmem::calculate_relocation_abs;
#[cfg(feature = "native")]
use crate::{
  BytecodeResolver, CacheData, CacheLevel,
  acaot::native::store::{Exec, SwappableCodeSpace},
  management::jitmem::JITMemoryManager,
};
#[cfg(feature = "native")]
use sajit::Executable;
#[cfg(feature = "native")]
use sart::code::SwappableCodeStore;
#[cfg(feature = "native")]
use std::process::abort;

#[cfg(feature = "native")]
pub fn process_jit<T: BytecodeResolver + Send + Sync + 'static>(
  resolver: &T,
  evmap: &SwappableCodeSpace,
  sajit: &mut JITMemoryManager,
  moduleid: u64,
  cache: CacheData,
) {
  // Upload
  match cache {
    CacheData::None => {}
    cache => {
      resolver.update_cache(moduleid, cache.clone());

      // Logical Safety : To the same thread that has `set` the value - it will never ever
      // be out of date on that exact core.
      // If it gets context switched to a different core - the updates will still be flushed.
      let write = |cinder: bool, bin: *const Executable, parent_counter: *mut usize| {
        if let Some(jitblob) = evmap.get(moduleid) {
          // Case `usize` back into the `*mut JIT`
          _ = unsafe { jitblob.set(0, Exec { exec: bin, cinder }, parent_counter, None) };
        } else {
          let mgr = SwappableCodeStore::new(Exec { exec: bin, cinder }, parent_counter);
          _ = unsafe { mgr.set(0, Exec { exec: bin, cinder }, parent_counter, None) };
          unsafe { evmap.set(moduleid, mgr) };
        }
      };

      match cache {
        CacheData::None | CacheData::Pickle { .. } => {}
        CacheData::CinderTempCache {
          binary: _stencil,
          entrymap: _entries,
        } => {
          #[cfg(all(
            feature = "native",
            any(target_arch = "x86_64"),
            any(target_os = "windows", target_os = "linux")
          ))]
          {
            let (bin, ctr) = cinder::link(_entries, _stencil, sajit);
            write(true, bin, ctr);
          }
        }
        CacheData::JITCache {
          level,
          binary: _binary,
          reloc: _reloc,
        } => match level {
          CacheLevel::Pickle => {
            // How did Jesus allow this honestly?
            abort();
          }
          level => match level {
            #[cfg(feature = "cranelift")]
            CacheLevel::CraneliftEpicenter => {
              todo!("Soon")
            }
            #[cfg(feature = "llvm")]
            CacheLevel::LLVMEpitome => {
              todo!("Soon");
            }
            CacheLevel::ACAoTCinder => {
              unreachable!();
            }
            #[cfg(feature = "llvm")]
            CacheLevel::LLVMCrater => {
              let (bin, parent_counter) = sajit
                .write_llvm(&_binary, |_| {
                  panic!("Crater and Cinder need not resolve pointers.");
                })
                .expect("Unable to write LLVM JIT Memory");
              write(false, bin, parent_counter);
            }
            #[cfg(feature = "cranelift")]
            CacheLevel::CraneliftCrafter => {
              let relocs = calculate_relocation_abs(&_reloc);

              let (bin, parent_counter) = sajit.write_quick(&_binary, &relocs);
              write(false, bin, parent_counter);
            }
            e => unreachable!("Found an unknown tier for current feature set : {e:?}"),
          },
        },
      }
    }
  }
}
