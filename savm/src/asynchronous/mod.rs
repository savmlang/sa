use std::{
  cell::UnsafeCell,
  hint::cold_path,
  ops::Deref,
  ptr::addr_of_mut,
  sync::atomic::{Ordering, compiler_fence},
};

#[cfg(feature = "native")]
use sajit::Executable;
use sart::saffi::futures::FFIFuture;

use crate::{
  BytecodeResolver, CODE_CACHE, VM,
  acaot::pickle::{
    def::{
      DISPATCH_TOTAL_ITEMS, PICKLE_OPCODE_HINT, PICKLE_OPCODE_MARK, PickleInstruction,
      pickle_generate_table_async,
    },
    implementation::ResolveFnAsync,
  },
  kvwrap::{SaVMJumpWrap, SaVMJumpWrapImpl},
  sync::VMState,
};

pub mod pool;

/// An Async SaVM Implementation to aid the sync design of the VM
pub struct AsyncVMSubstrate<'a, E>
where
  E: BytecodeResolver + Send + Sync + 'static,
{
  vm: &'a VM<E>,
  vmstat: UnsafeCell<VMState>,
}

impl<'a, E: BytecodeResolver + Send + Sync + 'static> AsyncVMSubstrate<'a, E> {
  pub fn create(vm: &'a VM<E>) -> Self {
    Self {
      vm,
      vmstat: UnsafeCell::new(VMState::init()),
    }
  }
}

impl<'a, E: BytecodeResolver + Send + Sync + 'static> Deref for AsyncVMSubstrate<'a, E> {
  type Target = VM<E>;

  fn deref(&self) -> &Self::Target {
    self.vm
  }
}

impl<'a, E: BytecodeResolver + Send + Sync + 'static> AsyncVMSubstrate<'a, E> {
  pub const PICKLE_DISPATCH_TABLE_ASYNC: [ResolveFnAsync; DISPATCH_TOTAL_ITEMS] =
    pickle_generate_table_async::<E>();

  pub async fn call_section_async(&mut self, sectionid: u64) {
    return self.dispatch_chocolate_async::<true>(sectionid).await;
  }

  #[inline(always)]
  pub async fn dispatch_chocolate_async<const JMPTOJIT: bool>(&mut self, sectionid: u64) {
    let Some((data, jumps)) = CODE_CACHE.get(&sectionid) else {
      self.pickle_section(sectionid);

      return Box::pin(self.dispatch_chocolate_async::<JMPTOJIT>(sectionid)).await;
    };

    let leng = data.len();

    #[allow(unused)]
    let mut jumptomark = None;
    #[allow(unused)]
    let mut run_jit = false;

    let t = self.vmstat.get();

    unsafe {
      let wrapped = SaVMJumpWrap(jumps);

      (*t).ws.dispatch.dispatch_async = Self::PICKLE_DISPATCH_TABLE_ASYNC.as_ptr();
      (*t).ws.jmp = (0, wrapped.get(&0).unwrap_or_default());
      (*t).ws.relocmap = wrapped;

      let ts = (*t).ts.as_mut_ptr().add((*t).cindex as usize);

      (*ts).engine_or_pt.pt = self as *const _ as _;
      (*ts).curline_or_resume.usi = 0;

      'jcheck: loop {
        let dt = data.as_ref();

        #[cfg(feature = "native")]
        if JMPTOJIT {
          if let Some(_) = &crate::JIT_CACHE.get().unwrap_unchecked().get(sectionid) {
            if let Some(marker) = jumptomark {
              use sart::ctr::FLAGS::FLAG_JUMP_TO_RESUME;

              (*ts).flags = FLAG_JUMP_TO_RESUME;
              (*ts).curline_or_resume.unsigned = marker;
            }

            // Jump to JIT
            run_jit = true;

            break 'jcheck;
          };
        }

        // We use loop-in-a-loop to correctly manage state!
        // eg, yield that makes it re-check for JIT
        loop {
          if (*ts).curline_or_resume.usi == leng {
            break 'jcheck;
          }

          let pickle = dt.get_unchecked((*ts).curline_or_resume.usi);

          // USE A NO-OP to our benefit
          if pickle.opcode == PICKLE_OPCODE_HINT && PICKLE_OPCODE_MARK == pickle.u1 {
            let data: [PickleInstruction; 2] = {
              dt[((*ts).curline_or_resume.usi + 1)..(*ts).curline_or_resume.usi + 3]
                .try_into()
                .unwrap()
            };

            let out = u64::from_le_bytes([
              data[0].opcode,
              data[0].u1,
              data[0].u2,
              data[0].u3,
              data[1].opcode,
              data[1].u1,
              data[1].u2,
              data[1].u3,
            ]);

            jumptomark = Some(out);

            (*ts).curline_or_resume.usi += 3;

            continue 'jcheck;
          }

          if pickle.opcode == PICKLE_OPCODE_HINT {
            let dptr = dt.as_ptr();
            (*ts).engine_or_pt.pt = dptr as _;
          }

          // Ensure the state's reflected
          compiler_fence(Ordering::SeqCst);
          if let Some(fut) = (Self::PICKLE_DISPATCH_TABLE_ASYNC
            .get_unchecked(pickle.opcode as usize))(
            pickle, addr_of_mut!((*t).ws), ts
          ) {
            FFIFuture::new(fut).await;
          }
          compiler_fence(Ordering::SeqCst);

          (*ts).curline_or_resume.usi += 1;
        }
      }
    }

    #[cfg(feature = "native")]
    if run_jit {
      // TODO: Replace with `become`
      return self.dispatch_jit_async(sectionid);
    }

    cold_path();
    return self.ame_free(sectionid);
  }

  #[inline(always)]
  #[cfg(feature = "native")]
  pub fn dispatch_jit_async(&mut self, sectionid: u64) {
    use std::ops::Deref;

    use crate::JIT_CACHE;

    let Some(jitcache) = JIT_CACHE.get() else {
      unreachable!();
    };

    let Some(jit) = jitcache.get(sectionid) else {
      return self.dispatch_chocolate::<true>(sectionid);
    };

    let (_, exec) = jit.get();

    self.exec_jit_async(*exec.deref());

    drop(exec);

    return self.ame_free(sectionid);
  }

  #[inline(always)]
  #[cfg(feature = "native")]
  pub fn exec_jit_async(&mut self, exec: *const Executable) {
    unsafe {
      use sart::ctr::VMTaskState;
      use std::mem::transmute;

      let vmstate = self.vmstat.get();

      let curr_taskstate = (*vmstate).ts.as_mut_ptr().add((*vmstate).cindex);

      // Setup Pointers
      // todo!() figure out jumping
      {
        let task = &mut *curr_taskstate;

        task.engine_or_pt.pt = self as *const _ as *mut Self as _;
        task.ws_or_pt2.pt = &mut (*vmstate).ws as *mut _ as _;
      }

      // Execute
      let exec: extern "C" fn(vmtskstate: *mut VMTaskState) = transmute(exec);
      exec(curr_taskstate);
    }
  }
}
