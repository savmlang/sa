use std::{
  ffi::c_void,
  mem::{replace, zeroed},
  ptr,
};

use sart::{
  boxed::{RTSafeBoxWrapper, drop_rtbox},
  ctr::{DispatchFn, REGISTER_SET_SIZE, VMTaskState},
  map::HeapStructure,
};

pub mod heaps;

mod alc;
mod arithmatic;
mod bitwise;
mod cmp;
mod ptrarith;
mod regmov;
mod threading;

pub use alc::*;
pub use arithmatic::*;
pub use bitwise::*;
pub use cmp::*;
pub use ptrarith::*;
pub use regmov::*;
pub use threading::*;

use crate::{BytecodeResolver, CVM, VM, sync::heaps::SYNC_HEAP};

macro_rules! inst_clr {
  (
    $($re:ident),*
  ) => {
    pastey::paste! {
      $(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_clr_ $re>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            (&mut *task).$re.nullify();
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_free_ $re>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let r = (&mut *task).$re.heap();

            debug_assert!(
              !r._checknull.is_null(),
              "inst_free called with a null register."
            );

            drop_rtbox(r.complex);
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_own_ $re>](
          vm: *mut c_void,
          task: *mut VMTaskState,
          addr: u64,
        ) {
          unsafe {
            let vm = &mut *(vm as *mut CVM);
            let task = &mut *task;

            *(&mut *task).$re.heap() = *(vm.heapmap as *mut HeapStructure).add(addr as usize);
          }
        }
      )*
    }
  };
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_free_addr(vm: *mut c_void, _: *mut VMTaskState, addr: u64) {
  unsafe {
    let data = ((&mut *(vm as *mut CVM)).heapmap.add(addr as _)) as *mut RTSafeBoxWrapper;

    drop_rtbox(data);
  }
}

inst_clr! {
  r1, r2, r3, r4, r5, r6
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_jmp(_: *mut c_void, task: *mut VMTaskState, index: u64) {
  unsafe {
    let task = &mut *task;

    task.curline = index as usize;
  }
}

macro_rules! jumps {
  (
    $($r:ident),*
  ) => {
    $(
      jumps! {
        u8 => $r,
        u16 => $r,
        u32 => $r,
        u64 => $r,
        i8 => $r,
        i16 => $r,
        i32 => $r,
        i64 => $r
      }
    )*
  };
  (
    $(
      $t:ty => $r:ident
    ),*
  ) => {
    $(
      jumps! {
        { $t } => { $r }
      }
    )*
  };
  (
    { $data:ty } => { $r:ident }
  ) => {
    pastey::paste! {
      #[inline(never)]
      #[unsafe(link_section = ".vm_fast_instructions")]
      pub extern "C" fn [<inst_jz_ $data _register_ $r>](_: *mut c_void, task: *mut VMTaskState, index: u64) {
        unsafe {
          let task = (&mut *task);
          let register = &mut task.$r;

          // Compare the inline 64-bit value directly. No pointer dereference.
          if register.heap().$data == 0 {
            task.curline = index as usize;
          }
        }
      }

      #[inline(never)]
      #[unsafe(link_section = ".vm_fast_instructions")]
      pub extern "C" fn [<inst_jnz_ $data _register_ $r>](_: *mut c_void, task: *mut VMTaskState, index: u64) {
        unsafe {
          let task = (&mut *task);
          let register = &mut task.$r;

          // Compare the inline 64-bit value directly. No pointer dereference.
          if register.heap().$data != 0 {
            task.curline = index as usize;
          }
        }
      }
    }
  };
}

jumps! {
  r1
}

macro_rules! jumpptr {
  (
    $($r:ident),*
  ) => {
    $(
      jumpptr! {
        u8 => $r,
        u16 => $r,
        u32 => $r,
        u64 => $r,
        i8 => $r,
        i16 => $r,
        i32 => $r,
        i64 => $r,
      }
    )*
  };
  (
    $(
      $t:ty => $r:ident
    ),*$(,)?
  ) => {
    $(
      jumpptr! {
        $t | $r
      }
    )*
  };
  (
    $data:ty | $r:ident
  ) => {
    pastey::paste! {
      #[inline(never)]
      #[unsafe(link_section = ".vm_fast_instructions")]
      /// Conditional Jump (Zero): Checks R1's *POINTER CONTENT* (u8) for zero.
      /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
      pub extern "C" fn [<inst_jz_ptr_ $data _register_ $r>](_: *mut c_void, task: *mut VMTaskState, index: u64) {
        unsafe {
          let task = &mut *task;
          // Assumes R1.ptr is pointing to a valid single byte (u8) for comparison.
          let data = *(task.$r.ptr as *const $data);

          if data == 0 {
            task.curline = index as usize;
          }
        }
      }

      #[inline(never)]
      #[unsafe(link_section = ".vm_fast_instructions")]
      /// Conditional Jump (Non-Zero): Checks R1's *POINTER CONTENT* (u8) for non-zero.
      /// SLOW PATH: Requires a memory load (dereference) from the heap/stack.
      pub extern "C" fn [<inst_jnz_ptr_ $data _register_ $r>](_: *mut c_void, task: *mut VMTaskState, index: u64) {
        unsafe {
          let task = &mut *task;
          // Assumes R1.ptr is pointing to a valid single byte (u8) for comparison.
          let data = *(task.$r.ptr as *const $data);

          if data != 0 {
            task.curline = index as usize;
          }
        }
      }
    }
  };
}

jumpptr! {
  r6
}

pub fn jz_map(ty: u8) -> DispatchFn {
  match ty {
    1 => inst_jz_u8_register_r1,
    2 => inst_jz_u16_register_r1,
    3 => inst_jz_u32_register_r1,
    4 => inst_jz_u64_register_r1,
    5 => inst_jz_i8_register_r1,
    6 => inst_jz_i16_register_r1,
    7 => inst_jz_i32_register_r1,
    8 => inst_jz_i64_register_r1,
    _ => unreachable!(),
  }
}

pub fn jnz_map(ty: u8) -> DispatchFn {
  match ty {
    1 => inst_jnz_u8_register_r1,
    2 => inst_jnz_u16_register_r1,
    3 => inst_jnz_u32_register_r1,
    4 => inst_jnz_u64_register_r1,
    5 => inst_jnz_i8_register_r1,
    6 => inst_jnz_i16_register_r1,
    7 => inst_jnz_i32_register_r1,
    8 => inst_jnz_i64_register_r1,
    _ => unreachable!(),
  }
}

pub fn jz_ptr_map(ty: u8) -> DispatchFn {
  match ty {
    1 => inst_jz_ptr_u8_register_r6,
    2 => inst_jz_ptr_u16_register_r6,
    3 => inst_jz_ptr_u32_register_r6,
    4 => inst_jz_ptr_u64_register_r6,
    5 => inst_jz_ptr_i8_register_r6,
    6 => inst_jz_ptr_i16_register_r6,
    7 => inst_jz_ptr_i32_register_r6,
    8 => inst_jz_ptr_i64_register_r6,
    _ => unreachable!(),
  }
}

pub fn jnz_ptr_map(ty: u8) -> DispatchFn {
  match ty {
    1 => inst_jnz_ptr_u8_register_r6,
    2 => inst_jnz_ptr_u16_register_r6,
    3 => inst_jnz_ptr_u32_register_r6,
    4 => inst_jnz_ptr_u64_register_r6,
    5 => inst_jnz_ptr_i8_register_r6,
    6 => inst_jnz_ptr_i16_register_r6,
    7 => inst_jnz_ptr_i32_register_r6,
    8 => inst_jnz_ptr_i64_register_r6,
    _ => unreachable!(),
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_clr_full(_: *mut c_void, task: *mut VMTaskState, _: u64) {
  unsafe {
    let task = &mut *task;

    ptr::write_bytes(
      &mut task.r1 as *mut _ as *mut u8, // Start address of r1
      0,                                 // Byte value to write (0)
      REGISTER_SET_SIZE,                 // Total bytes to clear (64 bytes)
    );
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn inst_sync_libcall<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  u: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    _ = replace(
      &mut vm.heapmap,
      SYNC_HEAP.with(|x| x.as_mut_unchecked().get()),
    );

    vm.counter += 1;

    if vm.counter > 10 {
      let mut state = Box::new(zeroed::<VMTaskState>());

      state.super_ = task as _;

      vm.run_module(state.as_mut(), u);

      drop(state);

      vm.counter -= 1;

      _ = replace(
        &mut vm.heapmap,
        SYNC_HEAP.with(|x| x.as_mut_unchecked().collect()),
      );
      return;
    }

    let mut state = zeroed::<VMTaskState>();

    state.super_ = task as _;

    vm.run_module(&mut state, u);

    drop(state);

    _ = replace(
      &mut vm.heapmap,
      SYNC_HEAP.with(|x| x.as_mut_unchecked().collect()),
    );

    vm.counter -= 1;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn new_context<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  _: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    _ = replace(
      &mut vm.heapmap,
      SYNC_HEAP.with(|x| x.as_mut_unchecked().get()),
    );

    let new_task: VMTaskState = zeroed();

    let old_task = Box::into_raw(Box::new(replace(task, new_task)));

    task.super_ = old_task;
  }
}

#[inline(never)]
#[unsafe(link_section = ".vm_fast_instructions")]
pub extern "C" fn restore_context<T: BytecodeResolver + Send + Sync + 'static>(
  vm: *mut c_void,
  task: *mut VMTaskState,
  _: u64,
) {
  unsafe {
    let vm = &mut *(vm as *mut VM<T>);
    let task = &mut *task;

    _ = replace(
      &mut vm.heapmap,
      SYNC_HEAP.with(|x| x.as_mut_unchecked().collect()),
    );

    let old_task_moved_from_heap = *Box::from_raw(task.super_);

    let new_task = &mut *task;
    drop(replace(new_task, old_task_moved_from_heap));
  }
}
