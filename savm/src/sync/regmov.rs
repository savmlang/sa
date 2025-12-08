use core::ffi::c_void;
use sart::ctr::{DispatchFn, VMTaskState};

macro_rules! regmov {
  () => {
    regmov! {
      generate r1, r2, r3, r4
    }

    regmov! {
      permute r5
      { r5 | 5, r6 | 6 }
    }
    regmov! {
      permute r6
      { r5 | 5, r6 | 6 }
    }
  };
  (generate $($r:ident),*) => {
    $(
      regmov! {
        permute $r
        { r1 | 1, r2 | 2, r3 | 3, r4 | 4 }
      }
    )*
  };
  (
    permute $reg:ident
    { $($intoreg:ident | $id:expr),* }
  ) => {
    pastey::paste! {
      pub fn [<generate_mov_from_ $reg>](to: u8) -> DispatchFn {
        match to {
          $(
            $id => [<inst_mov_from_ $reg _to_ $intoreg>],
          )*
          _ => unreachable!()
        }
      }

      pub fn [<generate_mov_super_from_ $reg>](to: u8) -> DispatchFn {
        match to {
          $(
            $id => [<inst_mov_from_super_ $reg _to_ $intoreg>],
          )*
          _ => unreachable!()
        }
      }

      $(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_mov_from_ $reg _to_ $intoreg>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            // A simple copy!
            task.$intoreg = task.$reg;
          }
        }

        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_mov_from_super_ $reg _to_ $intoreg>](_: *mut c_void, task: *mut VMTaskState, _: u64) {
          unsafe {
            let task = &mut *task;

            // A simple copy!
            task.$intoreg = (&*task.super_).$reg;
          }
        }
      )*
    }
  }
}

pub fn generate_mov(from: u8, to: u8) -> DispatchFn {
  match from {
    1 => generate_mov_from_r1(to),
    2 => generate_mov_from_r2(to),
    3 => generate_mov_from_r3(to),
    4 => generate_mov_from_r4(to),
    5 => generate_mov_from_r5(to),
    6 => generate_mov_from_r6(to),
    _ => unreachable!(),
  }
}

pub fn generate_mov_super(from: u8, to: u8) -> DispatchFn {
  match from {
    1 => generate_mov_super_from_r1(to),
    2 => generate_mov_super_from_r2(to),
    3 => generate_mov_super_from_r3(to),
    4 => generate_mov_super_from_r4(to),
    5 => generate_mov_super_from_r5(to),
    6 => generate_mov_super_from_r6(to),
    _ => unreachable!(),
  }
}

regmov! {}
