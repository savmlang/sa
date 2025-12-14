use sart::ctr::DispatchFn;
use sart::ctr::VMTaskState;
use std::ffi::c_void;

macro_rules! regput {
  () => {
    regput! {
      make
        (0, u8),
        (1, u16),
        (2, u32),
        (3, u64),
        (4, i8),
        (5, i16),
        (6, i32),
        (7, i64),
        (8, f32),
        (9, f64)
    }
  };

  (
    make $(($xx:expr, $x:ident)),*
  ) => {
    regput! {
      generate $(
        (r1 -> $xx, $x),
        (r2 -> $xx, $x),
        (r3 -> $xx, $x),
        (r4 -> $xx, $x),
        (r5 -> $xx, $x),
        (r6 -> $xx, $x)
      ),*
    }
  };

  (generate $(($reg:ident -> $xx:expr, $x:ident)),*) => {
    pastey::paste! {
      #[allow(unreachable_patterns)]
      pub fn put_reg_handler(to: u8, typ: u8) -> DispatchFn {
        match to {
          1 => match typ {
            $(
              $xx => [<inst_reg_put_in_r1_and_ $x>],
            )*
            _ => unreachable!()
          },
          2 => match typ {
            $(
              $xx => [<inst_reg_put_in_r2_and_ $x>],
            )*
            _ => unreachable!()
          }
          3 => match typ {
            $(
              $xx => [<inst_reg_put_in_r3_and_ $x>],
            )*
            _ => unreachable!()
          }
          4 => match typ {
            $(
              $xx => [<inst_reg_put_in_r4_and_ $x>],
            )*
            _ => unreachable!()
          }
          5 => match typ {
            $(
              $xx => [<inst_reg_put_in_r5_and_ $x>],
            )*
            _ => unreachable!()
          }
          6 => match typ {
            $(
              $xx => [<inst_reg_put_in_r6_and_ $x>],
            )*
            _ => unreachable!()
          }
          _ => unreachable!()
        }
      }

      $(
        #[inline(never)]
        #[unsafe(link_section = ".vm_fast_instructions")]
        pub extern "C" fn [<inst_reg_put_in_ $reg _and_ $x>](_: *mut c_void, task: *mut VMTaskState, data: u64) {
          unsafe {
            let task = &mut *task;

            putup!($xx, r1, task, data);
          }
        }
      )*
    }
  };
}

macro_rules! putup {
  (0, $r:ident, $task:ident, $data:ident) => {
    $task.$r.u8 = $data as _;
  };
  (1, $r:ident, $task:ident, $data:ident) => {
    $task.$r.u16 = $data as _;
  };
  (2, $r:ident, $task:ident, $data:ident) => {
    $task.$r.u32 = $data as _;
  };
  (3, $r:ident, $task:ident, $data:ident) => {
    $task.$r.u64 = $data as _;
  };
  (4, $r:ident, $task:ident, $data:ident) => {
    $task.$r.i8 = i64::from_le_bytes($data.to_le_bytes()) as _;
  };
  (5, $r:ident, $task:ident, $data:ident) => {
    $task.$r.i16 = i64::from_le_bytes($data.to_le_bytes()) as _;
  };
  (6, $r:ident, $task:ident, $data:ident) => {
    $task.$r.i32 = i64::from_le_bytes($data.to_le_bytes()) as _;
  };
  (7, $r:ident, $task:ident, $data:ident) => {
    $task.$r.i64 = i64::from_le_bytes($data.to_le_bytes());
  };
  (8, $r:ident, $task:ident, $data:ident) => {
    $task.$r.f32 = f64::from_le_bytes($data.to_le_bytes()) as _;
  };
  (9, $r:ident, $task:ident, $data:ident) => {
    $task.$r.f64 = f64::from_le_bytes($data.to_le_bytes());
  };
}

regput! {}
