pub trait WideningMul: Sized + Copy {
  /// Returns (low, high)
  fn mul_widen(self, b: Self) -> (Self, Self);
}

macro_rules! implmulwide {
  (
    $(
      {
        implto: $num:ty,
        usi: $ucast:ty,
        cast: $cast:ty,
        bits: $bt:expr
      }
    ),*
  ) => {
    $(
      impl WideningMul for $num {
        #[inline]
        fn mul_widen(self, b: Self) -> (Self, Self) {
          let output = (self as $cast)*(b as $cast);

          (output as $num, ((output as $ucast) >> $bt) as $num)
        }
      }
    )*
  };
}

implmulwide! {
  {
    implto: u8,
    usi: u16,
    cast: u16,
    bits: 8
  },
  {
    implto: u16,
    usi: u32,
    cast: u32,
    bits: 16
  },
  {
    implto: u32,
    usi: u64,
    cast: u64,
    bits: 32
  },
  {
    implto: u64,
    usi: u128,
    cast: u128,
    bits: 64
  },
  {
    implto: i8,
    usi: u16,
    cast: i16,
    bits: 8
  },
  {
    implto: i16,
    usi: u32,
    cast: i32,
    bits: 16
  },
  {
    implto: i32,
    usi: u64,
    cast: i64,
    bits: 32
  },
  {
    implto: i64,
    usi: u128,
    cast: i128,
    bits: 64
  }
}
