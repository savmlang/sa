pub mod llvm;

macro_rules! polyfills {
  (
    $(
      $fname:ident(
        $( $a:ident : $aty:ty ),* // Fixed: ident first, then ty
      ) -> $out:ty {
        $compute:expr
      }
    )*
  ) => {
    $(
      pub extern "C" fn $fname(
        $($a: $aty),*
      ) -> $out {
        $compute
      }
    )*
  };
}

polyfills! {
  // Ceil
  ceil32(
    a: f32
  ) -> f32 {
    a.ceil()
  }
  ceil64(
    a: f64
  ) -> f64 {
    a.ceil()
  }

  // Floor
  floor32(
    a: f32
  ) -> f32 {
    a.floor()
  }
  floor64(
    a: f64
  ) -> f64 {
    a.floor()
  }

  // FMA
  fma32(
    a: f32,
    b: f32,
    c: f32
  ) -> f32 {
    a.mul_add(b, c)
  }
  fma64(
    a: f64,
    b: f64,
    c: f64
  ) -> f64 {
    a.mul_add(b, c)
  }

  // Trunc
  trunc32(
    a: f32
  ) -> f32 {
    a.trunc()
  }
  trunc64(
    a: f64
  ) -> f64 {
    a.trunc()
  }

  // Nearest
  nearest32(
    a: f32
  ) -> f32 {
    a.round()
  }
  nearest64(
    a: f64
  ) -> f64 {
    a.round()
  }
}
