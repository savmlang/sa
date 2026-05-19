pub trait IIntImpl: Sized + Copy {
  fn carryadd(self, rhs: Self, carry: bool) -> (Self, bool);
  fn borrowsub(self, rhs: Self, borrow: bool) -> (Self, bool);
}

/// https://doc.rust-lang.org/src/core/num/int_macros.rs.html#2555
/// https://doc.rust-lang.org/src/core/num/int_macros.rs.html#2662
macro_rules! impl_iint {
  ($($t:ty),*) => {
    $(
      impl IIntImpl for $t {
        #[inline]
        fn carryadd(
          self,
          rhs: Self,
          carry: bool
        ) -> (Self, bool) {
          let (r1, o1) = self.overflowing_add(rhs);
          let (r2, o2) = r1.overflowing_add(carry as Self);

          (r2, o1 != o2)
        }

        #[inline]
        fn borrowsub(
          self,
          rhs: Self,
          borrow: bool
        ) -> (Self, bool) {
          let (r1, o1) = self.overflowing_sub(rhs);
          let (r2, o2) = r1.overflowing_sub(borrow as Self);

          (r2, o1 != o2)
        }
      }
    )*
  };
}

impl_iint!(i8, i16, i32, i64, i128);
