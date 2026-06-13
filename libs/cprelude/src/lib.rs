use core::slice;

pub use pastey;

#[macro_export]
macro_rules! cprelude {
  ($namespace:ident) => {
    cprelude::pastey::paste! {
      #[repr(C)]
      #[allow(non_camel_case_types)]
      /// Defines a UTF8 String Type characterized by
      ///
      /// data : pointer to the content
      /// len  : Total bytes (NOT characters)
      pub struct [<$namespace _ IStr>]{
        /// Pointer to the content
        pub data: *const ::core::ffi::c_char,
        /// Total Bytes
        pub len: usize
      }

      impl cprelude::Slicable for [<$namespace _ IStr>] {
        type Output = u8;

        fn data(&self) -> *const u8 {self.data as _}
        fn len(&self) -> usize {self.len}
      }
      impl cprelude::Strable for [<$namespace _ IStr>] {}

      #[repr(C)]
      #[allow(non_camel_case_types)]
      /// Defines a Binary Slice
      ///
      /// data : pointer to the content
      /// len  : Total bytes
      pub struct [<$namespace _ ISlice_Impl>]<T>{
        /// Pointer to the content
        pub data: *const T,
        /// Total Bytes
        pub len: usize
      }

      #[allow(non_camel_case_types)]
      pub type [<$namespace _ ISlice>] = [<$namespace _ ISlice_Impl>]<u8>;

      impl<T> cprelude::Slicable for [<$namespace _ ISlice_Impl>]<T> {
        type Output = T;

        fn data(&self) -> *const T {self.data as _}
        fn len(&self) -> usize {self.len}
      }

      #[repr(C)]
      #[allow(non_camel_case_types)]
      pub enum [<$namespace _ Maybe>]<T> {
        Some(T),
        None,
      }
    }
  };
}

pub trait Slicable {
  type Output;

  fn data(&self) -> *const Self::Output;
  fn len(&self) -> usize;

  fn to_bytes(&self) -> &[u8] {
    unsafe {
      slice::from_raw_parts(
        self.data() as *const u8,
        self.len() * size_of::<Self::Output>(),
      )
    }
  }

  fn to_slice(&self) -> &[Self::Output] {
    unsafe { slice::from_raw_parts(self.data(), self.len()) }
  }

  unsafe fn to_slice_raw<'a, 'b>(&'a self) -> &'b [Self::Output] {
    unsafe { slice::from_raw_parts(self.data(), self.len()) }
  }
}

pub trait Strable: Slicable {
  fn to_str(&self) -> &str {
    unsafe { str::from_utf8_unchecked(self.to_bytes()) }
  }
}
