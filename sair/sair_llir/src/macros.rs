#[macro_export]
macro_rules! instloader {
  (
    $(
      $(#[$meta:meta])*
      $name:ident { $( $imm:ident: $ty:ty ),* $(,)? } ( $($arg:ident),* ) -> ( $($out:ident),* )
      lower { $inst:expr } $f:expr
    )*
  ) => {
    $crate::pastey::paste! {
      /// `LLInstruction` is the low-level intermediate representation instruction set for SaVM.
      /// Implements all 32 opcodes defined in `sart::ctr`.
      #[derive(Debug, Clone, PartialEq, Eq)]
      pub enum LLInstruction {
        $(
          $(#[$meta])*
          $name {
            $(
              $imm: $ty,
            )*
            $(
              $arg: $crate::loc::LocSrc,
            )*
            $(
              $out: $crate::loc::LocSrc,
            )*
          }
        ),*
      }

      /// Convenience constructors in prelude
      pub mod llprelude {
        use super::*;

        $(
          #[inline]
          pub fn [<inst_ $name:snake>](
            $(
              $imm: $ty,
            )*
            $(
              $arg: $crate::loc::LocSrc,
            )*
            $(
              $out: $crate::loc::LocSrc,
            )*
          ) -> LLInstruction {
            LLInstruction::[<inst_ $name:snake>](
              $( $imm, )*
              $( $arg, )*
              $( $out, )*
            )
          }
        )*
      }

      impl LLInstruction {
        $(
          #[inline]
          pub fn [<inst_ $name:snake>](
            $(
              $imm: $ty,
            )*
            $(
              $arg: $crate::loc::LocSrc,
            )*
            $(
              $out: $crate::loc::LocSrc,
            )*
          ) -> Self {
            Self::$name {
              $(
                $imm,
              )*
              $(
                $arg,
              )*
              $(
                $out,
              )*
            }
          }
        )*

        /// Return the 1-byte opcode identifier according to `sart::ctr` constants.
        pub fn opcode(&self) -> u8 {
          match self {
            $(
              Self::$name { .. } => $inst,
            )*
          }
        }

        /// Return the mnemonic name of this instruction.
        pub fn name(&self) -> &'static str {
          match self {
            $(
              Self::$name { .. } => stringify!([<$name:snake>]),
            )*
          }
        }

        /// Lower this instruction directly into raw SaVM bytecode bytes.
        pub fn lower(&self, buf: &mut Vec<u8>) {
          match self {
            $(
              Self::$name {
                $(
                  $imm,
                )*
                $(
                  $arg,
                )*
                $(
                  $out,
                )*
              } => {
                buf.push($inst);
                $f(buf, $( $imm, )* $( $arg, )* $( $out, )*);
              }
            ),*
          }
        }

        /// Lower this instruction into a newly allocated `Vec<u8>`.
        pub fn to_bytes(&self) -> Vec<u8> {
          let mut buf = Vec::new();
          self.lower(&mut buf);
          buf
        }

        /// Format the instruction into assembly-like LLIR text.
        pub fn format(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          use $crate::format::LLFormat;
          match self {
            $(
              Self::$name {
                $(
                  $imm,
                )*
                $(
                  $arg,
                )*
                $(
                  $out,
                )*
              } => {
                let name = stringify!([<$name:snake>]);

                #[allow(unused_mut, unused_variables, unused_assignments)]
                let mut outputs = false;
                $(
                  $out.f(f)?;
                  write!(f, " ")?;
                  outputs = true;
                )*

                if outputs {
                  write!(f, "= ")?;
                }

                write!(f, "{name}")?;

                $(
                  write!(f, " ")?;
                  $imm.f(f)?;
                )*

                $(
                  write!(f, " ")?;
                  $arg.f(f)?;
                )*
              }
            ),*
          }
          Ok(())
        }

        /// Iterate over all input source operand locations.
        pub fn src_locs<F: FnMut(&$crate::loc::LocSrc)>(&self, mut cb: F) {
          match self {
            $(
              Self::$name {
                $(
                  $arg,
                )*
                ..
              } => {
                $(
                  cb($arg);
                )*
              }
            ),*
          }
        }

        /// Iterate over all destination/output operand locations.
        pub fn dst_locs<F: FnMut(&$crate::loc::LocSrc)>(&self, mut cb: F) {
          match self {
            $(
              Self::$name {
                $(
                  $out,
                )*
                ..
              } => {
                $(
                  cb($out);
                )*
              }
            ),*
          }
        }

        /// Iterate over all operand locations (both inputs and outputs).
        pub fn all_locs<F: FnMut(&$crate::loc::LocSrc)>(&self, mut cb: F) {
          self.src_locs(&mut cb);
          self.dst_locs(&mut cb);
        }
      }

      impl std::fmt::Display for LLInstruction {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          self.format(f)
        }
      }

      impl $crate::format::LLFormat for LLInstruction {
        fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
          self.format(f)
        }
      }
    }
  };
}
