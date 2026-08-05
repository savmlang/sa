macro_rules! instloader {
  (
    $(
      $(#[$meta:meta])*
      $name:ident {$(.$t:ty,)? $( $imm:ident: $ty:ty ),* } ($($arg:ident),*) -> ($($out:ident),*)
      lower { $inst:expr } $f:expr
    )*
  ) => {
    instloader!(
      final {
        $(
          $(#[$meta])*
          $name { $( typedata: $t, )? $( $imm: $ty ),* } ($($arg),*) -> ($($out),*)
          lower { $inst } $f
        )*
      }
    );
  };
  (
    final {
      $(
        $(#[$meta:meta])*
        $name:ident { $( $imm:ident: $ty:ty ),* $(,)? } ($($arg:ident),*) -> ($($out:ident),*)
        lower { $inst:expr } $f:expr
      )*
    }
  ) => {
    pastey::paste! {
    /// `V*` instructions support BOTH vector and scalar values
    /// non `V` prefixed instructions are scalar only
    ///
    /// `V_` prefixed instructions mean that they selectively accept vectors
    pub enum LLInstruction {
      $(
        $(#[$meta])*
        $name {
          $(
            $arg: LocSrc,
          )*

          $(
            $out: LocSrc,
          )*

          $(
            $imm: $ty,
          )*
        }
      ),*
    }

    pub mod llprelude {
      use super::*;

      $(
        pub fn [<inst_ $name:lower>](
          $(
            $imm: $ty,
          )*
          $(
            $arg: LocSrc,
          )*
          $(
            $out: LocSrc,
          )*
        ) -> LLInstruction {
          LLInstruction::[<inst_ $name:lower>]($($imm,)*$($arg,)*$($out,)*)
        }
      )*
    }

    impl LLInstruction {
      $(
        pub fn [<inst_ $name:lower>](
          $(
            $imm: $ty,
          )*
          $(
            $arg: LocSrc,
          )*
          $(
            $out: LocSrc,
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

      pub fn lower(&self, buf: &mut Vec<u8>) {
        match self {
          $(
            Self::$name {
              $(
                $arg,
              )*

              $(
                $out,
              )*

              $(
                $imm,
              )*
            } => {
              buf.push($inst);
              $f(buf, $($imm,)* $($arg,)* $($out,)*);
            }
          ),*
        }
      }

      pub fn format(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
          $(
            Self::$name {
              $(
                $arg,
              )*
              $(
                $out,
              )*
              $(
                $imm,
              )*
            } => {
              let name = stringify!([<$name:lower>]);

              // Show output if available
              #[allow(unused)]
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

              // Pass Immediates
              $(
                write!(f, " ")?;
                $imm.f(f)?;
              )*

              // Pass Args First
              $(
                write!(f, " ")?;
                $arg.f(f)?;
              )*
            }
          ),*
        }
        write!(f, "")
      }
    }
    }
  };
}
