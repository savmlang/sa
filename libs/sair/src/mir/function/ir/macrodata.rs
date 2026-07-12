#[macro_export]
macro_rules! implement {
  (
    $(
      $(#[$meta:meta])*
      fn $name:ident($ctx:ident $(,$a:ident)* $(,)? ) $(-> $($b:ty),*)? {
        $(immediates {
          $($imm:ident: $immt:ty),*
        })?

        $(verify: $verify:block,)?
        process: $code:block
      }
    )*
  ) => {
    $(
      #[allow(unused_parens)]
      $(#[$meta])*
      pub fn $name(&mut self $(, $a: ValueId)* $(, $($imm: $immt),*)?) -> Result<Instruction<( $($($b),*)? )>, CommonError> {
        let $ctx = self;

        $(
          $verify;
        )?

        let out = {
          $code
        };

        Ok(out)
      }
    )*
  };
}

macro_rules! typecheck {
  ($ctx:ident, $($a:ident),* $( { $f:ident } )?) => {
    if ![$( $ctx.type_of($a) ),*]
      .windows(2)
      .all(|d| {
        let (x, v) = d[0];
        let (y, _) = d[1];

        x == y
          $( && v.$f() )?
      }) {
        return Err(CommonError::TypeVerificationFailure);
      }
  };
}
