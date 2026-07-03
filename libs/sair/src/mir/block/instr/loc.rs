#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocSrc {
  pub reg: VMRegister,
  pub offset: i8,

  /// What's the canoical width
  pub width: usize,
  /// Upto how much does this span
  pub count: u8,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VMRegister {
  R1 = 0,
  R2 = 1,
  R3 = 2,
  R4 = 3,
  R5 = 4,
  R6 = 5,
  R7 = 6,
  R8 = 7,

  Scratchpad = 8,
  Largepad = 9,

  PtrFromR2 = 10,
  PtrFromR3 = 11,
}
