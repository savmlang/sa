use crate::llir::instr::loc::VMLoc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocSrc {
  pub reg: VMLoc,
  pub offset: i8,

  /// What's the canoical width
  pub width: usize,
  /// Upto how much does this span
  pub count: u8,
}
