use std::fmt::{self, Formatter};
use crate::format::LLFormat;
use crate::loc::{LocSrc, VMLoc};

/// Flags for spawning worker threads and asynchronous tasks (`spawn`).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpawnFlags {
  pub hwnd: bool,
  pub task_out: Option<LocSrc>,
}

impl SpawnFlags {
  pub const fn new(hwnd: bool, task_out: Option<LocSrc>) -> Self {
    Self { hwnd, task_out }
  }

  pub fn lower(&self) -> u8 {
    let mut out = 0u8;
    if self.hwnd {
      out |= 1;
    }
    if let Some(to) = self.task_out {
      out |= (to.get_loc_bits() & 0x0F) << 1;
    }
    out
  }
}

impl LLFormat for SpawnFlags {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    if self.hwnd {
      write!(f, "hwnd")?;
      if let Some(to) = self.task_out {
        write!(f, "->")?;
        to.f(f)?;
      }
    }
    Ok(())
  }
}

impl fmt::Display for SpawnFlags {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Task control sub-operations (`task`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum TaskSubOp {
  AsyncDetach = 0,
  AsyncJoin = 1,
  AsyncIsComplete = 2,
  #[default]
  SyncDetach = 3,
  SyncJoin = 4,
  SyncIsComplete = 5,
  ThreadUnpark = 6,
  SyncThreadUnpark = 7,
  SyncThreadDetach = 8,
  SyncYield = 9,
  AsyncYield = 10,
  WaitMs = 11,
  Park = 12,
}

impl LLFormat for TaskSubOp {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::AsyncDetach => "async_detach",
        Self::AsyncJoin => "async_join",
        Self::AsyncIsComplete => "async_is_complete",
        Self::SyncDetach => "sync_detach",
        Self::SyncJoin => "sync_join",
        Self::SyncIsComplete => "sync_is_complete",
        Self::ThreadUnpark => "thread_unpark",
        Self::SyncThreadUnpark => "sync_thread_unpark",
        Self::SyncThreadDetach => "sync_thread_detach",
        Self::SyncYield => "sync_yield",
        Self::AsyncYield => "async_yield",
        Self::WaitMs => "wait_ms",
        Self::Park => "park",
      }
    )
  }
}

impl fmt::Display for TaskSubOp {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}

/// Register bitmask abstraction for passing ignored or tracked registers (e.g. for `synccall`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct RegBitmask(pub u8);

impl RegBitmask {
  pub const fn empty() -> Self {
    Self(0)
  }

  pub const fn all() -> Self {
    Self(0xFF)
  }

  pub const fn from_raw(raw: u8) -> Self {
    Self(raw)
  }

  pub const fn with(mut self, loc: VMLoc) -> Self {
    let bit = loc as u8;
    if bit < 8 {
      self.0 |= 1 << bit;
    }
    self
  }

  pub const fn without(mut self, loc: VMLoc) -> Self {
    let bit = loc as u8;
    if bit < 8 {
      self.0 &= !(1 << bit);
    }
    self
  }

  pub const fn contains(&self, loc: VMLoc) -> bool {
    let bit = loc as u8;
    if bit < 8 {
      (self.0 & (1 << bit)) != 0
    } else {
      false
    }
  }

  pub const fn raw(&self) -> u8 {
    self.0
  }
}

impl LLFormat for RegBitmask {
  fn f(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "ignore[")?;
    let mut first = true;
    for reg_idx in 0..8 {
      if (self.0 & (1 << reg_idx)) != 0 {
        if !first {
          write!(f, ",")?;
        }
        write!(f, "r{}", reg_idx + 1)?;
        first = false;
      }
    }
    if first {
      write!(f, "none")?;
    }
    write!(f, "]")
  }
}

impl fmt::Display for RegBitmask {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.f(f)
  }
}
