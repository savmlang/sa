use crate::mir::block::instr::AHQF;

pub enum Count {
  Abs { abs: u32 },
  ReadFromR1,
}

impl AHQF for Count {
  fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      &Self::Abs { abs } => {
        write!(f, "{abs}")
      }
      Self::ReadFromR1 => write!(f, "@count:r1"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct VCopyMemFlags {
  pub volatile: bool,
  pub nonoverlapping: bool,

  pub srcalign: AlignData,
  pub tgtalign: AlignData,
}

impl VCopyMemFlags {
  pub fn lower(self, counttag: bool) -> u8 {
    let mut out = 0;

    for (id, op) in [
      (1 << 7, counttag),
      (1 << 5, self.volatile),
      (1 << 4, self.nonoverlapping),
    ] {
      if op {
        out |= id;
      }
    }

    // Alignment lowering
    {
      out |= self.srcalign.lower_vadd_style() << 2;
      out |= self.tgtalign.lower_vadd_style();
    }

    out
  }
}

impl Default for VCopyMemFlags {
  fn default() -> Self {
    Self {
      volatile: true,
      nonoverlapping: false,
      srcalign: AlignData::Unknown,
      tgtalign: AlignData::Unknown,
    }
  }
}

impl AHQF for VCopyMemFlags {
  fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    [
      ("volatile", self.volatile),
      ("nonoverlapping", self.nonoverlapping),
    ]
    .into_iter()
    .filter(|(_, t)| *t)
    .map(|(x, _)| x)
    .enumerate()
    .try_for_each(|(idx, flag)| {
      if idx != 0 {
        write!(f, " ")?;
      }

      write!(f, "{}", flag)
    })?;

    write!(f, "src:")?;
    self.srcalign.f(f)?;

    write!(f, " tgt:")?;
    self.tgtalign.f(f)
  }
}

#[derive(Debug, Default, Clone, Copy)]
pub enum AlignData {
  #[default]
  Unknown,
  B16,
  B32,
  B64,
}

impl AlignData {
  pub fn lower_vadd_style(self) -> u8 {
    match self {
      Self::Unknown => 0b00,
      Self::B16 => 0b01,
      Self::B32 => 0b10,
      Self::B64 => 0b11,
    }
  }
}
impl AHQF for AlignData {
  fn f(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Unknown => "align(def)",
        Self::B16 => "align(16)",
        Self::B32 => "align(32)",
        Self::B64 => "align(64)",
      }
    )
  }
}
