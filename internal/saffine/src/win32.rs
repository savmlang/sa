use std::ops::{Div, Rem};
use windows::Win32::System::{
  SystemInformation::GROUP_AFFINITY,
  Threading::{GetCurrentThread, SetThreadGroupAffinity},
};

pub unsafe fn set_core(coreid: usize) -> Option<()> {
  unsafe {
    let hthread = GetCurrentThread();
    let groupaffinity = GROUP_AFFINITY {
      Group: coreid.div(64) as u16,
      Mask: 1 << coreid.rem(64),
      ..Default::default()
    };

    SetThreadGroupAffinity(hthread, &groupaffinity, None)
      .ok()
      .ok()
  }
}
