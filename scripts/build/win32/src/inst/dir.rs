use std::path::PathBuf;

use windows::Win32::UI::Shell::{FOLDERID_ProgramFiles, KF_FLAG_DEFAULT, SHGetKnownFolderPath};

pub unsafe fn get_savmdir() -> String {
  unsafe {
    let fb = || format!(r"C:\Program Files\SaVM Runtime");
    let Ok(fldr) = SHGetKnownFolderPath(&FOLDERID_ProgramFiles, KF_FLAG_DEFAULT, None) else {
      return fb();
    };

    let Ok(d) = fldr.to_string() else {
      return fb();
    };

    let mut pth = PathBuf::from(d);
    pth.push("SaVM Runtime");

    pth.to_str().map_or_else(|| fb(), |x| x.to_string())
  }
}
