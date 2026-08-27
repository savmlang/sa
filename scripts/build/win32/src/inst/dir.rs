use std::{fs, path::PathBuf};

use windows::Win32::UI::Shell::{
  FOLDERID_CommonStartMenu, FOLDERID_ProgramFiles, KF_FLAG_DEFAULT, SHGetKnownFolderPath,
};

use crate::inst::Config;

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

pub unsafe fn get_savm_startmenu() -> String {
  unsafe {
    let fb = || format!(r"C:\ProgramData\Microsoft\Windows\Start Menu");
    let Ok(fldr) = SHGetKnownFolderPath(&FOLDERID_CommonStartMenu, KF_FLAG_DEFAULT, None) else {
      return fb();
    };

    let Ok(d) = fldr.to_string() else {
      return fb();
    };

    let mut pth = PathBuf::from(d);
    pth.push("Programs");
    pth.push("SaVM Runtime");

    pth.to_str().map_or_else(|| fb(), |x| x.to_string())
  }
}

pub fn configure(dir: &str, config: &Config) {
  let entries = fs::read_dir(dir)
    .unwrap()
    .map(Result::unwrap)
    .map(|x| (x.file_name().into_string().unwrap(), x.path()))
    .collect::<Vec<_>>();

  if !config.sdk.headers {
    _ = fs::remove_dir_all(format!("{dir}/include"));
    entries
      .iter()
      .filter(|x| x.0.ends_with(".h"))
      .for_each(|x| {
        fs::remove_file(x.1).unwrap();
      });
  }

  if !config.sdk.linklibs {
    entries
      .iter()
      .filter(|x| x.0.ends_with(".dll.lib"))
      .for_each(|x| {
        fs::remove_file(x.1).unwrap();
      });
  }

  if !config.sdk.staticarchives {
    entries
      .iter()
      .filter(|x| x.0.ends_with(".lib") && !x.0.ends_with(".dll.lib"))
      .for_each(|x| {
        fs::remove_file(x.1).unwrap();
      });
  }

  if !config.tools.satest {
    entries
      .iter()
      .filter(|x| x.0.starts_with("satest"))
      .for_each(|x| {
        fs::remove_file(x.1).unwrap();
      });
  }

  if !config.tools.saapprt {
    entries
      .iter()
      .filter(|x| x.0.starts_with("savmrt"))
      .for_each(|x| {
        fs::remove_file(x.1).unwrap();
      });
  }

  // Clear Start Menu Entries first
  let start = unsafe { get_savm_startmenu() };

  _ = fs::remove_dir_all(&start);
  if config.w32.start {
    fs::create_dir_all(&start).unwrap();
  }
}
