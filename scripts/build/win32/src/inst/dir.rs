use std::{fs, path::PathBuf};

use windows::{
  Win32::{
    System::Com::{CLSCTX_INPROC_SERVER, CoCreateInstance, IPersistFile},
    UI::Shell::{
      FOLDERID_CommonStartMenu, FOLDERID_ProgramFiles, IShellLinkW, KF_FLAG_DEFAULT,
      SHGetKnownFolderPath, ShellLink,
    },
  },
  core::{Interface, PCWSTR, Param, w},
};

use crate::inst::Config;

#[cfg(target_pointer_width = "64")]
pub static SAVM_PATH_DATA: &'static str = "%ProgramW6432%\\SaVM Runtime";

#[cfg(target_pointer_width = "32")]
pub static SAVM_PATH_DATA: &'static str = "%ProgramFiles(x86)%\\SaVM Runtime";

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
        fs::remove_file(&x.1).unwrap();
      });
  }

  if !config.sdk.linklibs {
    entries
      .iter()
      .filter(|x| x.0.ends_with(".dll.lib"))
      .for_each(|x| {
        fs::remove_file(&x.1).unwrap();
      });
  }

  if !config.sdk.staticarchives {
    entries
      .iter()
      .filter(|x| x.0.ends_with(".lib") && !x.0.ends_with(".dll.lib"))
      .for_each(|x| {
        fs::remove_file(&x.1).unwrap();
      });
  }

  if !config.tools.satest {
    entries
      .iter()
      .filter(|x| x.0.starts_with("satest"))
      .for_each(|x| {
        fs::remove_file(&x.1).unwrap();
      });
  }

  if !config.tools.saapprt {
    entries
      .iter()
      .filter(|x| x.0.starts_with("savmrt"))
      .for_each(|x| {
        fs::remove_file(&x.1).unwrap();
      });
  }

  // Clear Start Menu Entries first
  let start = unsafe { get_savm_startmenu() };

  _ = fs::remove_dir_all(&start);
  if config.w32.start {
    fs::create_dir_all(&start).unwrap();

    let utf16_path = dir
      .encode_utf16()
      .chain("\\savmsetupconfigureuninstallrepair.exe\0".encode_utf16())
      .collect::<Vec<_>>();

    let loc = start
      .encode_utf16()
      .chain("\\Uninstall SaVM.lnk\0".encode_utf16())
      .collect::<Vec<_>>();
    let loc_repair = start
      .encode_utf16()
      .chain("\\Repair SaVM.lnk\0".encode_utf16())
      .collect::<Vec<_>>();

    let path = PCWSTR::from_raw(utf16_path.as_ptr());
    let location = PCWSTR::from_raw(loc.as_ptr());
    let icon = Some(IconInfo {
      iicon: 0,
      path: PCWSTR(utf16_path.as_ptr()),
    });

    shelllink(
      w!("Uninstall SaVM"),
      path,
      Some(w!("uninstall")),
      icon,
      location,
    )
    .unwrap();

    shelllink(
      w!("Repair SaVM"),
      path,
      Some(w!("repair --prompt")),
      icon,
      PCWSTR::from_raw(loc_repair.as_ptr()),
    )
    .unwrap();

    drop(utf16_path);
    drop(loc);
    drop(loc_repair);
  }
}

fn shelllink<T: Param<PCWSTR>>(
  description: PCWSTR,
  path: PCWSTR,
  args: Option<PCWSTR>,
  icon: Option<IconInfo>,

  location: T,
) -> windows::core::Result<()> {
  unsafe {
    let link: IShellLinkW = CoCreateInstance(&ShellLink, None, CLSCTX_INPROC_SERVER)?;

    link.SetDescription(description)?;
    link.SetPath(path)?;

    if let Some(args) = args {
      link.SetArguments(args)?;
    }

    if let Some(icon) = icon {
      link.SetIconLocation(icon.path, icon.iicon)?;
    }

    let ipersistfile: IPersistFile = link.cast()?;

    ipersistfile.Save(location, true)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct IconInfo {
  pub path: PCWSTR,
  pub iicon: i32,
}
