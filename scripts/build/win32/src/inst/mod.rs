use coreinstaller::extract;
use std::{borrow::Cow, env::current_exe, fs, path::PathBuf, thread::sleep, time::Duration};
use windows::Win32::{
  Storage::FileSystem::{MOVEFILE_DELAY_UNTIL_REBOOT, MoveFileExW},
  System::Threading::ExitProcess,
};
use windows_registry::HSTRING;

use crate::inst::{
  dir::get_savm_startmenu,
  regedit::{cleanup_registry, setup_registry},
};

mod dir;
mod regedit;

#[derive(Debug)]
pub struct Config {
  pub sdk: SDKConfig,
  pub tools: ToolConfig,
  pub w32: WinConfig,
}

#[derive(Debug)]
pub struct ToolConfig {
  pub satest: bool,
  pub saapprt: bool,
}

#[derive(Debug)]
pub struct WinConfig {
  pub path: bool,
  pub start: bool,
}

#[derive(Debug)]
pub struct SDKConfig {
  pub staticarchives: bool,
  pub linklibs: bool,
  pub headers: bool,
}

pub fn install_info<
  F: FnMut(Cow<'static, str>, f64) -> (),
  I: FnOnce() -> (),
  const AUTOEXIT: bool,
>(
  mut cb: F,
  installed: I,
  config: Config,
  repair: bool,
) {
  let path;
  if repair {
    cb(s("Clearing old SaVM"), 0.0);

    path = unsafe { dir::get_savmdir() };

    for entry in fs::read_dir(&path).unwrap().map(Result::unwrap) {
      let path = entry.path();

      if entry.file_name() != "savmsetupconfigureuninstallrepair.exe" {
        fs::remove_file(&path)
          .or_else(|_| fs::remove_dir_all(&path))
          .expect("SaVM has another install currently running!");
      }
    }
  } else {
    cb(s("Creating Directories..."), 0.0);

    path = unsafe { dir::get_savmdir() };
    _ = fs::remove_dir_all(&path);
    fs::create_dir_all(&path).expect("This shouldn't error, if it does we crash");
  }

  let steps = 5.0;

  {
    cb(s("Copying Core Runtime..."), 1.0 / steps);

    extract(&path);
  }

  if !repair {
    cb(s("Copying important files..."), 2.0 / steps);

    let cexe = current_exe().unwrap();
    _ = fs::copy(
      cexe,
      format!("{}/savmsetupconfigureuninstallrepair.exe", &path),
    );
  }

  {
    cb(s("Configuring SaVM..."), 3.0 / steps);
    dir::configure(&path, &config);
  }

  {
    cb(s("Setting Up Registry..."), 4.0 / steps);

    setup_registry(&path, config.w32.path);
  }

  {
    cb(s("Successful"), 1.0);
    installed();
  }

  if AUTOEXIT {
    sleep(Duration::from_secs(10));
    unsafe { ExitProcess(0) };
  }
}

pub fn uninstall<I: FnOnce() -> (), const AUTOEXIT: bool>(done: I) {
  let path = unsafe { dir::get_savmdir() };

  // Autodelete as much as we can
  if let Ok(d) = fs::read_dir(&path) {
    d.map(Result::ok)
      .filter(Option::is_some)
      .map(Option::unwrap)
      .for_each(|x| {
        let path = x.path();

        _ = fs::remove_dir_all(&path);
        _ = fs::remove_file(&path);
      });
  }
  let mut pbuf = PathBuf::from(path);

  {
    unsafe {
      let start = get_savm_startmenu();
      fs::remove_dir_all(start).unwrap();
    }
  }

  unsafe {
    let dir = HSTRING::from(pbuf.to_str().unwrap());
    pbuf.push("savmsetupconfigureuninstallrepair.exe");
    let uninstaller = HSTRING::from(pbuf.to_str().unwrap());

    _ = MoveFileExW(&uninstaller, None, MOVEFILE_DELAY_UNTIL_REBOOT);
    _ = MoveFileExW(&dir, None, MOVEFILE_DELAY_UNTIL_REBOOT);
  }

  cleanup_registry();

  done();

  if AUTOEXIT {
    sleep(Duration::from_secs(3));
    unsafe { ExitProcess(0) };
  }
}

fn s<T>(t: T) -> Cow<'static, str>
where
  T: Into<Cow<'static, str>>,
{
  t.into()
}
