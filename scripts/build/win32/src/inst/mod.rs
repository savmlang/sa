use coreinstaller::extract;
use std::{borrow::Cow, env::current_exe, fs, path::PathBuf, thread::sleep, time::Duration};
use windows::Win32::{
  Storage::FileSystem::{MOVEFILE_DELAY_UNTIL_REBOOT, MoveFileExW},
  System::Threading::ExitProcess,
};
use windows_registry::HSTRING;

use crate::inst::regedit::{cleanup_registry, setup_registry};

mod dir;
mod regedit;

pub fn install_info<
  F: FnMut(Cow<'static, str>, f64) -> (),
  I: FnOnce() -> (),
  const AUTOEXIT: bool,
>(
  mut cb: F,
  installed: I,
) {
  cb(s("Creating Directories..."), 0.0);

  let path = unsafe { dir::get_savmdir() };
  _ = fs::remove_dir_all(&path);
  fs::create_dir_all(&path).expect("This shouldn't error, if it does we crash");

  let steps = 3.0;

  cb(s("Copying Core Runtime..."), 1.0 / steps);

  extract(&path);

  cb(s("Copying important files..."), 2.0 / steps);

  let cexe = current_exe().unwrap();
  _ = fs::copy(cexe, format!("{}/savmuninstaller.exe", &path));

  cb(s("Setting Up Registry..."), 3.0 / steps);

  setup_registry(&path);

  cb(s("Installed"), 1.0);
  installed();

  if AUTOEXIT {
    sleep(Duration::from_secs(3));
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
        _ = fs::remove_dir_all(&x.path());
        _ = fs::remove_file(&x.path());
      });
  }
  let mut pbuf = PathBuf::from(path);

  unsafe {
    let dir = HSTRING::from(pbuf.to_str().unwrap());
    pbuf.push("savmuninstaller.exe");
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
