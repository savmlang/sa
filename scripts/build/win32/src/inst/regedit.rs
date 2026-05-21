use std::{fs, io, path::Path};

use windows_registry::LOCAL_MACHINE;

pub fn setup_registry(savm: &str) {
  let khlm_software = LOCAL_MACHINE
    .options()
    .write()
    .read()
    .create()
    .open("Software")
    .expect("Unable to open HLKM/Software");

  // remove all the old stuff
  {
    _ = khlm_software.remove_tree("SaVM Runtime");

    let savmrt = khlm_software
      .create("SaVM Runtime")
      .expect("Cannot create SaVM");
    _ = savmrt.set_string("Path", savm);
  }

  // New Uninstall Keys
  {
    let uninstall = khlm_software
      .create(r"Microsoft\Windows\CurrentVersion\Uninstall\SaVM Runtime")
      .expect("Unable to create");

    _ = uninstall.set_string("DisplayName", "SaVM Runtime");
    _ = uninstall.set_string("Publisher", "SaVM Official");
    _ = uninstall.set_u32("NoModify", 1);
    _ = uninstall.set_u32("NoRepair", 1);
    _ = uninstall.set_string("DisplayIcon", format!("{savm}\\savmuninstaller.exe"));
    _ = uninstall.set_string(
      "UninstallString",
      format!("\"{}\\savmuninstaller.exe\" uninstall", savm),
    );

    if let Ok(size) = get_dir_size(savm) {
      _ = uninstall.set_u32("EstimatedSize", (size / 1024) as _);
    }
  }
}

fn get_dir_size(path: impl AsRef<Path>) -> io::Result<u64> {
  let mut size = 0;
  for entry in fs::read_dir(path)? {
    let entry = entry?;
    let metadata = entry.metadata()?;
    if metadata.is_dir() {
      size += get_dir_size(entry.path())?;
    } else {
      size += metadata.len();
    }
  }
  Ok(size)
}

pub fn cleanup_registry() {
  let khlm_software = LOCAL_MACHINE
    .options()
    .write()
    .read()
    .create()
    .open("Software")
    .expect("Unable to open HLKM/Software");

  _ = khlm_software.remove_tree("SaVM Runtime");
  _ = khlm_software.remove_tree(r"Microsoft\Windows\CurrentVersion\Uninstall\SaVM Runtime");
}
