use crate::inst::dir::SAVM_PATH_DATA;
use std::{fs, io, iter, path::Path};
use windows_registry::LOCAL_MACHINE;

pub fn setup_registry(savm: &str, setpath: bool) {
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
    _ = uninstall.set_u32("NoModify", 0);
    _ = uninstall.set_u32("NoRepair", 1);
    _ = uninstall.set_string(
      "DisplayIcon",
      format!("{savm}\\savmsetupconfigureuninstallrepair.exe"),
    );
    _ = uninstall.set_string(
      "UninstallString",
      format!("\"{}\\savmsetupconfigureuninstallrepair.exe\" uninstall", savm),
    );

    _ = uninstall.set_string(
      "ModifyPath",
      format!(
        "\"{}\\savmsetupconfigureuninstallrepair.exe\" repair --prompt",
        savm
      ),
    );

    if let Ok(size) = get_dir_size(savm) {
      _ = uninstall.set_u32("EstimatedSize", (size / 1024) as _);
    }
  }

  // Path
  {
    let env = LOCAL_MACHINE
      .options()
      .write()
      .read()
      .open(r"System\CurrentControlSet\Control\Session Manager\Environment")
      .expect("Unable to open PATH");

    let path = env.get_string("Path").unwrap_or_default();

    let pathval = path
      .split(";")
      .filter(|x| !x.is_empty())
      .filter(|x| *x != SAVM_PATH_DATA)
      .chain(iter::repeat_n(SAVM_PATH_DATA, if setpath { 1 } else { 0 }))
      .fold(String::default(), |mut out, x| {
        out += x;
        out += ";";

        out
      });

    env.set_expand_string("Path", pathval).unwrap();
  }
}

fn get_dir_size(path: impl AsRef<Path>) -> io::Result<u64> {
  let mut size = 0;
  for entry in fs::read_dir(path)? {
    let entry = entry?;
    let metadata = entry.metadata()?;

    if metadata.is_symlink() {
      continue;
    } else if metadata.is_dir() {
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

  // Path
  {
    let env = LOCAL_MACHINE
      .options()
      .write()
      .read()
      .open(r"System\CurrentControlSet\Control\Session Manager\Environment")
      .expect("Unable to open PATH");

    let path = env.get_string("Path").unwrap();

    let pathval = path
      .split(";")
      .filter(|x| !x.is_empty())
      .filter(|x| *x != SAVM_PATH_DATA)
      .fold(String::default(), |mut out, x| {
        out += x;
        out += ";";

        out
      });

    env.set_expand_string("Path", pathval).unwrap();
  }
}
