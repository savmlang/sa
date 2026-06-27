pub mod os;
pub mod ports;
pub mod sallocator;

pub fn osprelude() {
  #[cfg(windows)]
  unsafe {
    use std::env::current_exe;

    use windows::{
      Win32::System::LibraryLoader::{
        AddDllDirectory, LOAD_LIBRARY_SEARCH_DEFAULT_DIRS, SetDefaultDllDirectories,
      },
      core::HSTRING,
    };

    let mut pth = current_exe().expect("Unable to load Win32 Process Directory");
    pth.pop();

    let dir = HSTRING::from(pth.to_str().expect("Cannot coerce to &str"));

    SetDefaultDllDirectories(LOAD_LIBRARY_SEARCH_DEFAULT_DIRS).expect("Could not instruct dirs");
    AddDllDirectory(&dir);
  }
}
