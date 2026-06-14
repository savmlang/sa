use windows::Win32::{
  Foundation::{CloseHandle, ERROR_INVALID_PARAMETER, STILL_ACTIVE},
  System::Threading::{GetExitCodeProcess, OpenProcess, PROCESS_QUERY_LIMITED_INFORMATION},
};

pub fn is_alive(procid: u32) -> bool {
  (|| unsafe {
    let proc = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, false, procid);

    let proc = match proc {
      Ok(hwnd) => hwnd,
      Err(e) if e.code() == ERROR_INVALID_PARAMETER.to_hresult() => return Some(false),
      Err(_) => return None,
    };

    let mut lpexit = 0;
    let c = GetExitCodeProcess(proc, &mut lpexit);

    _ = CloseHandle(proc);

    c.ok()?;

    Some(lpexit == STILL_ACTIVE.0.cast_unsigned())
  })()
  .unwrap_or(true)
}
