use core::{slice, str};
use sasm::PathInfo;
use std::borrow::Cow;

cprelude::cprelude! {
  Sasm
}

#[repr(C)]
/// This describes the Path Information used by SASM runner
pub struct IPathInfo {
  /// The directory where assembly files are stored
  pub bindir: Sasm_IStr,
  /// The output directory
  pub distdir: Sasm_IStr,
}

#[no_mangle]
/// This runs sasm producing the desired sideffect
///
/// Please note that this is show the terminal progressbar
/// and other artifacts currently.
pub unsafe extern "C" fn sasm_run(pathinfo: IPathInfo) {
  unsafe {
    sasm::sasm(PathInfo {
      bindir: Cow::Borrowed(str::from_utf8_unchecked(slice::from_raw_parts(
        pathinfo.bindir.data as _,
        pathinfo.bindir.len,
      ))),
      distdir: Cow::Borrowed(str::from_utf8_unchecked(slice::from_raw_parts(
        pathinfo.distdir.data as _,
        pathinfo.distdir.len,
      ))),
    })
  };
}
