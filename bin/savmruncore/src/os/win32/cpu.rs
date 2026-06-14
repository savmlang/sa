use windows::{
  Win32::System::Registry::{
    HKEY_LOCAL_MACHINE, RRF_RT_REG_EXPAND_SZ, RRF_RT_REG_MULTI_SZ, RRF_RT_REG_SZ, RegGetValueW,
  },
  core::w,
};

pub fn get_cpuname() -> Vec<u8> {
  unsafe {
    let mut cpuname = [0u16; 256];

    let mut size: u32 = 2 * cpuname.len() as u32;

    RegGetValueW(
      HKEY_LOCAL_MACHINE,
      w!(r"HARDWARE\DESCRIPTION\System\CentralProcessor\0"),
      w!("ProcessorNameString"),
      RRF_RT_REG_SZ | RRF_RT_REG_EXPAND_SZ | RRF_RT_REG_MULTI_SZ,
      None,
      Some(cpuname.as_mut_ptr() as _),
      Some(&mut size),
    )
    .ok()
    .expect("Unexpected error");

    String::from_utf16(cpuname.get_unchecked(0..(size as usize / 2)))
      .expect("Unable to read string")
      .into_bytes()
  }
}
