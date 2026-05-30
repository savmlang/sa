use sysinfo::{CpuRefreshKind, RefreshKind, System};

pub fn get_cpuname() -> Vec<u8> {
  let mut s = System::new_with_specifics(RefreshKind::nothing().with_cpu(CpuRefreshKind::everything()));

  s.cpus()
    .first()
    .map(|cpu| cpu.brand().as_bytes().to_vec())
    .unwrap_or_else(|| b"Unknown CPU".to_vec())
}
