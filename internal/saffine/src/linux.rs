pub unsafe fn set_core(coreid: usize) -> Option<()> {
  use libc::{CPU_SET, CPU_ZERO, cpu_set_t, sched_setaffinity};
  use std::mem::zeroed;

  let mut cpuset: cpu_set_t = zeroed();
  CPU_ZERO(&mut cpuset);

  CPU_SET(coreid, &mut cpuset);

  let _ = sched_setaffinity(0, std::mem::size_of::<cpu_set_t>(), &cpuset);

  Some(())
}
