use std::{collections::HashMap, mem::forget, time::Duration};

use savm::management::jitmem::JITMemoryManager;

pub mod run;

pub struct JITMemData {
  pub mem: Option<JITMemoryManager>,
  pub ptrstore: HashMap<(u64, &'static str), (*const (), Duration)>,
}

pub struct JITMems {
  pub general: JITMemData,

  pub epitier: [JITMemData; 2],
}

impl JITMemData {
  pub fn mem(&mut self) -> &mut JITMemoryManager {
    self.mem.as_mut().unwrap()
  }
}

impl Drop for JITMemData {
  fn drop(&mut self) {
    forget(self.mem.take().unwrap());
  }
}

pub fn default_mem() -> JITMemData {
  JITMemData {
    mem: Some(JITMemoryManager::new()),
    ptrstore: Default::default(),
  }
}

pub fn default() -> JITMems {
  JITMems {
    general: default_mem(),
    epitier: [default_mem(), default_mem()],
  }
}
