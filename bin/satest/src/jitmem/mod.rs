use std::{collections::HashMap, mem::forget, time::Duration};

use savm::management::jitmem::JITMemoryManager;

pub mod run;

pub struct JITMemData {
  pub mem: Option<JITMemoryManager>,
  pub ptrstore: HashMap<(u64, &'static str), (*const (), Duration)>,
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

pub fn default() -> JITMemData {
  JITMemData {
    mem: Some(JITMemoryManager::new()),
    ptrstore: Default::default(),
  }
}
