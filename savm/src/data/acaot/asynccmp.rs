use std::sync::Arc;

use sart::ctr::Instruction;
use tokio::task::spawn_blocking;

use crate::{BytecodeResolver, acaot::sync_compile};

pub async fn async_compile<T: BytecodeResolver + Send + Sync + 'static>(
  resolver: Arc<T>,
  module: u32,
  region: u32,
) -> Box<[Instruction]> {
  spawn_blocking(move || {
    let resolver = resolver;
    sync_compile(resolver.as_ref(), module, region)
  })
  .await
  .expect("Compile error")
}
