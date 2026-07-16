use std::cell::UnsafeCell;

use tokio::task_local;

use crate::{
  BytecodeResolver, VM,
  acaot::pickle::{
    def::{DISPATCH_TOTAL_ITEMS, pickle_generate_table_async},
    implementation::ResolveFnAsync,
  },
  sync::VMState,
};

task_local! {
  pub static VMSTAT_ASYNC: UnsafeCell<VMState>;// = UnsafeCell::new(VMState);
}

impl<E: BytecodeResolver + Send + Sync + 'static> VM<E> {
  pub const PICKLE_DISPATCH_TABLE_ASYNC: [ResolveFnAsync; DISPATCH_TOTAL_ITEMS] =
    pickle_generate_table_async::<E>();
}
