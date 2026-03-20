use sart::ctr::VMTaskState;

use crate::{
  acaot::pickle::{def::PickleInstruction, implementation::WorkingSet},
  arrcastint,
};

struct NativeAsyncExecutor;

// impl NativeAsyncExecutor {
//   pub fn run(task: impl Future<Output = ()>) {}
// }

thread_local! {
  pub static EXEC: NativeAsyncExecutor = NativeAsyncExecutor;
}

pub fn call_synccall(pickle: &PickleInstruction, ws: &mut WorkingSet, taskstate: &mut VMTaskState) {
  let moduleid = arrcastint!(ws, start = 0, stop = 8, u64);
  unimplemented!("Synccall-asyncall will be implemented later!")
}

pub fn call_asynccall(
  _pickle: &PickleInstruction,
  _ws: &mut WorkingSet,
  _taskstate: &mut VMTaskState,
) {
  unimplemented!("Synccall-asyncall will be implemented later!")
}

pub fn call_spawn(_pickle: &PickleInstruction, _ws: &mut WorkingSet, _taskstate: &mut VMTaskState) {
  unimplemented!("Spawn will be implemented later!")
}

pub fn call_task(_pickle: &PickleInstruction, _ws: &mut WorkingSet, _taskstate: &mut VMTaskState) {
  unimplemented!("Task will be implemented later!")
}
