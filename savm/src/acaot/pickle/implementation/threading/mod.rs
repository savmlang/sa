use sart::ctr::VMTaskState;

use crate::acaot::pickle::{def::PickleInstruction, implementation::WorkingSet};

pub fn call_synccall(
  _pickle: &PickleInstruction,
  _ws: &mut WorkingSet,
  _taskstate: &mut VMTaskState,
) {
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
