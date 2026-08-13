#include "module.h"

extern JitFn NEXT;
extern CRTFn CALL;
extern uintptr_t PICKLE_IDX;

JITFN
void cinderjit_call(DispatchStarter *state)
{
  CALL(state->pickle + PICKLE_IDX, state->ws, state->taskstate);
  BECOME(NEXT(state));
}