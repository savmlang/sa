#include "module.h"

extern void NEXT(DispatchStarter *dsp);
extern CRTFn CALL;
extern char PICKLE_IDX[];

JITFN
void cinderjit_call(DispatchStarter *state)
{
  CALL(state->pickle + (uintptr_t)PICKLE_IDX, state->ws, state->taskstate);
  BECOME(NEXT(state));
}