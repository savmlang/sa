#include "module.h"

extern void NEXT(DispatchStarter *dsp);
extern void TAKEN_JUMP(DispatchStarter *dsp);

extern CRTFn CALL;
extern char PICKLE_IDX[];
extern char VERIFY[];

JITFN
void cinderjit_call_jumpable(DispatchStarter *state)
{
  CALL(state->pickle + (uintptr_t)PICKLE_IDX, state->ws, state->taskstate);

  if (state->taskstate->curline_or_resume == (uint64_t)VERIFY)
  {
    BECOME(TAKEN_JUMP(state));
  }

  BECOME(NEXT(state));
}