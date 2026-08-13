#include "module.h"

extern JitFn NEXT;
extern CRTFn CALL;
extern uintptr_t PICKLE_IDX;
extern uint64_t VERIFY;
extern JitFn ELSE_JUMP;

JITFN
void cinderjit_call_jumpable(DispatchStarter *state)
{
  CALL(state->pickle + PICKLE_IDX, state->ws, state->taskstate);

  if (state->taskstate->curline_or_resume == VERIFY)
  {
    BECOME(ELSE_JUMP(state));
  }

  BECOME(NEXT(state));
}