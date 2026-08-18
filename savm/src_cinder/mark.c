#include "module.h"

extern void NEXT(DispatchStarter *dsp);
extern void RETURN_RESUME(DispatchStarter *dsp);

extern char MARKER[];

JITFN
void cinderjit_mark(DispatchStarter *state)
{
  state->hotnessOrResume++;

  if (state->hotnessOrResume >= 256)
  {
    uint64_t marker = (uint64_t)MARKER;
    
    state->taskstate->curline_or_resume = marker;
    BECOME(RETURN_RESUME(state));
  }

  BECOME(NEXT(state));
}
