#include "module.h"

extern void NEXT(DispatchStarter *dsp);

JITFN
void cinderjit_nop(DispatchStarter *state)
{
  BECOME(NEXT(state));
}