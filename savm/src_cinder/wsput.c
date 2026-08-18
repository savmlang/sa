#include <string.h>

#include "module.h"

extern void NEXT(DispatchStarter *dsp);

extern char SRC[];
extern char SIZE[];

JITFN
void cinderjit_wsput(DispatchStarter *state)
{
  state->wsarr(state->ws, (uint8_t *)SRC, (uintptr_t)SIZE);

  BECOME(NEXT(state));
}