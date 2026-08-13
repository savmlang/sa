#include <string.h>

#include "module.h"

extern JitFn NEXT;

extern uint8_t *SRC;
extern uintptr_t SIZE;

extern MemCpy MEMCPY;

JITFN
void cinderjit_wsput(DispatchStarter *state)
{
  MEMCPY(state->wsarr, SRC, (size_t)SIZE);

  BECOME(NEXT(state));
}