#pragma once
#include "../module.h"

FORCE_INLINE
void *resolve_loc(VMTaskState *tsk, uint8_t loc)
{
  switch (loc)
  {
  case 8:
    return tsk->scratchpad;
  case 9:
    return tsk->largepad;
  case 10:
    return (void *)tsk->r2;
  case 11:
    return (void *)tsk->r3;

  default:
    // returns a register
    return (void *)((uint8_t *)tsk + 8 * loc);
  }
}