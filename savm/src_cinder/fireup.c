#include "module.h"

extern void CALL(DispatchStarter *dsp);

/// @brief Setup Stack Frame
JITFN
void cinderjit_fireup(VMTaskState *tsk)
{
  CALL((DispatchStarter *)tsk->misc);
}
