#include "module.h"
#include "utils/arith.h"
#include "utils/sub.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

JITFN
void cinderjit_vsub(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);
  uint64_t instdefined = prelude.instdefined;

  BECOME(NEXT(state));
}