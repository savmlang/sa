#include "module.h"
#include "utils/cast.h"

extern void NEXT(DispatchStarter *dsp);

CASTPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_cast(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  CastPrelude prelude = parse_castprelude(task, (uint64_t)TAGS_SRC_TGT, 1, (uint64_t)OF_SRC_TGT);

  execute_cast(prelude.tag_initial, prelude.tag_final, prelude.src, prelude.tgt, 1, prelude.of_src, prelude.of_tgt);

  BECOME(NEXT(state));
}
