#include "module.h"
#include "utils/cast.h"

extern void NEXT(DispatchStarter *dsp);

CASTPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vfcast(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  CastPrelude prelude = parse_castprelude(task, (uint64_t)TAGS_SRC_TGT, (uint64_t)COUNT, (uint64_t)OF_SRC_TGT);

  execute_cast(prelude.tag_initial, prelude.tag_final, prelude.src, prelude.tgt, prelude.count, prelude.of_src, prelude.of_tgt);

  BECOME(NEXT(state));
}
