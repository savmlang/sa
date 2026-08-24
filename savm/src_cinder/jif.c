#include "module.h"
#include "utils/jif.h"

extern void NEXT(DispatchStarter *dsp);
extern void TAKEN_JUMP(DispatchStarter *dsp);

JIFPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_jif(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  JifPrelude prelude = parse_jifprelude(task, (uint64_t)INTENT_SRC_WIDTH, (uint64_t)OFFSET);

  bool not_zero = false;
  switch (prelude.width)
  {
  case 0:
    not_zero = ((uint64_t *)prelude.src)[prelude.offset] != 0;
    break;
  case 1:
    not_zero = ((uint32_t *)prelude.src)[prelude.offset] != 0;
    break;
  case 2:
    not_zero = ((uint16_t *)prelude.src)[prelude.offset] != 0;
    break;
  case 3:
    not_zero = ((uint8_t *)prelude.src)[prelude.offset] != 0;
    break;
  default:
    break;
  }

  if ((prelude.intent == 0 && !not_zero) || (prelude.intent != 0 && not_zero))
  {
    BECOME(TAKEN_JUMP(state));
  }

  BECOME(NEXT(state));
}
