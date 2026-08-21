#include "module.h"
#include "utils/divlike.h"
#include "utils/rem.h"

extern void NEXT(DispatchStarter *dsp);

DIVPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_rem(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  DivPrelude prelude = parse_divprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT);

  switch (prelude.datatype)
  {
  case 0:
    DIVOP(uint64_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;
  case 1:
    DIVOP(uint32_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;
  case 2:
    DIVOP(uint16_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;
  case 3:
    DIVOP(uint8_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;

  case 4:
    DIVOP(int64_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;
  case 5:
    DIVOP(int32_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;
  case 6:
    DIVOP(int16_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;
  case 7:
    DIVOP(int8_t, %, prelude.src1, prelude.src2, prelude.tgt, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    break;

  default:
    break;
  }

  BECOME(NEXT(state));
}
