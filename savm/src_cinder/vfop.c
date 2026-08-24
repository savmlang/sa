#include "module.h"
#include "utils/arith.h"
#include "utils/fop.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vfop(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  uint8_t subop = (uint8_t)prelude.instdefined;
  uint8_t width = prelude.datatype;

  switch (width)
  {
  case 0:
  case 8:
  {
    double *s1 = (double *)prelude.src1;
    double *tgt = (double *)prelude.tgt;
    for (uint32_t i = 0; i < prelude.count; i++)
    {
      double v = s1[prelude.of_src1 + i];
      tgt[prelude.of_tgt + i] = fop64(subop, v);
    }
    break;
  }
  case 1:
  case 9:
  {
    float *s1 = (float *)prelude.src1;
    float *tgt = (float *)prelude.tgt;
    for (uint32_t i = 0; i < prelude.count; i++)
    {
      float v = s1[prelude.of_src1 + i];
      tgt[prelude.of_tgt + i] = fop32(subop, v);
    }
    break;
  }
  default:
    break;
  }

  BECOME(NEXT(state));
}
