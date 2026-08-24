#include "module.h"
#include "utils/arith.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

#define UNARY_LOOP(TYPE, EXPR, A, OUT, COUNT, OF_A, OF_TGT)   \
  do                                                           \
  {                                                            \
    TYPE *a_ = (TYPE *)(A);                                    \
    TYPE *out_ = (TYPE *)(OUT);                                \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)           \
    {                                                          \
      TYPE v = a_[(OF_A) + i];                                 \
      out_[(OF_TGT) + i] = (EXPR);                             \
    }                                                          \
  } while (0)

JITFN
void cinderjit_vabs(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  switch (prelude.datatype)
  {
  case 4:
    UNARY_LOOP(int64_t, (v < 0 ? -v : v), prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt);
    break;
  case 5:
    UNARY_LOOP(int32_t, (v < 0 ? -v : v), prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt);
    break;
  case 6:
    UNARY_LOOP(int16_t, (v < 0 ? -v : v), prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt);
    break;
  case 7:
    UNARY_LOOP(int8_t, (v < 0 ? -v : v), prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt);
    break;
  case 0:
  case 8:
    UNARY_LOOP(double, __builtin_fabs(v), prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt);
    break;
  case 1:
  case 9:
    UNARY_LOOP(float, __builtin_fabsf(v), prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt);
    break;
  default:
    break;
  }

  BECOME(NEXT(state));
}
