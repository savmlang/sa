#include "module.h"
#include "utils/arith.h"
#include "utils/cmp.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vcmp(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  uint8_t width = prelude.datatype;
  uint8_t op = (uint8_t)prelude.instdefined;
  uint64_t successval = prelude.count > 1 ? ~(uint64_t)0 : (uint64_t)1;

  if (op <= 9)
  {
    switch (width)
    {
    case 0:
    {
      uint64_t *s1 = (uint64_t *)prelude.src1;
      uint64_t *s2 = (uint64_t *)prelude.src2;
      uint64_t *tgt = (uint64_t *)prelude.tgt;
      for (uint32_t i = 0; i < prelude.count; i++)
      {
        uint64_t u1 = s1[prelude.of_src1 + i];
        uint64_t u2 = s2[prelude.of_src2 + i];
        bool cond = cmp_int(op, u1, u2, (int64_t)u1, (int64_t)u2);
        tgt[prelude.of_tgt + i] = cond ? (uint64_t)successval : 0;
      }
      break;
    }
    case 1:
    {
      uint32_t *s1 = (uint32_t *)prelude.src1;
      uint32_t *s2 = (uint32_t *)prelude.src2;
      uint32_t *tgt = (uint32_t *)prelude.tgt;
      for (uint32_t i = 0; i < prelude.count; i++)
      {
        uint32_t u1 = s1[prelude.of_src1 + i];
        uint32_t u2 = s2[prelude.of_src2 + i];
        bool cond = cmp_int(op, u1, u2, (int32_t)u1, (int32_t)u2);
        tgt[prelude.of_tgt + i] = cond ? (uint32_t)successval : 0;
      }
      break;
    }
    case 2:
    {
      uint16_t *s1 = (uint16_t *)prelude.src1;
      uint16_t *s2 = (uint16_t *)prelude.src2;
      uint16_t *tgt = (uint16_t *)prelude.tgt;
      for (uint32_t i = 0; i < prelude.count; i++)
      {
        uint16_t u1 = s1[prelude.of_src1 + i];
        uint16_t u2 = s2[prelude.of_src2 + i];
        bool cond = cmp_int(op, u1, u2, (int16_t)u1, (int16_t)u2);
        tgt[prelude.of_tgt + i] = cond ? (uint16_t)successval : 0;
      }
      break;
    }
    case 3:
    {
      uint8_t *s1 = (uint8_t *)prelude.src1;
      uint8_t *s2 = (uint8_t *)prelude.src2;
      uint8_t *tgt = (uint8_t *)prelude.tgt;
      for (uint32_t i = 0; i < prelude.count; i++)
      {
        uint8_t u1 = s1[prelude.of_src1 + i];
        uint8_t u2 = s2[prelude.of_src2 + i];
        bool cond = cmp_int(op, u1, u2, (int8_t)u1, (int8_t)u2);
        tgt[prelude.of_tgt + i] = cond ? (uint8_t)successval : 0;
      }
      break;
    }
    default:
      break;
    }
  }
  else
  {
    switch (width)
    {
    case 0:
    case 8:
    {
      double *s1 = (double *)prelude.src1;
      double *s2 = (double *)prelude.src2;
      uint64_t *tgt = (uint64_t *)prelude.tgt;
      for (uint32_t i = 0; i < prelude.count; i++)
      {
        double v1 = s1[prelude.of_src1 + i];
        double v2 = s2[prelude.of_src2 + i];
        bool cond = cmp_f64(op, v1, v2);
        tgt[prelude.of_tgt + i] = cond ? (uint64_t)successval : 0;
      }
      break;
    }
    case 1:
    case 9:
    {
      float *s1 = (float *)prelude.src1;
      float *s2 = (float *)prelude.src2;
      uint32_t *tgt = (uint32_t *)prelude.tgt;
      for (uint32_t i = 0; i < prelude.count; i++)
      {
        float v1 = s1[prelude.of_src1 + i];
        float v2 = s2[prelude.of_src2 + i];
        bool cond = cmp_f32(op, v1, v2);
        tgt[prelude.of_tgt + i] = cond ? (uint32_t)successval : 0;
      }
      break;
    }
    default:
      break;
    }
  }

  BECOME(NEXT(state));
}
