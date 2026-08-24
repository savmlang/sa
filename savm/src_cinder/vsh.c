#include "module.h"
#include "utils/arith.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vsh(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  bool right = (prelude.instdefined & 1) == 1;

  if (right)
  {
    switch (prelude.datatype)
    {
    case 0:
      INTOP(uint64_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 1:
      INTOP(uint32_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 2:
      INTOP(uint16_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 3:
      INTOP(uint8_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 4:
      INTOP(int64_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 5:
      INTOP(int32_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 6:
      INTOP(int16_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 7:
      INTOP(int8_t, >>, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;

    default:
      break;
    }
  }
  else
  {
    switch (prelude.datatype)
    {
    case 0:
      INTOP(uint64_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 1:
      INTOP(uint32_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 2:
      INTOP(uint16_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 3:
      INTOP(uint8_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 4:
      INTOP(int64_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 5:
      INTOP(int32_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 6:
      INTOP(int16_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 7:
      INTOP(int8_t, <<, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;

    default:
      break;
    }
  }

  BECOME(NEXT(state));
}
