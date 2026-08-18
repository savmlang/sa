#include "module.h"
#include "utils/arith.h"
#include "utils/add.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

JITFN
void cinderjit_vadd(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);
  uint64_t instdefined = prelude.instdefined;

  bool carry = (instdefined >> 15) == 1;
  bool saturate = ((instdefined >> 14) & 0b01) == 1;

  if (carry)
  {
    switch (prelude.datatype)
    {
    case 0:
      INTOP_ADD_CARRY_U(uint64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;
    case 1:
      INTOP_ADD_CARRY_U(uint32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;
    case 2:
      INTOP_ADD_CARRY_U(uint16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;
    case 3:
      INTOP_ADD_CARRY_U(uint8_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;

    case 4:
      INTOP_ADD_CARRY_S(int64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;
    case 5:
      INTOP_ADD_CARRY_S(int32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;
    case 6:
      INTOP_ADD_CARRY_S(int16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;
    case 7:
      INTOP_ADD_CARRY_S(int8_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, task->r5);
      break;

    default:
      break;
    }
  }
  else if (saturate)
  {
    switch (prelude.datatype)
    {
    case 0:
      INTOP_ADD_SAT_U(uint64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 1:
      INTOP_ADD_SAT_U(uint32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 2:
      INTOP_ADD_SAT_U(uint16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 3:
      INTOP_ADD_SAT_U(uint8_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
    case 4:
      INTOP_ADD_SAT_S(int64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 5:
      INTOP_ADD_SAT_S(int32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 6:
      INTOP_ADD_SAT_S(int16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 7:
      INTOP_ADD_SAT_S(int8_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
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
    case 4:
      INTOP(uint64_t, +, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 1:
    case 5:
      INTOP(uint32_t, +, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 2:
    case 6:
      INTOP(uint16_t, +, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 3:
    case 7:
      INTOP(uint8_t, +, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;

    default:
      break;
    }
  }

  BECOME(NEXT(state));
}