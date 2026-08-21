#include "module.h"
#include "utils/arith.h"
#include "utils/mul.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vmul(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);
  uint64_t instdefined = prelude.instdefined;

  uint8_t eflags = (uint8_t)(instdefined >> 14);
  bool wide = (eflags & 0x03) == 1;
  bool lowbits = (eflags & 0x01) == 0;

  if (wide)
  {
    switch (prelude.datatype)
    {
    case 0:
      INTOP_MUL_WIDE(uint64_t, unsigned __int128, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 64);
      break;
    case 1:
      INTOP_MUL_WIDE(uint32_t, uint64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 32);
      break;
    case 2:
      INTOP_MUL_WIDE(uint16_t, uint32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 16);
      break;
    case 3:
      INTOP_MUL_WIDE(uint8_t, uint16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 8);
      break;

    case 4:
      INTOP_MUL_WIDE_S(int64_t, __int128, unsigned __int128, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 64);
      break;
    case 5:
      INTOP_MUL_WIDE_S(int32_t, int64_t, uint64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 32);
      break;
    case 6:
      INTOP_MUL_WIDE_S(int16_t, int32_t, uint32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 16);
      break;
    case 7:
      INTOP_MUL_WIDE_S(int8_t, int16_t, uint16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 8);
      break;

    default:
      break;
    }
  }
  else if (lowbits)
  {
    switch (prelude.datatype)
    {
    case 0:
    case 4:
      INTOP(uint64_t, *, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 1:
    case 5:
      INTOP(uint32_t, *, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 2:
    case 6:
      INTOP(uint16_t, *, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 3:
    case 7:
      INTOP(uint8_t, *, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
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
      INTOP_MUL_HIGH(uint64_t, unsigned __int128, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 64);
      break;
    case 1:
      INTOP_MUL_HIGH(uint32_t, uint64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 32);
      break;
    case 2:
      INTOP_MUL_HIGH(uint16_t, uint32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 16);
      break;
    case 3:
      INTOP_MUL_HIGH(uint8_t, uint16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 8);
      break;

    case 4:
      INTOP_MUL_HIGH_S(int64_t, __int128, unsigned __int128, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 64);
      break;
    case 5:
      INTOP_MUL_HIGH_S(int32_t, int64_t, uint64_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 32);
      break;
    case 6:
      INTOP_MUL_HIGH_S(int16_t, int32_t, uint32_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 16);
      break;
    case 7:
      INTOP_MUL_HIGH_S(int8_t, int16_t, uint16_t, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt, 8);
      break;

    default:
      break;
    }
  }

  BECOME(NEXT(state));
}