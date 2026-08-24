#include "module.h"
#include "utils/arith.h"
#include "utils/rot.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vrot(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  bool right = (prelude.instdefined & 1) == 1;

  if (right)
  {
    switch (prelude.datatype)
    {
    case 0:
    case 4:
      INTOP_ROTR(uint64_t, rotr64, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 1:
    case 5:
      INTOP_ROTR(uint32_t, rotr32, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 2:
    case 6:
      INTOP_ROTR(uint16_t, rotr16, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 3:
    case 7:
      INTOP_ROTR(uint8_t, rotr8, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
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
      INTOP_ROTL(uint64_t, rotl64, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 1:
    case 5:
      INTOP_ROTL(uint32_t, rotl32, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 2:
    case 6:
      INTOP_ROTL(uint16_t, rotl16, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;
    case 3:
    case 7:
      INTOP_ROTL(uint8_t, rotl8, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt);
      break;

    default:
      break;
    }
  }

  BECOME(NEXT(state));
}
