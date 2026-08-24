#include "module.h"
#include "utils/arith.h"
#include "utils/minimax.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vminimax(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  bool is_max = (prelude.instdefined & 1) == 1;

  if (is_max)
  {
    switch (prelude.datatype)
    {
    case 0: MINIMAX_LOOP(uint64_t, MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: MINIMAX_LOOP(uint32_t, MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: MINIMAX_LOOP(uint16_t, MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: MINIMAX_LOOP(uint8_t,  MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 4: MINIMAX_LOOP(int64_t,  MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 5: MINIMAX_LOOP(int32_t,  MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 6: MINIMAX_LOOP(int16_t,  MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 7: MINIMAX_LOOP(int8_t,   MAX_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 8: MINIMAX_LOOP(double,   max_f64(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 9: MINIMAX_LOOP(float,    max_f32(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    default: break;
    }
  }
  else
  {
    switch (prelude.datatype)
    {
    case 0: MINIMAX_LOOP(uint64_t, MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: MINIMAX_LOOP(uint32_t, MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: MINIMAX_LOOP(uint16_t, MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: MINIMAX_LOOP(uint8_t,  MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 4: MINIMAX_LOOP(int64_t,  MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 5: MINIMAX_LOOP(int32_t,  MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 6: MINIMAX_LOOP(int16_t,  MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 7: MINIMAX_LOOP(int8_t,   MIN_VAL(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 8: MINIMAX_LOOP(double,   min_f64(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 9: MINIMAX_LOOP(float,    min_f32(a, b), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    default: break;
    }
  }

  BECOME(NEXT(state));
}
