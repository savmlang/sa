#include "module.h"
#include "utils/arith.h"
#include "utils/cnt.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vcnt(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  uint8_t op = (uint8_t)prelude.instdefined;
  uint8_t width = prelude.datatype;

  switch (op)
  {
  case 0: // popcnt
    switch (width)
    {
    case 0: CNTOP_LOOP(uint64_t, popcnt64, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 1: CNTOP_LOOP(uint32_t, popcnt32, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 2: CNTOP_LOOP(uint16_t, popcnt16, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 3: CNTOP_LOOP(uint8_t,  popcnt8,  prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    }
    break;
  case 1: // clz
    switch (width)
    {
    case 0: CNTOP_LOOP(uint64_t, clz64, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 1: CNTOP_LOOP(uint32_t, clz32, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 2: CNTOP_LOOP(uint16_t, clz16, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 3: CNTOP_LOOP(uint8_t,  clz8,  prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    }
    break;
  case 2: // cls (count leading sign bits / leading ones)
    switch (width)
    {
    case 0: CNTOP_LOOP(uint64_t, cls64, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 1: CNTOP_LOOP(uint32_t, cls32, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 2: CNTOP_LOOP(uint16_t, cls16, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 3: CNTOP_LOOP(uint8_t,  cls8,  prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    }
    break;
  case 3: // ctz
    switch (width)
    {
    case 0: CNTOP_LOOP(uint64_t, ctz64, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 1: CNTOP_LOOP(uint32_t, ctz32, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 2: CNTOP_LOOP(uint16_t, ctz16, prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    case 3: CNTOP_LOOP(uint8_t,  ctz8,  prelude.src1, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_tgt); break;
    }
    break;
  default:
    break;
  }

  BECOME(NEXT(state));
}
