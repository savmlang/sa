#include "module.h"
#include "utils/arith.h"
#include "utils/bit.h"

extern void NEXT(DispatchStarter *dsp);

ARITHPRELUDEGEN();

#pragma code_seg(push, ".jit_fn")
#pragma const_seg(push, ".jit_fn")

JITFN
void cinderjit_vbit(DispatchStarter *state)
{
  VMTaskState *task = state->taskstate;
  ArithPrelude prelude = parse_arithprelude(task, (uint64_t)DATATYPE_SRC1_SRC2_TGT_COUNT, (uint64_t)OF_SRC1_SRC2, (uint64_t)OF_TGT, (uint64_t)INSTDEFINED);

  uint8_t op = (uint8_t)prelude.instdefined;
  uint8_t width = prelude.datatype;

  switch (op)
  {
  case 0: // and
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, a & b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, a & b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, a & b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  a & b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 1: // or
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, a | b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, a | b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, a | b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  a | b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 2: // xor
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, a ^ b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, a ^ b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, a ^ b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  a ^ b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 3: // not
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, ~a, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, ~a, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, ~a, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  ~a, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 4: // or_not
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, a | ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, a | ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, a | ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  a | ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 5: // and_not
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, a & ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, a & ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, a & ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  a & ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 6: // xor_not
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, a ^ ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, a ^ ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, a ^ ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  a ^ ~b, prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 7: // bitrev
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, bitrev64(a), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, bitrev32(a), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, bitrev16(a), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  bitrev8(a),  prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  case 8: // bswap
    switch (width)
    {
    case 0: BITOP_LOOP(uint64_t, bswap64_val(a), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 1: BITOP_LOOP(uint32_t, bswap32_val(a), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 2: BITOP_LOOP(uint16_t, bswap16_val(a), prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    case 3: BITOP_LOOP(uint8_t,  bswap8_val(a),  prelude.src1, prelude.src2, prelude.tgt, prelude.count, prelude.of_src1, prelude.of_src2, prelude.of_tgt); break;
    }
    break;
  default:
    break;
  }

  BECOME(NEXT(state));
}
