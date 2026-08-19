#pragma once
#include "../module.h"
#include "resolver.h"

typedef struct DivPrelude
{
  uint8_t datatype;

  void *src1, *src2, *tgt;

  int32_t of_src1, of_src2, of_tgt;
} DivPrelude;

#define DIVPRELUDEGEN()                      \
  /* 4x8-bits: datatype, src1, src2, tgt */ \
  extern char DATATYPE_SRC1_SRC2_TGT[];      \
  /* 2x32bits: of_src1, of_src2 */          \
  extern char OF_SRC1_SRC2[];                \
  /* 32bits: of_tgt */                      \
  extern char OF_TGT[];

#define DIVOP(TYPE, OP, A, B, OUT, OF_A, OF_B, OF_TGT) \
  do                                                   \
  {                                                    \
    TYPE *a_ = (TYPE *)(A);                            \
    TYPE *b_ = (TYPE *)(B);                            \
    TYPE *out_ = (TYPE *)(OUT);                        \
    out_[(OF_TGT)] = a_[(OF_A)] OP b_[(OF_B)];         \
  } while (0)

FORCE_INLINE
DivPrelude parse_divprelude(
    VMTaskState *task,
    uint64_t dt_src1_src2_tgt,
    uint64_t of_src1_src2,
    uint64_t of_tgt)
{
  DivPrelude out = {
      .datatype = (uint8_t)(dt_src1_src2_tgt & 0xFF),
      .src1 = resolve_loc(task, (uint8_t)((dt_src1_src2_tgt & 0xFF00) >> 8)),
      .src2 = resolve_loc(task, (uint8_t)((dt_src1_src2_tgt & 0xFF0000) >> 16)),
      .tgt = resolve_loc(task, (uint8_t)((dt_src1_src2_tgt & 0xFF000000) >> 24)),
      .of_src1 = (int32_t)(of_src1_src2),
      .of_src2 = (int32_t)(of_src1_src2 >> 32),
      .of_tgt = (int32_t)(of_tgt),
  };

  return out;
}
