#pragma once
#include "../module.h"
#include "resolver.h"

typedef struct ArithPrelude
{
  uint8_t datatype;

  void *src1, *src2, *tgt;

  uint32_t count;
  uint16_t instdefined;

  int32_t of_src1, of_src2, of_tgt;
} ArithPrelude;

#define ARITHPRELUDEGEN()                     \
  /* 4x8-bits + 32bits */                     \
  /* DATATYPE = idx= 0 */                     \
  extern char DATATYPE_SRC1_SRC2_TGT_COUNT[]; \
  /* 2x32bits */                              \
  extern char OF_SRC1_SRC2[];                 \
  /* consider 32bits */                       \
  extern char OF_TGT[];                       \
  /* consider 16bits */                       \
  extern char INSTDEFINED[];

#define INTOP(TYPE, OP, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT) \
  do                                                          \
  {                                                           \
    TYPE *a_ = (A);                                           \
    TYPE *b_ = (B);                                           \
    TYPE *out_ = (OUT);                                       \
    for (int i = 0; i < (int)(COUNT); i++)                    \
    {                                                         \
      out_[(OF_TGT) + i] = a_[(OF_A) + i] OP b_[(OF_B) + i];  \
    }                                                         \
  } while (0)

FORCE_INLINE
ArithPrelude parse_arithprelude(
    VMTaskState *task,
    uint64_t dt_src1_src2_tgt_count,
    uint64_t of_src1_src2,
    uint64_t of_tgt,
    uint64_t instdefined)
{
  ArithPrelude out = {
      .instdefined = (uint16_t)instdefined,
      .datatype = (uint8_t)(dt_src1_src2_tgt_count & 0xFF),
      .src1 = resolve_loc(task, (uint8_t)((dt_src1_src2_tgt_count & 0xFF00) >> 8)),
      .src2 = resolve_loc(task, (uint8_t)((dt_src1_src2_tgt_count & 0xFF0000) >> 16)),
      .tgt = resolve_loc(task, (uint8_t)((dt_src1_src2_tgt_count & 0xFF000000) >> 24)),
      .count = (uint32_t)(dt_src1_src2_tgt_count >> 32),
      .of_src1 = (int32_t)(of_src1_src2),
      .of_src2 = (int32_t)(of_src1_src2 >> 32),
      .of_tgt = (int32_t)(of_tgt),
  };

  return out;
}