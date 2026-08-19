#pragma once
#include "arith.h"

#define INTOP_MUL_WIDE(TYPE, WIDE_TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT, BITS) \
  do                                                                                \
  {                                                                                 \
    TYPE *a_ = (TYPE *)(A);                                                         \
    TYPE *b_ = (TYPE *)(B);                                                         \
    TYPE *out_ = (TYPE *)(OUT);                                                     \
                                                                                    \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                                \
    {                                                                               \
      WIDE_TYPE prod_ = (WIDE_TYPE)a_[(OF_A) + i] * (WIDE_TYPE)b_[(OF_B) + i];      \
      out_[(OF_TGT) + 2 * i] = (TYPE)prod_;                                         \
      out_[(OF_TGT) + 2 * i + 1] = (TYPE)(prod_ >> (BITS));                         \
    }                                                                               \
  } while (0)

#define INTOP_MUL_WIDE_S(TYPE, WIDE_TYPE, UWIDE_TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT, BITS) \
  do                                                                                              \
  {                                                                                               \
    TYPE *a_ = (TYPE *)(A);                                                                       \
    TYPE *b_ = (TYPE *)(B);                                                                       \
    TYPE *out_ = (TYPE *)(OUT);                                                                   \
                                                                                                  \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                                              \
    {                                                                                             \
      WIDE_TYPE prod_ = (WIDE_TYPE)a_[(OF_A) + i] * (WIDE_TYPE)b_[(OF_B) + i];                    \
      out_[(OF_TGT) + 2 * i] = (TYPE)prod_;                                                       \
      out_[(OF_TGT) + 2 * i + 1] = (TYPE)((UWIDE_TYPE)prod_ >> (BITS));                           \
    }                                                                                             \
  } while (0)

#define INTOP_MUL_HIGH(TYPE, WIDE_TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT, BITS) \
  do                                                                                \
  {                                                                                 \
    TYPE *a_ = (TYPE *)(A);                                                         \
    TYPE *b_ = (TYPE *)(B);                                                         \
    TYPE *out_ = (TYPE *)(OUT);                                                     \
                                                                                    \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                                \
    {                                                                               \
      WIDE_TYPE prod_ = (WIDE_TYPE)a_[(OF_A) + i] * (WIDE_TYPE)b_[(OF_B) + i];      \
      out_[(OF_TGT) + i] = (TYPE)(prod_ >> (BITS));                                 \
    }                                                                               \
  } while (0)

#define INTOP_MUL_HIGH_S(TYPE, WIDE_TYPE, UWIDE_TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT, BITS) \
  do                                                                                              \
  {                                                                                               \
    TYPE *a_ = (TYPE *)(A);                                                                       \
    TYPE *b_ = (TYPE *)(B);                                                                       \
    TYPE *out_ = (TYPE *)(OUT);                                                                   \
                                                                                                  \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                                              \
    {                                                                                             \
      WIDE_TYPE prod_ = (WIDE_TYPE)a_[(OF_A) + i] * (WIDE_TYPE)b_[(OF_B) + i];                    \
      out_[(OF_TGT) + i] = (TYPE)((UWIDE_TYPE)prod_ >> (BITS));                                   \
    }                                                                                             \
  } while (0)