#pragma once
#include "arith.h"

#define INTOP_ADD_CARRY_U(TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT, CARRY) \
  do                                                                         \
  {                                                                          \
    TYPE *a_ = (TYPE *)(A);                                                  \
    TYPE *b_ = (TYPE *)(B);                                                  \
    TYPE *out_ = (TYPE *)(OUT);                                              \
    bool carry_ = (CARRY);                                                   \
                                                                             \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                         \
    {                                                                        \
      TYPE r_;                                                               \
      bool c1_ = __builtin_add_overflow(                                     \
          a_[(OF_A) + i], b_[(OF_B) + i], &r_);                              \
      bool c2_ = __builtin_add_overflow(                                     \
          r_, (TYPE)carry_, &r_);                                            \
                                                                             \
      out_[(OF_TGT) + i] = r_;                                               \
      carry_ = c1_ || c2_;                                                   \
    }                                                                        \
                                                                             \
    (CARRY) = carry_;                                                        \
  } while (0)

#define INTOP_ADD_CARRY_S(TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT, CARRY) \
  do                                                                         \
  {                                                                          \
    TYPE *a_ = (TYPE *)(A);                                                  \
    TYPE *b_ = (TYPE *)(B);                                                  \
    TYPE *out_ = (TYPE *)(OUT);                                              \
    bool carry_ = (CARRY);                                                   \
                                                                             \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                         \
    {                                                                        \
      TYPE r_;                                                               \
      bool o1_ = __builtin_add_overflow(                                     \
          a_[(OF_A) + i], b_[(OF_B) + i], &r_);                              \
      bool o2_ = __builtin_add_overflow(                                     \
          r_, (TYPE)carry_, &r_);                                            \
                                                                             \
      out_[(OF_TGT) + i] = r_;                                               \
      carry_ = o1_ != o2_;                                                   \
    }                                                                        \
                                                                             \
    (CARRY) = carry_;                                                        \
  } while (0)

#define INTOP_ADD_SAT_U(TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT) \
  do                                                                \
  {                                                                 \
    TYPE *a_ = (TYPE *)(A);                                         \
    TYPE *b_ = (TYPE *)(B);                                         \
    TYPE *out_ = (TYPE *)(OUT);                                     \
    TYPE max_ = (TYPE) - 1;                                         \
                                                                    \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                \
    {                                                               \
      TYPE r_;                                                      \
      if (__builtin_add_overflow(                                   \
              a_[(OF_A) + i], b_[(OF_B) + i], &r_))                 \
        r_ = max_;                                                  \
                                                                    \
      out_[(OF_TGT) + i] = r_;                                      \
    }                                                               \
  } while (0)

#define INTOP_ADD_SAT_S(TYPE, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT) \
  do                                                                \
  {                                                                 \
    TYPE *a_ = (TYPE *)(A);                                         \
    TYPE *b_ = (TYPE *)(B);                                         \
    TYPE *out_ = (TYPE *)(OUT);                                     \
                                                                    \
    TYPE max_ = (TYPE)((((TYPE) - 1) >> 1));                        \
    TYPE min_ = (TYPE)(~max_);                                      \
                                                                    \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                \
    {                                                               \
      TYPE a_val_ = a_[(OF_A) + i];                                 \
      TYPE b_val_ = b_[(OF_B) + i];                                 \
      TYPE r_;                                                      \
                                                                    \
      if (__builtin_add_overflow(a_val_, b_val_, &r_))              \
      {                                                             \
        r_ = (a_val_ < 0) ? min_ : max_;                            \
      }                                                             \
                                                                    \
      out_[(OF_TGT) + i] = r_;                                      \
    }                                                               \
  } while (0)