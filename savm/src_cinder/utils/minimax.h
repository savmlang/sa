#pragma once
#include "arith.h"

#define MIN_VAL(a, b) (((a) < (b)) ? (a) : (b))
#define MAX_VAL(a, b) (((a) > (b)) ? (a) : (b))

FORCE_INLINE double min_f64(double a, double b) { return __builtin_isnan(a) ? a : (__builtin_isnan(b) ? b : MIN_VAL(a, b)); }
FORCE_INLINE double max_f64(double a, double b) { return __builtin_isnan(a) ? a : (__builtin_isnan(b) ? b : MAX_VAL(a, b)); }
FORCE_INLINE float  min_f32(float a, float b)   { return __builtin_isnan(a) ? a : (__builtin_isnan(b) ? b : MIN_VAL(a, b)); }
FORCE_INLINE float  max_f32(float a, float b)   { return __builtin_isnan(a) ? a : (__builtin_isnan(b) ? b : MAX_VAL(a, b)); }

#define MINIMAX_LOOP(TYPE, EXPR, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT) \
  do                                                                    \
  {                                                                     \
    TYPE *a_ = (TYPE *)(A);                                             \
    TYPE *b_ = (TYPE *)(B);                                             \
    TYPE *out_ = (TYPE *)(OUT);                                         \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                    \
    {                                                                   \
      TYPE a = a_[(OF_A) + i];                                          \
      TYPE b = b_[(OF_B) + i];                                          \
      out_[(OF_TGT) + i] = (EXPR);                                      \
    }                                                                   \
  } while (0)
