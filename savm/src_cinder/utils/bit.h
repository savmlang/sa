#pragma once
#include "arith.h"

FORCE_INLINE uint64_t bitrev64(uint64_t x) { return __builtin_bitreverse64(x); }
FORCE_INLINE uint32_t bitrev32(uint32_t x) { return __builtin_bitreverse32(x); }
FORCE_INLINE uint16_t bitrev16(uint16_t x) { return __builtin_bitreverse16(x); }
FORCE_INLINE uint8_t  bitrev8(uint8_t x)   { return __builtin_bitreverse8(x); }

FORCE_INLINE uint64_t bswap64_val(uint64_t x) { return __builtin_bswap64(x); }
FORCE_INLINE uint32_t bswap32_val(uint32_t x) { return __builtin_bswap32(x); }
FORCE_INLINE uint16_t bswap16_val(uint16_t x) { return __builtin_bswap16(x); }
FORCE_INLINE uint8_t  bswap8_val(uint8_t x)   { return x; }

#define BITOP_LOOP(TYPE, OP_EXPR, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT) \
  do                                                                     \
  {                                                                      \
    TYPE *a_ = (TYPE *)(A);                                              \
    TYPE *b_ = (TYPE *)(B);                                              \
    TYPE *out_ = (TYPE *)(OUT);                                          \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)                     \
    {                                                                    \
      TYPE a = a_[(OF_A) + i];                                           \
      TYPE b = b_[(OF_B) + i];                                           \
      out_[(OF_TGT) + i] = (OP_EXPR);                                    \
    }                                                                    \
  } while (0)
