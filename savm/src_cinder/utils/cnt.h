#pragma once
#include "arith.h"

FORCE_INLINE uint64_t popcnt64(uint64_t x) { return (uint64_t)__builtin_popcountll(x); }
FORCE_INLINE uint32_t popcnt32(uint32_t x) { return (uint32_t)__builtin_popcount(x); }
FORCE_INLINE uint16_t popcnt16(uint16_t x) { return (uint16_t)__builtin_popcount((uint32_t)x); }
FORCE_INLINE uint8_t  popcnt8(uint8_t x)   { return (uint8_t)__builtin_popcount((uint32_t)x); }

FORCE_INLINE uint64_t clz64(uint64_t x) { return x == 0 ? 64 : (uint64_t)__builtin_clzll(x); }
FORCE_INLINE uint32_t clz32(uint32_t x) { return x == 0 ? 32 : (uint32_t)__builtin_clz(x); }
FORCE_INLINE uint16_t clz16(uint16_t x) { return x == 0 ? 16 : (uint16_t)(__builtin_clz((uint32_t)x << 16)); }
FORCE_INLINE uint8_t  clz8(uint8_t x)   { return x == 0 ? 8  : (uint8_t)(__builtin_clz((uint32_t)x << 24)); }

FORCE_INLINE uint64_t cls64(uint64_t x) { return clz64(~x); }
FORCE_INLINE uint32_t cls32(uint32_t x) { return clz32(~x); }
FORCE_INLINE uint16_t cls16(uint16_t x) { return clz16((uint16_t)~x); }
FORCE_INLINE uint8_t  cls8(uint8_t x)   { return clz8((uint8_t)~x); }

FORCE_INLINE uint64_t ctz64(uint64_t x) { return x == 0 ? 64 : (uint64_t)__builtin_ctzll(x); }
FORCE_INLINE uint32_t ctz32(uint32_t x) { return x == 0 ? 32 : (uint32_t)__builtin_ctz(x); }
FORCE_INLINE uint16_t ctz16(uint16_t x) { return x == 0 ? 16 : (uint16_t)__builtin_ctz((uint32_t)x); }
FORCE_INLINE uint8_t  ctz8(uint8_t x)   { return x == 0 ? 8  : (uint8_t)__builtin_ctz((uint32_t)x); }

#define CNTOP_LOOP(TYPE, CNT_FN, A, OUT, COUNT, OF_A, OF_TGT) \
  do                                                           \
  {                                                            \
    TYPE *a_ = (TYPE *)(A);                                    \
    TYPE *out_ = (TYPE *)(OUT);                                \
    for (uint32_t i = 0; i < (uint32_t)(COUNT); i++)           \
    {                                                          \
      out_[(OF_TGT) + i] = CNT_FN(a_[(OF_A) + i]);             \
    }                                                          \
  } while (0)
