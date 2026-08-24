#pragma once
#include "arith.h"

FORCE_INLINE uint64_t rotl64(uint64_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 64 + 64) % 64);
  return amt == 0 ? x : (x << amt) | (x >> (64 - amt));
}
FORCE_INLINE uint64_t rotr64(uint64_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 64 + 64) % 64);
  return amt == 0 ? x : (x >> amt) | (x << (64 - amt));
}
FORCE_INLINE uint32_t rotl32(uint32_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 32 + 32) % 32);
  return amt == 0 ? x : (x << amt) | (x >> (32 - amt));
}
FORCE_INLINE uint32_t rotr32(uint32_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 32 + 32) % 32);
  return amt == 0 ? x : (x >> amt) | (x << (32 - amt));
}
FORCE_INLINE uint16_t rotl16(uint16_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 16 + 16) % 16);
  return amt == 0 ? x : (uint16_t)((x << amt) | (x >> (16 - amt)));
}
FORCE_INLINE uint16_t rotr16(uint16_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 16 + 16) % 16);
  return amt == 0 ? x : (uint16_t)((x >> amt) | (x << (16 - amt)));
}
FORCE_INLINE uint8_t rotl8(uint8_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 8 + 8) % 8);
  return amt == 0 ? x : (uint8_t)((x << amt) | (x >> (8 - amt)));
}
FORCE_INLINE uint8_t rotr8(uint8_t x, int64_t r) {
  uint32_t amt = (uint32_t)((r % 8 + 8) % 8);
  return amt == 0 ? x : (uint8_t)((x >> amt) | (x << (8 - amt)));
}

#define INTOP_ROTL(TYPE, ROTFN, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT) \
  do                                                                  \
  {                                                                   \
    TYPE *a_ = (TYPE *)(A);                                           \
    TYPE *b_ = (TYPE *)(B);                                           \
    TYPE *out_ = (TYPE *)(OUT);                                       \
    for (int i = 0; i < (int)(COUNT); i++)                            \
    {                                                                 \
      out_[(OF_TGT) + i] = (TYPE)ROTFN(a_[(OF_A) + i], (int64_t)b_[(OF_B) + i]); \
    }                                                                 \
  } while (0)

#define INTOP_ROTR(TYPE, ROTFN, A, B, OUT, COUNT, OF_A, OF_B, OF_TGT) \
  do                                                                  \
  {                                                                   \
    TYPE *a_ = (TYPE *)(A);                                           \
    TYPE *b_ = (TYPE *)(B);                                           \
    TYPE *out_ = (TYPE *)(OUT);                                       \
    for (int i = 0; i < (int)(COUNT); i++)                            \
    {                                                                 \
      out_[(OF_TGT) + i] = (TYPE)ROTFN(a_[(OF_A) + i], (int64_t)b_[(OF_B) + i]); \
    }                                                                 \
  } while (0)
