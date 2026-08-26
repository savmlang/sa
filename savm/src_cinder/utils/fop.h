#pragma once
#include "arith.h"

FORCE_INLINE double fop64_trunc(double x)
{
  uint64_t u;
  __builtin_memcpy(&u, &x, sizeof(u));
  uint32_t exp = (uint32_t)((u >> 52) & 0x7FF);
  if (exp < 1075) // 1023 + 52
  {
    uint64_t sign_mask = 1ULL << 63;
    __asm__("" : "+r"(sign_mask));
    uint64_t abs_mask = ~sign_mask;
    __asm__("" : "+r"(abs_mask));

    uint64_t sign = u & sign_mask;
    int64_t i = (int64_t)x;
    double res = (double)i;
    uint64_t res_u;
    __builtin_memcpy(&res_u, &res, sizeof(res_u));
    res_u = (res_u & abs_mask) | sign;
    __builtin_memcpy(&res, &res_u, sizeof(res));
    return res;
  }
  return x;
}

FORCE_INLINE double fop64_floor(double x)
{
  double res = fop64_trunc(x);
  if (x < 0.0 && res != x)
  {
    uint64_t one_bits = 0x3FF0000000000000ULL;
    __asm__("" : "+r"(one_bits));
    double one;
    __builtin_memcpy(&one, &one_bits, sizeof(one));
    res -= one;
  }
  return res;
}

FORCE_INLINE double fop64_ceil(double x)
{
  double res = fop64_trunc(x);
  if (x > 0.0 && res != x)
  {
    uint64_t one_bits = 0x3FF0000000000000ULL;
    __asm__("" : "+r"(one_bits));
    double one;
    __builtin_memcpy(&one, &one_bits, sizeof(one));
    res += one;
  }
  return res;
}

FORCE_INLINE double fop64_round(double x)
{
  uint64_t half_bits = 0x3FE0000000000000ULL;
  __asm__("" : "+r"(half_bits));
  double half;
  __builtin_memcpy(&half, &half_bits, sizeof(half));
  if (x >= 0.0)
    return fop64_trunc(x + half);
  else
    return fop64_trunc(x - half);
}

FORCE_INLINE double fop64_sqrt(double x)
{
  double res;
  __asm__("sqrtsd %1, %0" : "=x"(res) : "x"(x));
  return res;
}

FORCE_INLINE double fop64(uint8_t subop, double x)
{
  switch (subop)
  {
  case 0: return fop64_ceil(x);
  case 1: return fop64_floor(x);
  case 2: return fop64_trunc(x);
  case 3: return fop64_round(x);
  case 4: return fop64_sqrt(x);
  default: return x;
  }
}

FORCE_INLINE float fop32_trunc(float x)
{
  uint32_t u;
  __builtin_memcpy(&u, &x, sizeof(u));
  uint32_t exp = (u >> 23) & 0xFF;
  if (exp < 150) // 127 + 23
  {
    uint32_t sign_mask = 1U << 31;
    __asm__("" : "+r"(sign_mask));
    uint32_t abs_mask = ~sign_mask;
    __asm__("" : "+r"(abs_mask));

    uint32_t sign = u & sign_mask;
    int32_t i = (int32_t)x;
    float res = (float)i;
    uint32_t res_u;
    __builtin_memcpy(&res_u, &res, sizeof(res_u));
    res_u = (res_u & abs_mask) | sign;
    __builtin_memcpy(&res, &res_u, sizeof(res));
    return res;
  }
  return x;
}

FORCE_INLINE float fop32_floor(float x)
{
  float res = fop32_trunc(x);
  if (x < 0.0f && res != x)
  {
    uint32_t one_bits = 0x3F800000U;
    __asm__("" : "+r"(one_bits));
    float one;
    __builtin_memcpy(&one, &one_bits, sizeof(one));
    res -= one;
  }
  return res;
}

FORCE_INLINE float fop32_ceil(float x)
{
  float res = fop32_trunc(x);
  if (x > 0.0f && res != x)
  {
    uint32_t one_bits = 0x3F800000U;
    __asm__("" : "+r"(one_bits));
    float one;
    __builtin_memcpy(&one, &one_bits, sizeof(one));
    res += one;
  }
  return res;
}

FORCE_INLINE float fop32_round(float x)
{
  uint32_t half_bits = 0x3F000000U;
  __asm__("" : "+r"(half_bits));
  float half;
  __builtin_memcpy(&half, &half_bits, sizeof(half));
  if (x >= 0.0f)
    return fop32_trunc(x + half);
  else
    return fop32_trunc(x - half);
}

FORCE_INLINE float fop32_sqrt(float x)
{
  float res;
  __asm__("sqrtss %1, %0" : "=x"(res) : "x"(x));
  return res;
}

FORCE_INLINE float fop32(uint8_t subop, float x)
{
  switch (subop)
  {
  case 0: return fop32_ceil(x);
  case 1: return fop32_floor(x);
  case 2: return fop32_trunc(x);
  case 3: return fop32_round(x);
  case 4: return fop32_sqrt(x);
  default: return x;
  }
}
