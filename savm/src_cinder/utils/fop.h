#pragma once
#include "arith.h"

FORCE_INLINE double fop64(uint8_t subop, double x)
{
  switch (subop)
  {
  case 0: return __builtin_ceil(x);
  case 1: return __builtin_floor(x);
  case 2: return __builtin_trunc(x);
  case 3: return __builtin_round(x);
  case 4: return __builtin_sqrt(x);
  default: return x;
  }
}

FORCE_INLINE float fop32(uint8_t subop, float x)
{
  switch (subop)
  {
  case 0: return __builtin_ceilf(x);
  case 1: return __builtin_floorf(x);
  case 2: return __builtin_truncf(x);
  case 3: return __builtin_roundf(x);
  case 4: return __builtin_sqrtf(x);
  default: return x;
  }
}
