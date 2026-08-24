#pragma once
#include "arith.h"
#include <stdbool.h>

FORCE_INLINE bool cmp_int(uint8_t op, uint64_t s1_u, uint64_t s2_u, int64_t s1_s, int64_t s2_s)
{
  switch (op)
  {
  case 0: return s1_u == s2_u;
  case 1: return s1_u != s2_u;
  case 2: return s1_s < s2_s;
  case 3: return s1_u < s2_u;
  case 4: return s1_s <= s2_s;
  case 5: return s1_u <= s2_u;
  case 6: return s1_s > s2_s;
  case 7: return s1_u > s2_u;
  case 8: return s1_s >= s2_s;
  case 9: return s1_u >= s2_u;
  default: return false;
  }
}

FORCE_INLINE bool cmp_f32(uint8_t op, float v1, float v2)
{
  bool un = __builtin_isnan(v1) || __builtin_isnan(v2);
  bool eq = v1 == v2;
  bool lt = v1 < v2;
  bool gt = v1 > v2;

  switch (op)
  {
  case 10: return eq || lt || gt;
  case 11: return un;
  case 12: return eq;
  case 13: return un || lt || gt;
  case 14: return lt || gt;
  case 15: return un || eq;
  case 16: return lt;
  case 17: return lt || eq;
  case 18: return gt;
  case 19: return gt || eq;
  case 20: return un || lt;
  case 21: return un || lt || eq;
  case 22: return un || gt;
  case 23: return un || gt || eq;
  default: return false;
  }
}

FORCE_INLINE bool cmp_f64(uint8_t op, double v1, double v2)
{
  bool un = __builtin_isnan(v1) || __builtin_isnan(v2);
  bool eq = v1 == v2;
  bool lt = v1 < v2;
  bool gt = v1 > v2;

  switch (op)
  {
  case 10: return eq || lt || gt;
  case 11: return un;
  case 12: return eq;
  case 13: return un || lt || gt;
  case 14: return lt || gt;
  case 15: return un || eq;
  case 16: return lt;
  case 17: return lt || eq;
  case 18: return gt;
  case 19: return gt || eq;
  case 20: return un || lt;
  case 21: return un || lt || eq;
  case 22: return un || gt;
  case 23: return un || gt || eq;
  default: return false;
  }
}
