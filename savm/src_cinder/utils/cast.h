#pragma once
#include "../module.h"
#include "resolver.h"

typedef struct CastPrelude
{
  uint8_t tag_initial;
  uint8_t tag_final;
  void *src, *tgt;
  uint32_t count;
  int32_t of_src, of_tgt;
} CastPrelude;

#define CASTPRELUDEGEN()                           \
  /* 4x8-bits: tag_initial, tag_final, src, tgt */ \
  extern char TAGS_SRC_TGT[];                      \
  /* 32-bit count */                               \
  extern char COUNT[];                             \
  /* 2x32bits: of_src, of_tgt */                   \
  extern char OF_SRC_TGT[];

FORCE_INLINE
CastPrelude parse_castprelude(
    VMTaskState *task,
    uint64_t tags_src_tgt,
    uint64_t count,
    uint64_t of_src_tgt)
{
  CastPrelude out = {
      .tag_initial = (uint8_t)(tags_src_tgt & 0xFF),
      .tag_final = (uint8_t)((tags_src_tgt & 0xFF00) >> 8),
      .src = resolve_loc(task, (uint8_t)((tags_src_tgt & 0xFF0000) >> 16)),
      .tgt = resolve_loc(task, (uint8_t)((tags_src_tgt & 0xFF000000) >> 24)),
      .count = (uint32_t)count,
      .of_src = (int32_t)(of_src_tgt),
      .of_tgt = (int32_t)(of_src_tgt >> 32),
  };
  return out;
}

FORCE_INLINE double cast_u64_to_f64(uint64_t x)
{
  if ((int64_t)x >= 0)
    return (double)(int64_t)x;
  double d = (double)(int64_t)((x >> 1) | (x & 1));
  return d + d;
}

FORCE_INLINE float cast_u64_to_f32(uint64_t x)
{
  if ((int64_t)x >= 0)
    return (float)(int64_t)x;
  float f = (float)(int64_t)((x >> 1) | (x & 1));
  return f + f;
}

FORCE_INLINE uint64_t cast_f64_to_u64(double x)
{
  uint64_t bits = 0x43E0000000000000ULL;
  __asm__("" : "+r"(bits));
  double two63;
  __builtin_memcpy(&two63, &bits, sizeof(two63));
  if (x < two63)
    return (uint64_t)(int64_t)x;
  return ((uint64_t)(int64_t)(x - two63)) | (1ULL << 63);
}

FORCE_INLINE uint64_t cast_f32_to_u64(float x)
{
  uint32_t bits = 0x5F000000U;
  __asm__("" : "+r"(bits));
  float two63;
  __builtin_memcpy(&two63, &bits, sizeof(two63));
  if (x < two63)
    return (uint64_t)(int64_t)x;
  return ((uint64_t)(int64_t)(x - two63)) | (1ULL << 63);
}

#define DISPATCH_CAST_TO(SRC_TYPE, SRC_PTR, TGT_TAG, TGT_PTR, COUNT, OF_SRC, OF_TGT) \
  do                                                                                 \
  {                                                                                  \
    SRC_TYPE *s_ = (SRC_TYPE *)(SRC_PTR);                                            \
    switch (TGT_TAG)                                                                 \
    {                                                                                \
    case 0:                                                                          \
    {                                                                                \
      uint64_t *d_ = (uint64_t *)(TGT_PTR);                                          \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (uint64_t)s_[(OF_SRC) + i];                               \
      break;                                                                         \
    }                                                                                \
    case 1:                                                                          \
    {                                                                                \
      uint32_t *d_ = (uint32_t *)(TGT_PTR);                                          \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (uint32_t)s_[(OF_SRC) + i];                               \
      break;                                                                         \
    }                                                                                \
    case 2:                                                                          \
    {                                                                                \
      uint16_t *d_ = (uint16_t *)(TGT_PTR);                                          \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (uint16_t)s_[(OF_SRC) + i];                               \
      break;                                                                         \
    }                                                                                \
    case 3:                                                                          \
    {                                                                                \
      uint8_t *d_ = (uint8_t *)(TGT_PTR);                                            \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (uint8_t)s_[(OF_SRC) + i];                                \
      break;                                                                         \
    }                                                                                \
    case 4:                                                                          \
    {                                                                                \
      int64_t *d_ = (int64_t *)(TGT_PTR);                                            \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (int64_t)s_[(OF_SRC) + i];                                \
      break;                                                                         \
    }                                                                                \
    case 5:                                                                          \
    {                                                                                \
      int32_t *d_ = (int32_t *)(TGT_PTR);                                            \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (int32_t)s_[(OF_SRC) + i];                                \
      break;                                                                         \
    }                                                                                \
    case 6:                                                                          \
    {                                                                                \
      int16_t *d_ = (int16_t *)(TGT_PTR);                                            \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (int16_t)s_[(OF_SRC) + i];                                \
      break;                                                                         \
    }                                                                                \
    case 7:                                                                          \
    {                                                                                \
      int8_t *d_ = (int8_t *)(TGT_PTR);                                              \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (int8_t)s_[(OF_SRC) + i];                                 \
      break;                                                                         \
    }                                                                                \
    case 8:                                                                          \
    {                                                                                \
      double *d_ = (double *)(TGT_PTR);                                              \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (double)s_[(OF_SRC) + i];                                 \
      break;                                                                         \
    }                                                                                \
    case 9:                                                                          \
    {                                                                                \
      float *d_ = (float *)(TGT_PTR);                                                \
      for (uint32_t i = 0; i < (COUNT); i++)                                         \
        d_[(OF_TGT) + i] = (float)s_[(OF_SRC) + i];                                  \
      break;                                                                         \
    }                                                                                \
    default:                                                                         \
      break;                                                                         \
    }                                                                                \
  } while (0)

#define DISPATCH_CAST_U64(SRC_PTR, TGT_TAG, TGT_PTR, COUNT, OF_SRC, OF_TGT) \
  do                                                                          \
  {                                                                           \
    uint64_t *s_ = (uint64_t *)(SRC_PTR);                                    \
    switch (TGT_TAG)                                                          \
    {                                                                         \
    case 0:                                                                   \
    {                                                                         \
      uint64_t *d_ = (uint64_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint64_t)s_[(OF_SRC) + i];                        \
      break;                                                                  \
    }                                                                         \
    case 1:                                                                   \
    {                                                                         \
      uint32_t *d_ = (uint32_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint32_t)s_[(OF_SRC) + i];                        \
      break;                                                                  \
    }                                                                         \
    case 2:                                                                   \
    {                                                                         \
      uint16_t *d_ = (uint16_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint16_t)s_[(OF_SRC) + i];                        \
      break;                                                                  \
    }                                                                         \
    case 3:                                                                   \
    {                                                                         \
      uint8_t *d_ = (uint8_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint8_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 4:                                                                   \
    {                                                                         \
      int64_t *d_ = (int64_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int64_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 5:                                                                   \
    {                                                                         \
      int32_t *d_ = (int32_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int32_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 6:                                                                   \
    {                                                                         \
      int16_t *d_ = (int16_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int16_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 7:                                                                   \
    {                                                                         \
      int8_t *d_ = (int8_t *)(TGT_PTR);                                       \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int8_t)s_[(OF_SRC) + i];                          \
      break;                                                                  \
    }                                                                         \
    case 8:                                                                   \
    {                                                                         \
      double *d_ = (double *)(TGT_PTR);                                       \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = cast_u64_to_f64(s_[(OF_SRC) + i]);                 \
      break;                                                                  \
    }                                                                         \
    case 9:                                                                   \
    {                                                                         \
      float *d_ = (float *)(TGT_PTR);                                         \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = cast_u64_to_f32(s_[(OF_SRC) + i]);                 \
      break;                                                                  \
    }                                                                         \
    default:                                                                  \
      break;                                                                  \
    }                                                                         \
  } while (0)

#define DISPATCH_CAST_F64(SRC_PTR, TGT_TAG, TGT_PTR, COUNT, OF_SRC, OF_TGT) \
  do                                                                          \
  {                                                                           \
    double *s_ = (double *)(SRC_PTR);                                         \
    switch (TGT_TAG)                                                          \
    {                                                                         \
    case 0:                                                                   \
    {                                                                         \
      uint64_t *d_ = (uint64_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = cast_f64_to_u64(s_[(OF_SRC) + i]);                 \
      break;                                                                  \
    }                                                                         \
    case 1:                                                                   \
    {                                                                         \
      uint32_t *d_ = (uint32_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint32_t)s_[(OF_SRC) + i];                        \
      break;                                                                  \
    }                                                                         \
    case 2:                                                                   \
    {                                                                         \
      uint16_t *d_ = (uint16_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint16_t)s_[(OF_SRC) + i];                        \
      break;                                                                  \
    }                                                                         \
    case 3:                                                                   \
    {                                                                         \
      uint8_t *d_ = (uint8_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint8_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 4:                                                                   \
    {                                                                         \
      int64_t *d_ = (int64_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int64_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 5:                                                                   \
    {                                                                         \
      int32_t *d_ = (int32_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int32_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 6:                                                                   \
    {                                                                         \
      int16_t *d_ = (int16_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int16_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 7:                                                                   \
    {                                                                         \
      int8_t *d_ = (int8_t *)(TGT_PTR);                                       \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int8_t)s_[(OF_SRC) + i];                          \
      break;                                                                  \
    }                                                                         \
    case 8:                                                                   \
    {                                                                         \
      double *d_ = (double *)(TGT_PTR);                                       \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (double)s_[(OF_SRC) + i];                          \
      break;                                                                  \
    }                                                                         \
    case 9:                                                                   \
    {                                                                         \
      float *d_ = (float *)(TGT_PTR);                                         \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (float)s_[(OF_SRC) + i];                           \
      break;                                                                  \
    }                                                                         \
    default:                                                                  \
      break;                                                                  \
    }                                                                         \
  } while (0)

#define DISPATCH_CAST_F32(SRC_PTR, TGT_TAG, TGT_PTR, COUNT, OF_SRC, OF_TGT) \
  do                                                                          \
  {                                                                           \
    float *s_ = (float *)(SRC_PTR);                                           \
    switch (TGT_TAG)                                                          \
    {                                                                         \
    case 0:                                                                   \
    {                                                                         \
      uint64_t *d_ = (uint64_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = cast_f32_to_u64(s_[(OF_SRC) + i]);                 \
      break;                                                                  \
    }                                                                         \
    case 1:                                                                   \
    {                                                                         \
      uint32_t *d_ = (uint32_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint32_t)s_[(OF_SRC) + i];                        \
      break;                                                                  \
    }                                                                         \
    case 2:                                                                   \
    {                                                                         \
      uint16_t *d_ = (uint16_t *)(TGT_PTR);                                   \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint16_t)s_[(OF_SRC) + i];                        \
      break;                                                                  \
    }                                                                         \
    case 3:                                                                   \
    {                                                                         \
      uint8_t *d_ = (uint8_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (uint8_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 4:                                                                   \
    {                                                                         \
      int64_t *d_ = (int64_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int64_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 5:                                                                   \
    {                                                                         \
      int32_t *d_ = (int32_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int32_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 6:                                                                   \
    {                                                                         \
      int16_t *d_ = (int16_t *)(TGT_PTR);                                     \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int16_t)s_[(OF_SRC) + i];                         \
      break;                                                                  \
    }                                                                         \
    case 7:                                                                   \
    {                                                                         \
      int8_t *d_ = (int8_t *)(TGT_PTR);                                       \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (int8_t)s_[(OF_SRC) + i];                          \
      break;                                                                  \
    }                                                                         \
    case 8:                                                                   \
    {                                                                         \
      double *d_ = (double *)(TGT_PTR);                                       \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (double)s_[(OF_SRC) + i];                          \
      break;                                                                  \
    }                                                                         \
    case 9:                                                                   \
    {                                                                         \
      float *d_ = (float *)(TGT_PTR);                                         \
      for (uint32_t i = 0; i < (COUNT); i++)                                  \
        d_[(OF_TGT) + i] = (float)s_[(OF_SRC) + i];                           \
      break;                                                                  \
    }                                                                         \
    default:                                                                  \
      break;                                                                  \
    }                                                                         \
  } while (0)

FORCE_INLINE void execute_cast(uint8_t src_tag, uint8_t tgt_tag, void *src, void *tgt, uint32_t count, int32_t of_src, int32_t of_tgt)
{
  switch (src_tag)
  {
  case 0:
    DISPATCH_CAST_U64(src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 1:
    DISPATCH_CAST_TO(uint32_t, src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 2:
    DISPATCH_CAST_TO(uint16_t, src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 3:
    DISPATCH_CAST_TO(uint8_t, src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 4:
    DISPATCH_CAST_TO(int64_t, src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 5:
    DISPATCH_CAST_TO(int32_t, src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 6:
    DISPATCH_CAST_TO(int16_t, src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 7:
    DISPATCH_CAST_TO(int8_t, src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 8:
    DISPATCH_CAST_F64(src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  case 9:
    DISPATCH_CAST_F32(src, tgt_tag, tgt, count, of_src, of_tgt);
    break;
  default:
    break;
  }
}
