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

#define CASTPRELUDEGEN()                      \
  /* 4x8-bits: tag_initial, tag_final, src, tgt */ \
  extern char TAGS_SRC_TGT[];                 \
  /* 32-bit count */                          \
  extern char COUNT[];                        \
  /* 2x32bits: of_src, of_tgt */              \
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

#define DISPATCH_CAST_TO(SRC_TYPE, SRC_PTR, TGT_TAG, TGT_PTR, COUNT, OF_SRC, OF_TGT) \
  do {                                                                                \
    SRC_TYPE *s_ = (SRC_TYPE *)(SRC_PTR);                                             \
    switch (TGT_TAG) {                                                                \
    case 0: { uint64_t *d_ = (uint64_t *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (uint64_t)s_[(OF_SRC) + i]; break; } \
    case 1: { uint32_t *d_ = (uint32_t *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (uint32_t)s_[(OF_SRC) + i]; break; } \
    case 2: { uint16_t *d_ = (uint16_t *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (uint16_t)s_[(OF_SRC) + i]; break; } \
    case 3: { uint8_t  *d_ = (uint8_t  *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (uint8_t) s_[(OF_SRC) + i]; break; } \
    case 4: { int64_t  *d_ = (int64_t  *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (int64_t) s_[(OF_SRC) + i]; break; } \
    case 5: { int32_t  *d_ = (int32_t  *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (int32_t) s_[(OF_SRC) + i]; break; } \
    case 6: { int16_t  *d_ = (int16_t  *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (int16_t) s_[(OF_SRC) + i]; break; } \
    case 7: { int8_t   *d_ = (int8_t   *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (int8_t)  s_[(OF_SRC) + i]; break; } \
    case 8: { double   *d_ = (double   *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (double)  s_[(OF_SRC) + i]; break; } \
    case 9: { float    *d_ = (float    *)(TGT_PTR); for (uint32_t i = 0; i < (COUNT); i++) d_[(OF_TGT) + i] = (float)   s_[(OF_SRC) + i]; break; } \
    default: break;                                                                   \
    }                                                                                 \
  } while (0)

FORCE_INLINE void execute_cast(uint8_t src_tag, uint8_t tgt_tag, void *src, void *tgt, uint32_t count, int32_t of_src, int32_t of_tgt)
{
  switch (src_tag)
  {
  case 0: DISPATCH_CAST_TO(uint64_t, src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 1: DISPATCH_CAST_TO(uint32_t, src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 2: DISPATCH_CAST_TO(uint16_t, src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 3: DISPATCH_CAST_TO(uint8_t,  src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 4: DISPATCH_CAST_TO(int64_t,  src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 5: DISPATCH_CAST_TO(int32_t,  src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 6: DISPATCH_CAST_TO(int16_t,  src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 7: DISPATCH_CAST_TO(int8_t,   src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 8: DISPATCH_CAST_TO(double,   src, tgt_tag, tgt, count, of_src, of_tgt); break;
  case 9: DISPATCH_CAST_TO(float,    src, tgt_tag, tgt, count, of_src, of_tgt); break;
  default: break;
  }
}
