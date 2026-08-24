#pragma once
#include "../module.h"
#include "resolver.h"

typedef struct JifPrelude
{
  uint8_t intent;
  uint8_t width;
  void *src;
  int32_t offset;
} JifPrelude;

#define JIFPRELUDEGEN()               \
  /* 3x8-bits: intent, src, width */  \
  extern char INTENT_SRC_WIDTH[];     \
  /* 32bits: offset */                \
  extern char OFFSET[];

FORCE_INLINE
JifPrelude parse_jifprelude(
    VMTaskState *task,
    uint64_t intent_src_width,
    uint64_t offset)
{
  JifPrelude out = {
      .intent = (uint8_t)(intent_src_width & 0xFF),
      .src = resolve_loc(task, (uint8_t)((intent_src_width & 0xFF00) >> 8)),
      .width = (uint8_t)((intent_src_width & 0xFF0000) >> 16),
      .offset = (int32_t)(offset),
  };

  return out;
}
