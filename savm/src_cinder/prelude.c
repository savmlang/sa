#include "module.h"

extern Slice_MarkerList *MARKERS;
extern JitFn NEXT;

JitFn get_fidx(Slice_MarkerList *slicedata, uint64_t marker);

/// @brief Setup initial SaVMJIT
JITFN
void cinderjit_prelude(DispatchStarter *state)
{
  VMTaskState tsk = *state->taskstate;

  if (tsk.flags == FLAG_JUMP_TO_RESUME)
  {
    JitFn f = get_fidx(MARKERS, tsk.curline_or_resume);
    BECOME(f(state));
  }

  BECOME(NEXT(state));
}

FORCE_INLINE JitFn get_fidx(Slice_MarkerList *slicedata, uint64_t marker)
{
  if (!slicedata || slicedata->len == 0)
  {
    goto abort;
  }

  uintptr_t low = 0;
  uintptr_t high = slicedata->len - 1;

  Marker *pointer = slicedata->ptr;

  while (low <= high)
  {
    uintptr_t mid = low + (high - low) / 2;

    Marker entry = pointer[mid];
    uint64_t value = entry.marker;

    // If mid is the value
    if (value == marker)
      return entry.loc;

    if (mid == 0)
      break;

    // Go the high side
    if (value < marker)
      low = mid + 1;

    if (value > marker)
      high = mid - 1;
  }

abort:
  // Natural trap
  return NULL;
}