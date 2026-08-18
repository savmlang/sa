#include "module.h"

extern Marker MARKER_FIRST;
extern char MARKERS_TOTAL[];

extern void NEXT(DispatchStarter *dsp);

FORCE_INLINE JitFn get_fidx(Slice_MarkerList slicedata, uint64_t marker);

/// @brief Setup initial SaVMJIT
JITFN
void cinderjit_prelude(DispatchStarter *state)
{
  VMTaskState *tsk = state->taskstate;

  if ((tsk->flags & FLAG_JUMP_TO_RESUME) > 0)
  {
    Slice_MarkerList markers = {
        .ptr = &MARKER_FIRST,
        .len = (uintptr_t)MARKERS_TOTAL,
    };

    JitFn f = get_fidx(markers, tsk->curline_or_resume);
    BECOME(f(state));
  }

  BECOME(NEXT(state));
}

FORCE_INLINE JitFn get_fidx(Slice_MarkerList slicedata, uint64_t marker)
{
  if (slicedata.len == 0)
  {
    return NULL;
  }

  uintptr_t low = 0;
  uintptr_t high = slicedata.len;
  Marker *pointer = slicedata.ptr;

  while (low < high)
  {
    uintptr_t mid = low + (high - low) / 2;
    Marker entry = pointer[mid];

    if (entry.marker == marker)
      return entry.loc;

    if (entry.marker < marker)
      low = mid + 1;
    else
      high = mid;
  }

  return NULL;
}