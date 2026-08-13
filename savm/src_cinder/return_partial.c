#include "module.h"

extern uint64_t RESUME;

/// @brief The return instruction of the copy-patch JIT.
/// @brief This
/// @brief This can be used to terminate the set of calls done earlier
JITFN
void cinderjit_return_partial(DispatchStarter *state)
{
  state->taskstate->curline_or_resume = RESUME;
  state->taskstate->flags |= FLAG_JUMP_TO_RESUME;
  state->taskstate->opcodes = OPCODE_JIT_CHECK;

  return;
}