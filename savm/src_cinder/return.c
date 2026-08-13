#include "module.h"

/// @brief The return instruction of the copy-patch JIT.
/// @brief This can be used to terminate the set of calls done earlier
JITFN
void cinderjit_return(DispatchStarter *state)
{
  state->taskstate->opcodes = OPCODE_OK;
  state->taskstate->flags &= ~FLAG_JUMP_TO_RESUME;
  return;
}