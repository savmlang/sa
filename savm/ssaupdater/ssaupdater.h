#include <stdint.h>
#include <stddef.h>
#include "llvm-c/Types.h"
#include "llvm-c/Core.h"

#ifdef __cplusplus
extern "C"
{
#endif

  void *create_ssaupdater();
  void ssaupdater_init(void *updater, LLVMTypeRef type, const char *name);
  void ssaupdater_def(void *updater, LLVMBasicBlockRef basicblock, LLVMValueRef value);
  LLVMValueRef ssaupdater_get(void *updater, LLVMBasicBlockRef basicblock);
  void ssaupdater_free(void *updater);

#ifdef __cplusplus
}
#endif