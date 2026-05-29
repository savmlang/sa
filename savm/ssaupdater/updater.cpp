#include "llvm/Transforms/Utils/SSAUpdater.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

using namespace llvm;

extern "C"
{
  void *create_ssaupdater()
  {
    return new SSAUpdater();
  }

  void ssaupdater_init(void *updater, LLVMTypeRef type, const char *name)
  {
    auto ssaupdater = reinterpret_cast<SSAUpdater *>(updater);
    ssaupdater->Initialize(unwrap(type), name);
  }

  void ssaupdater_def(void *updater, LLVMBasicBlockRef basicblock, LLVMValueRef value)
  {
    auto ssaupdater = reinterpret_cast<SSAUpdater *>(updater);

    ssaupdater->AddAvailableValue(unwrap(basicblock), unwrap(value));
  }

  LLVMValueRef ssaupdater_get(void *updater, LLVMBasicBlockRef basicblock)
  {
    auto ssaupdater = reinterpret_cast<SSAUpdater *>(updater);

    return wrap(ssaupdater->GetValueInMiddleOfBlock(unwrap(basicblock)));
  }

  void ssaupdater_free(void *updater)
  {
    delete reinterpret_cast<SSAUpdater *>(updater);
  }
}