#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm-c/Core.h"

#include <iterator>

using namespace llvm;

extern "C"
{
  uint64_t llvm_get_num_preds(
      LLVMBasicBlockRef BasicBlock)
  {
    llvm::BasicBlock *cpp_bb = unwrap(BasicBlock);

    auto preds = llvm::predecessors(cpp_bb);
    return std::distance(preds.begin(), preds.end());
  }

  void llvm_get_preds(
      LLVMBasicBlockRef BasicBlock,
      LLVMBasicBlockRef *Arr)
  {
    if (!BasicBlock || !Arr)
      return;

    llvm::BasicBlock *cpp_bb = unwrap(BasicBlock);

    uint64_t idx = 0;

    for (auto preds : llvm::predecessors(cpp_bb))
    {
      Arr[idx] = wrap(preds);
      idx += 1;
    }
  }
}