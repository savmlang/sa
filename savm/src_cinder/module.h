#pragma once
#include <stdint.h>
#include <stdalign.h>

#if defined(_MSC_VER)
#define FORCE_INLINE static __forceinline
#elif defined(__GNUC__) || defined(__clang__)
#define FORCE_INLINE static inline __attribute__((always_inline))
#else
#define FORCE_INLINE static inline
#endif

#define DEFINE_SLICE(T, E)  \
  typedef struct ISlice_##E \
  {                         \
    T *ptr;                 \
    uintptr_t len;          \
  } Slice_##E;

// Flagss
#define FLAG_FIRST 0b000000000000000000000000000000001
#define FLAG_JUMP_TO_RESUME 0b000000000000000000000000000000010

// Opcodes
#define OPCODE_OK 0
#define OPCODE_JIT_CHECK 1

#define BECOME(x) [[clang::musttail]] return (x)

#define JITFN __attribute__((section(".jit_fn"), noinline))

typedef struct IVM
{
  alignas(64) uint64_t r1;
  uint64_t r2, r3, r4, r5, r6, r7, r8;

  // scratchpad, largepad, ame
  uint64_t _, _1, _2;

  uint32_t flags, opcodes;

  uint64_t curline_or_resume, engine;
  alignas(8) void *misc;

  uint64_t _padding;
} VMTaskState;

typedef struct IInst
{
  uint8_t opcode, u1, u2, u3;
} PickleInstruction;

typedef void (*SetWSArr)(void *ws, uint8_t *ptr, uintptr_t len);

typedef struct IDispatch
{
  /// @brief A hotness or resume counter
  uint64_t hotnessOrResume;

  /// Pointer to the WS structure
  void *ws;

  /// @brief Pointer to the current pickle instruction
  PickleInstruction *pickle;

  /// @brief VM Task State
  VMTaskState *taskstate;

  /// @brief  WorkingSet Array - C pointer marshalled to allow effective mutability
  SetWSArr wsarr;
} DispatchStarter;

typedef void (*JitFn)(DispatchStarter *dsp);

typedef struct IMarker
{
  uint64_t marker;
  uint64_t _internal;
  alignas(8) JitFn loc;
} Marker;

typedef void (*CRTFn)(PickleInstruction *pki, void *ws, VMTaskState *taskstate);

DEFINE_SLICE(Marker, MarkerList);

static_assert(sizeof(VMTaskState) == 128, "Size mismatch: Expected 128 bytes");
static_assert(alignof(VMTaskState) == 64, "Alignment mismatch: Expected 64-byte alignment");
