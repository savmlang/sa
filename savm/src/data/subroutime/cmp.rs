pub fn execute_cmp(ctx: &mut VMContext, task: &mut VMTaskState) {
  let a = unsafe { task.r1.unsigned };
  let b = unsafe { task.r2.unsigned };

    // ONE subtraction. The 'carry' boolean here is essentially the Carry Flag.
  let (res, carry) = a.overflowing_sub(b);

    // Reset flags
  ctx.flags &= FLAG_ASYNC;

    // 1. Zero Flag: Was the result 0?
  if res == 0 { ctx.flags |= FLAG_ZERO; }

    // 2. Sign Flag: Is the result's high bit 1?
  if (res >> 63) == 1 { ctx.flags |= FLAG_SIGN; }

    // 3. Carry Flag: Did we have to "borrow" (unsigned)?
  if carry { ctx.flags |= FLAG_CARRY; }

    // 4. Overflow Flag: Did the sign flip unexpectedly (signed)?
    // This bitwise check looks at the signs of a, b, and the result.
  if ((a ^ b) >> 63 != 0) && ((res ^ a) >> 63 != 0) {
    ctx.flags |= FLAG_OFLOW;
  }
}