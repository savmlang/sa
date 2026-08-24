use cranelift::codegen::ir::{Type, Value, condcodes::IntCC};
use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::InstBuilder;

pub struct IntArith<'a, 'b> {
  pub builder: &'a mut FunctionBuilder<'b>,
}

impl<'a, 'b> IntArith<'a, 'b> {
  #[inline]
  fn zero(&mut self, ty: Type) -> Value {
    self.builder.ins().iconst(ty, 0)
  }

  #[inline]
  fn one(&mut self, ty: Type) -> Value {
    self.builder.ins().iconst(ty, 1)
  }

  #[inline]
  fn flag_to_int(&mut self, flag: Value, ty: Type) -> Value {
    let zero = self.zero(ty);
    let one = self.one(ty);

    self.builder.ins().select(flag, one, zero)
  }

  /// Unsigned addition.
  #[inline]
  pub fn uadd_overflow(&mut self, a: Value, b: Value) -> (Value, Value) {
    let result = self.builder.ins().iadd(a, b);

    let carry = self.builder.ins().icmp(IntCC::UnsignedLessThan, result, a);

    (result, carry)
  }

  /// Unsigned subtraction.
  /// Returns `(result, borrow_out)`.
  #[inline]
  pub fn usub_overflow(&mut self, a: Value, b: Value) -> (Value, Value) {
    let result = self.builder.ins().isub(a, b);

    let borrow = self.builder.ins().icmp(IntCC::UnsignedLessThan, a, b);

    (result, borrow)
  }

  /// Signed addition.
  /// Returns `(result, overflow)`.
  #[inline]
  pub fn sadd_overflow(&mut self, a: Value, b: Value) -> (Value, Value) {
    let ty = self.builder.func.dfg.value_type(a);
    let zero = self.zero(ty);

    let result = self.builder.ins().iadd(a, b);

    let a_neg = self.builder.ins().icmp(IntCC::SignedLessThan, a, zero);

    let b_neg = self.builder.ins().icmp(IntCC::SignedLessThan, b, zero);

    let result_neg = self.builder.ins().icmp(IntCC::SignedLessThan, result, zero);

    // a and b have the same sign.
    let same_sign = self.builder.ins().icmp(IntCC::Equal, a_neg, b_neg);

    // Result changed sign relative to a.
    let changed_sign = self.builder.ins().icmp(IntCC::NotEqual, result_neg, a_neg);

    let overflow = self.builder.ins().band(same_sign, changed_sign);

    (result, overflow)
  }

  /// Signed subtraction.
  /// Returns `(result, overflow)`.
  #[inline]
  pub fn ssub_overflow(&mut self, a: Value, b: Value) -> (Value, Value) {
    let ty = self.builder.func.dfg.value_type(a);
    let zero = self.zero(ty);

    let result = self.builder.ins().isub(a, b);

    let a_neg = self.builder.ins().icmp(IntCC::SignedLessThan, a, zero);

    let b_neg = self.builder.ins().icmp(IntCC::SignedLessThan, b, zero);

    let result_neg = self.builder.ins().icmp(IntCC::SignedLessThan, result, zero);

    // a and b have different signs.
    let different_sign = self.builder.ins().icmp(IntCC::NotEqual, a_neg, b_neg);

    // Result changed sign relative to a.
    let changed_sign = self.builder.ins().icmp(IntCC::NotEqual, result_neg, a_neg);

    let overflow = self.builder.ins().band(different_sign, changed_sign);

    (result, overflow)
  }

  /// Unsigned addition with carry-in.
  /// Computes:
  ///     a + b + carry
  /// Returns `(result, carry_out)`.
  #[inline]
  pub fn uadd_carry(&mut self, a: Value, b: Value, carry: Value) -> (Value, Value) {
    let ty = self.builder.func.dfg.value_type(a);
    let carry = self.flag_to_int(carry, ty);

    let (sum, carry1) = self.uadd_overflow(a, b);
    let (result, carry2) = self.uadd_overflow(sum, carry);

    let carry_out = self.builder.ins().bor(carry1, carry2);

    (result, carry_out)
  }

  /// Signed addition with carry-in.
  /// Computes:
  ///     a + b + carry
  #[inline]
  pub fn sadd_carry(&mut self, a: Value, b: Value, carry: Value) -> (Value, Value) {
    let ty = self.builder.func.dfg.value_type(a);
    let carry = self.flag_to_int(carry, ty);

    let (sum, overflow1) = self.sadd_overflow(a, b);
    let (result, overflow2) = self.sadd_overflow(sum, carry);

    let overflow = self.builder.ins().bxor(overflow1, overflow2);

    (result, overflow)
  }

  /// Unsigned subtraction with borrow-in.
  /// Computes:
  ///     a - b - borrow
  /// Returns `(result, borrow_out)`.
  #[inline]
  pub fn usub_overflow_bin(&mut self, a: Value, b: Value, borrow: Value) -> (Value, Value) {
    let ty = self.builder.func.dfg.value_type(a);
    let borrow = self.flag_to_int(borrow, ty);

    let (diff, borrow1) = self.usub_overflow(a, b);
    let (result, borrow2) = self.usub_overflow(diff, borrow);

    let borrow_out = self.builder.ins().bor(borrow1, borrow2);

    (result, borrow_out)
  }

  /// Signed subtraction with borrow-in.
  /// Computes:
  ///     a - b - borrow
  #[inline]
  pub fn ssub_overflow_bin(&mut self, a: Value, b: Value, borrow: Value) -> (Value, Value) {
    let ty = self.builder.func.dfg.value_type(a);
    let borrow = self.flag_to_int(borrow, ty);

    let (diff, overflow1) = self.ssub_overflow(a, b);
    let (result, overflow2) = self.ssub_overflow(diff, borrow);

    let overflow = self.builder.ins().bxor(overflow1, overflow2);

    (result, overflow)
  }
}
