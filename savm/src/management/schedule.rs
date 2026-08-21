#[cfg(feature = "native")]
use crossbeam_channel::Sender;
#[cfg(feature = "native")]
use std::iter::Peekable;

/// Dispatches compilation tasks to critical, fastlane, and public compiler queues.
#[cfg(feature = "native")]
pub fn schedule<
  'a,
  'b,
  F: Fn() -> Peekable<I2>,
  E: Fn() -> Peekable<I3>,
  I1: Iterator<Item = &'a u64>,
  I2: Iterator<Item = &'b u64>,
  I3: Iterator<Item = u64>,
>(
  tx_critical: &Sender<(u64, usize, bool)>,
  tx_fastlane: &Sender<(u64, usize, bool)>,
  tx_public: &Sender<(u64, usize, bool)>,
  critical: &mut Peekable<I1>,
  important: &mut Peekable<I2>,
  others: &mut Peekable<I3>,
  compiler_fastlane: &mut usize,
  compiler_public: &mut usize,
  compilers_len: usize,
  important_s: F,
  others_iter: E,
) {
  /*
    Schedule more work through each sector
  */

  'critical_loop: while let Some(x) = critical.peek() {
    // Note: **x implies x is a reference to a reference/pointer
    if tx_critical
      .try_send((**x, compilers_len - 1, false))
      .is_ok()
    {
      _ = critical.next();
    } else {
      break 'critical_loop;
    }
  }

  'important_loop: while let Some(x) = important.peek() {
    if tx_fastlane
      .try_send((**x, *compiler_fastlane, false))
      .is_ok()
    {
      _ = important.next();
    } else {
      break 'important_loop;
    }
  }

  'others_loop: while let Some(x) = others.peek() {
    if tx_public.try_send((*x, *compiler_public, false)).is_ok() {
      _ = others.next();
    } else {
      break 'others_loop;
    }
  }

  /*
    Sanity Checking
  */

  if critical.peek().is_none() {
    _ = tx_critical.try_send((0, 0, true));
  }

  if important.peek().is_none() {
    if *compiler_fastlane + 1 == compilers_len {
      _ = tx_fastlane.try_send((0, 0, true));
    } else {
      *compiler_fastlane += 1;
      *important = important_s();
    }
  }

  if others.peek().is_none() {
    if *compiler_public + 1 == compilers_len {
      _ = tx_public.try_send((0, 0, true));
    } else {
      *compiler_public += 1;
      *others = others_iter();
    }
  }
}
