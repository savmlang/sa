use std::{num::NonZeroU64, process::abort};

use rand::{TryRng, rngs::SysRng};

pub mod range;

#[derive(Clone, Debug)]
pub struct HashedPermutation {
  pub seed: u64,
  pub length: NonZeroU64,
}

impl HashedPermutation {
  pub const fn new_with_seed(length: NonZeroU64, seed: u64) -> Self {
    HashedPermutation { length, seed }
  }

  pub fn new_panicking(length: NonZeroU64) -> Self {
    let Ok(seed) = SysRng.try_next_u64() else {
      println!("RNG Error : Unsupported/Unexpected");
      abort();
    };
    HashedPermutation { length, seed }
  }

  pub fn new(length: NonZeroU64) -> Result<Self, rand::rngs::SysError> {
    let seed = SysRng.try_next_u64()?;
    Ok(HashedPermutation { length, seed })
  }

  pub const fn shuffle(&self, input_index: u64) -> Option<u64> {
    let max_length = self.length.get();

    if input_index >= max_length {
      return None;
    }

    let mut current_index = input_index;
    let seed = self.seed;

    // Create a bitmask for the next highest power of two minus one.
    // This bounds our scrambled numbers within a predictable bitwise range.
    let mut bitmask = max_length - 1;
    bitmask |= bitmask >> 1;
    bitmask |= bitmask >> 2;
    bitmask |= bitmask >> 4;
    bitmask |= bitmask >> 8;
    bitmask |= bitmask >> 16;
    bitmask |= bitmask >> 32;

    // Cycle-Walking Loop: If the scrambled result falls outside our
    // original `max_length` range, run the block again until it lands inside.
    loop {
      current_index ^= seed;

      current_index = current_index.wrapping_mul(0xd6e8feb86659fd93);
      current_index ^= seed >> 16;
      current_index ^= (current_index & bitmask) >> 4;
      current_index ^= seed >> 8;
      current_index = current_index.wrapping_mul(0x7c79e5af0654199d);
      current_index ^= seed >> 23;
      current_index ^= (current_index & bitmask) >> 1;
      current_index = current_index.wrapping_mul(1 | seed >> 27);
      current_index = current_index.wrapping_mul(0x5c4e40e7a57a55c5);
      current_index ^= (current_index & bitmask) >> 11;
      current_index = current_index.wrapping_mul(0x27cf5c4d32f5d0b5);
      current_index ^= (current_index & bitmask) >> 2;
      current_index = current_index.wrapping_mul(0x9e3779b97f4a7c15);
      current_index ^= (current_index & bitmask) >> 2;
      current_index = current_index.wrapping_mul(0xc6a4a7935bd1e995);
      current_index &= bitmask;
      current_index ^= current_index >> 5;

      if current_index < max_length {
        break;
      }
    }

    Some((current_index.wrapping_add(seed)) % max_length)
  }
}

pub struct ShuffledSliceIter<'a, T> {
  slice: &'a [T],
  permutation: HashedPermutation,
  current_step: u64,
}

impl<'a, T> ShuffledSliceIter<'a, T> {
  pub fn new(slice: &'a [T], seed: u64) -> Self {
    let length = NonZeroU64::new(slice.len() as u64).unwrap_or(NonZeroU64::new(1).unwrap());

    Self {
      slice,
      permutation: HashedPermutation::new_with_seed(length, seed),
      current_step: 0,
    }
  }

  pub fn new_panicking(slice: &'a [T]) -> Self {
    let length = NonZeroU64::new(slice.len() as u64).unwrap_or_else(|| NonZeroU64::new(1).unwrap());

    Self {
      slice,
      permutation: HashedPermutation::new_panicking(length),
      current_step: 0,
    }
  }

  pub fn get(&self, idx: usize) -> Option<&T> {
    let random_index = self.permutation.shuffle(idx as _)?;

    self.slice.get(random_index as usize)
  }
}

impl<'a, T> Iterator for ShuffledSliceIter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    let random_index = self.permutation.shuffle(self.current_step)?;

    self.current_step += 1;

    self.slice.get(random_index as usize)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::HashSet;

  #[test]
  fn test_no_repeats() {
    let total_items = 16_777_213;
    let length = NonZeroU64::new(total_items).unwrap();
    let perm = HashedPermutation::new_with_seed(length, 8675309);

    let mut seen = HashSet::new();

    for i in 0..total_items {
      let shuffled_val = perm.shuffle(i).expect("Should yield a value");

      // HashSet::insert returns false if the value already existed in the set
      let is_unique = seen.insert(shuffled_val);
      assert!(
        is_unique,
        "Duplicate detected! Value {} was repeated.",
        shuffled_val
      );
    }
  }

  #[test]
  fn test_all_items_produced() {
    let total_items = 16_777_213;
    let length = NonZeroU64::new(total_items).unwrap();
    let perm = HashedPermutation::new_with_seed(length, 123456789);

    let mut seen = HashSet::new();

    for i in 0..total_items {
      let shuffled_val = perm.shuffle(i).expect("Should yield a value");
      seen.insert(shuffled_val);
    }

    // If the length of the set matches the total items,
    // mathematically nothing was missed or left out.
    assert_eq!(
      seen.len(),
      total_items as usize,
      "Leak detected! Some values from the range were skipped."
    );
  }
}
