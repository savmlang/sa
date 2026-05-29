use crate::permute::HashedPermutation;

pub struct HashedPermutationIter {
  permutation: HashedPermutation,
  current_step: u64,
}

impl HashedPermutation {
  pub fn to_iter(&self) -> HashedPermutationIter {
    HashedPermutationIter {
      permutation: HashedPermutation {
        seed: self.seed,
        length: self.length,
      },
      current_step: 0,
    }
  }

  pub fn into_iter(self) -> HashedPermutationIter {
    HashedPermutationIter {
      permutation: self,
      current_step: 0,
    }
  }
}

impl Iterator for HashedPermutationIter {
  type Item = u64;

  fn next(&mut self) -> Option<Self::Item> {
    let result = self.permutation.shuffle(self.current_step);
    if result.is_some() {
      self.current_step += 1;
    }
    result
  }
}
