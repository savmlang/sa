use crate::{
  acaot::acdag::fixedvec::FixedVec,
  permute::{HashedPermutation, ShuffledSliceIter},
};
use ahash::{HashSet, HashSetExt};
use std::num::NonZeroU64;

pub mod fixedvec;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum HashState {
  Handled,
  Cyclic,
  Pending,
  NotVisited,
}

pub fn acdag_resolve<E: AsRef<[u64]>, F: FnMut(u64) -> E>(
  length: NonZeroU64,
  resolver: &mut F,
) -> (FixedVec<u64>, HashSet<u64>) {
  let num_nodes = length.get() as usize;
  let permutations = HashedPermutation::new_panicking(length);

  let permutations_iter = permutations.to_iter();

  let mut visited = HashSet::with_capacity(num_nodes);
  let mut visiting = HashSet::with_capacity(num_nodes);
  let mut cycle_nodes = HashSet::with_capacity(num_nodes);

  let mut order = FixedVec::new(num_nodes);

  // Explicit heap-allocated execution stack for our DFS.
  // It stores: (current_node, index_of_next_dependency_to_process)
  let mut stack = FixedVec::new(num_nodes);

  // A parallel vector mirroring `stack` to give us O(1) loop lookups
  // for fast cycle extraction without searching the tuple stack.
  let mut path = FixedVec::new(num_nodes);

  for start_node in permutations_iter {
    if visited.contains(&start_node) {
      continue;
    }

    // Push the starting root node onto our manual stack
    stack
      .push((start_node, 0))
      .expect("This shouldn't overallocate");
    visiting.insert(start_node);
    path.push(start_node).expect("This shouldn't overallocate");

    while let Some((curr, dep_idx)) = stack.pop() {
      if curr < length.get() {
        let depdata = resolver(curr);
        let deps = ShuffledSliceIter::new(depdata.as_ref(), permutations.seed);

        // 1. ADVANCE: If we still have dependencies left to explore for this node, iterate
        if let Some(next_dep) = deps.get(dep_idx) {
          // Put the current node back on the stack, incrementing its index for next time
          _ = stack.push((curr, dep_idx + 1));

          if visiting.contains(next_dep) {
            // Cycle detected! Extract participants from our active path stack
            if let Some(pos) = path.iter().position(|x| x == next_dep) {
              for member in &path[pos..] {
                cycle_nodes.insert(*member);
              }
            }
          } else if !visited.contains(next_dep) {
            // Valid unvisited dependency, push it onto the stack to explore deeper
            visiting.insert(*next_dep);
            path.push(*next_dep).expect("This shouldn't overallocate");
            stack
              .push((*next_dep, 0))
              .expect("This shouldn't overallocate");
          }
        } else {
          // 2. BACKTRACK: All dependencies for this node have been fully evaluated
          visiting.remove(&curr);
          path.pop();
          visited.insert(curr);
          order.push(curr).expect("This shouldn't overallocate");
        }
      } else {
        // Node has no dependencies listed in the graph mapping
        visiting.remove(&curr);
        path.pop();
        visited.insert(curr);
        order.push(curr).expect("This shouldn't overallocate");
      }
    }
  }

  (order, cycle_nodes)
}
