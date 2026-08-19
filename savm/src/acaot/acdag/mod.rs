use crate::{acaot::acdag::fixedvec::FixedVec, permute::HashedPermutation};
use std::{num::NonZeroU64, ops::Range};

pub mod fixedvec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDef {
  pub functions: Range<u64>,
}

impl ModuleDef {
  #[inline(always)]
  pub const fn new(functions: Range<u64>) -> Self {
    Self { functions }
  }
}

impl From<Range<u64>> for ModuleDef {
  #[inline(always)]
  fn from(functions: Range<u64>) -> Self {
    Self { functions }
  }
}

pub struct ModuleAwareResolution {
  pub module_order: FixedVec<u64>,

  pub intra_module_order: FixedVec<FixedVec<u64>>,

  pub cyclic: FixedVec<u64>,
}

impl std::fmt::Debug for ModuleAwareResolution {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let intra_debug: Vec<&[u64]> = self.intra_module_order.iter().map(|fns| &fns[..]).collect();

    f.debug_struct("ModuleAwareResolution")
      .field("module_order", &&self.module_order[..])
      .field("intra_module_order", &intra_debug)
      .field("cyclic", &&self.cyclic[..])
      .finish()
  }
}

impl ModuleAwareResolution {
  #[inline(always)]
  pub fn is_acyclic(&self) -> bool {
    self.cyclic.is_empty()
  }

  #[inline(always)]
  pub fn has_cyclic(&self) -> bool {
    !self.cyclic.is_empty()
  }

  #[inline(always)]
  pub fn is_cyclic(&self, fn_id: u64) -> bool {
    self.cyclic.binary_search(&fn_id).is_ok()
  }
}

pub fn acdag_module_resolve<F: FnMut(u64) -> Range<u64>>(
  modules: &[ModuleDef],
  resolver: &mut F,
) -> ModuleAwareResolution {
  let num_modules = modules.len();
  if num_modules == 0 {
    return ModuleAwareResolution {
      module_order: FixedVec::new(0),
      intra_module_order: FixedVec::new(0),
      cyclic: FixedVec::new(0),
    };
  }

  let mut max_fn_id = 0u64;
  let mut max_fns_in_module = 0usize;

  for m in modules {
    let count = (m.functions.end.saturating_sub(m.functions.start)) as usize;
    if count > max_fns_in_module {
      max_fns_in_module = count;
    }
    if m.functions.end > max_fn_id {
      max_fn_id = m.functions.end;
    }
  }

  let lookup_cap = (max_fn_id + 1) as usize;
  let mut fn_to_loc: FixedVec<(u32, u32)> = FixedVec::factory(|_| (u32::MAX, u32::MAX), lookup_cap);

  for (mod_id, m) in modules.iter().enumerate() {
    let start = m.functions.start;
    let end = m.functions.end;
    for fn_id in start..end {
      let local_idx = (fn_id - start) as u32;
      fn_to_loc[fn_id as usize] = (mod_id as u32, local_idx);
    }
  }

  let use_dense_matrix = num_modules <= 2048;
  let max_deps_per_module = if use_dense_matrix { num_modules } else { 16 };

  let mut inter_mod_deps: FixedVec<FixedVec<usize>> =
    FixedVec::factory(|_| FixedVec::new(max_deps_per_module), num_modules);

  let mut inter_dep_matrix: FixedVec<bool> = if use_dense_matrix {
    FixedVec::factory(|_| false, num_modules * num_modules)
  } else {
    FixedVec::new(0)
  };

  for (mod_id, m) in modules.iter().enumerate() {
    for fn_id in m.functions.start..m.functions.end {
      let dep_range = resolver(fn_id);
      for dep_fn in dep_range {
        if (dep_fn as usize) < lookup_cap {
          let (dep_mod_id_u32, _) = fn_to_loc[dep_fn as usize];
          if dep_mod_id_u32 != u32::MAX {
            let dep_mod_id = dep_mod_id_u32 as usize;
            if dep_mod_id != mod_id {
              if use_dense_matrix {
                let matrix_idx = mod_id * num_modules + dep_mod_id;
                if !inter_dep_matrix[matrix_idx] {
                  inter_dep_matrix[matrix_idx] = true;
                  _ = inter_mod_deps[mod_id].push(dep_mod_id);
                }
              } else {
                let current_deps = &inter_mod_deps[mod_id];
                if !current_deps.contains(&dep_mod_id) {
                  _ = inter_mod_deps[mod_id].push(dep_mod_id);
                }
              }
            }
          }
        }
      }
    }
  }

  let mod_length = NonZeroU64::new(num_modules as u64).unwrap();
  let mod_permutations = HashedPermutation::new_panicking(mod_length);

  let mut mod_visited = FixedVec::factory(|_| false, num_modules);
  let mut mod_visiting = FixedVec::factory(|_| false, num_modules);
  let mut mod_order_indices = FixedVec::new(num_modules);
  let mut mod_stack = FixedVec::new(num_modules);

  for start_mod_idx_u64 in mod_permutations.to_iter() {
    let start_mod_idx = start_mod_idx_u64 as usize;
    if start_mod_idx >= num_modules || mod_visited[start_mod_idx] {
      continue;
    }

    _ = mod_stack.push((start_mod_idx, 0));
    mod_visiting[start_mod_idx] = true;

    while let Some((curr_mod, dep_idx)) = mod_stack.pop() {
      let deps = &inter_mod_deps[curr_mod];
      let num_deps = deps.len();

      let next_dep_mod = if num_deps > 0 {
        if let Some(nonzero) = NonZeroU64::new(num_deps as u64) {
          let perm = HashedPermutation::new_with_seed(nonzero, mod_permutations.seed);
          perm
            .shuffle(dep_idx as u64)
            .and_then(|idx| deps.get(idx as usize))
            .copied()
        } else {
          None
        }
      } else {
        None
      };

      if let Some(next_dep) = next_dep_mod {
        _ = mod_stack.push((curr_mod, dep_idx + 1));

        if !mod_visiting[next_dep] && !mod_visited[next_dep] {
          mod_visiting[next_dep] = true;
          _ = mod_stack.push((next_dep, 0));
        }
      } else {
        mod_visiting[curr_mod] = false;
        mod_visited[curr_mod] = true;
        _ = mod_order_indices.push(curr_mod);
      }
    }
  }

  let mut module_order = FixedVec::new(num_modules);
  for &m_idx in mod_order_indices.iter() {
    _ = module_order.push(m_idx as u64);
  }

  let mut intra_module_order = FixedVec::new(num_modules);

  let mut shared_visited = FixedVec::factory(|_| false, max_fns_in_module);
  let mut shared_visiting = FixedVec::factory(|_| false, max_fns_in_module);
  let mut shared_stack = FixedVec::new(max_fns_in_module);

  for &m_idx in mod_order_indices.iter() {
    let module_def = &modules[m_idx];
    let fn_start = module_def.functions.start;
    let fn_end = module_def.functions.end;
    let fn_count = (fn_end.saturating_sub(fn_start)) as usize;

    if fn_count == 0 {
      _ = intra_module_order.push(FixedVec::new(0));
      continue;
    }

    for i in 0..fn_count {
      shared_visited[i] = false;
      shared_visiting[i] = false;
    }

    let mut local_fn_order = FixedVec::new(fn_count);
    let fn_length = NonZeroU64::new(fn_count as u64).unwrap();
    let fn_permutations = HashedPermutation::new_panicking(fn_length);

    for start_local_idx_u64 in fn_permutations.to_iter() {
      let start_local_idx = start_local_idx_u64 as usize;
      if start_local_idx >= fn_count || shared_visited[start_local_idx] {
        continue;
      }

      _ = shared_stack.push((start_local_idx, 0));
      shared_visiting[start_local_idx] = true;

      while let Some((curr_local, dep_idx)) = shared_stack.pop() {
        let curr_fn_id = fn_start + curr_local as u64;
        let dep_range = resolver(curr_fn_id);
        let dep_len = dep_range.end.saturating_sub(dep_range.start);

        let dep_opt = if dep_len > 0 {
          if let Some(nonzero) = NonZeroU64::new(dep_len) {
            let perm = HashedPermutation::new_with_seed(nonzero, fn_permutations.seed);
            perm
              .shuffle(dep_idx as u64)
              .map(|offset| dep_range.start + offset)
          } else {
            None
          }
        } else {
          None
        };

        if let Some(dep_fn) = dep_opt {
          _ = shared_stack.push((curr_local, dep_idx + 1));

          if (dep_fn as usize) < lookup_cap {
            let (dep_mod_id_u32, dep_local_idx_u32) = fn_to_loc[dep_fn as usize];
            if dep_mod_id_u32 as usize == m_idx {
              let dep_local_idx = dep_local_idx_u32 as usize;
              if !shared_visiting[dep_local_idx] && !shared_visited[dep_local_idx] {
                shared_visiting[dep_local_idx] = true;
                _ = shared_stack.push((dep_local_idx, 0));
              }
            }
          }
        } else {
          shared_visiting[curr_local] = false;
          shared_visited[curr_local] = true;
          _ = local_fn_order.push(curr_fn_id);
        }
      }
    }

    _ = intra_module_order.push(local_fn_order);
  }

  let mut linked_mask: FixedVec<bool> = FixedVec::factory(|_| false, lookup_cap);
  let mut cyclic_mask: FixedVec<bool> = FixedVec::factory(|_| false, lookup_cap);
  let mut cyclic_count = 0usize;

  for fns in intra_module_order.iter() {
    for &fn_id in fns.iter() {
      let dep_range = resolver(fn_id);
      for dep_fn in dep_range {
        if (dep_fn as usize) < lookup_cap {
          let (dep_mod_id, _) = fn_to_loc[dep_fn as usize];
          if dep_mod_id != u32::MAX && !linked_mask[dep_fn as usize] {
            if !cyclic_mask[dep_fn as usize] {
              cyclic_mask[dep_fn as usize] = true;
              cyclic_count += 1;
            }
          }
        }
      }
      linked_mask[fn_id as usize] = true;
    }
  }

  let mut cyclic = FixedVec::new(cyclic_count);
  for fn_id in 0..lookup_cap {
    if cyclic_mask[fn_id] {
      _ = cyclic.push(fn_id as u64);
    }
  }

  ModuleAwareResolution {
    module_order,
    intra_module_order,
    cyclic,
  }
}

pub fn acdag_resolve<F: FnMut(u64) -> Range<u64>>(
  length: NonZeroU64,
  resolver: &mut F,
) -> (FixedVec<u64>, FixedVec<u64>) {
  let num_nodes = length.get() as usize;
  let permutations = HashedPermutation::new_panicking(length);

  let permutations_iter = permutations.to_iter();

  let mut visited = FixedVec::factory(|_| false, num_nodes);
  let mut visiting = FixedVec::factory(|_| false, num_nodes);
  let mut cycle_mask = FixedVec::factory(|_| false, num_nodes);

  let mut order = FixedVec::new(num_nodes);
  let mut stack = FixedVec::new(num_nodes);

  for start_node_u64 in permutations_iter {
    let start_node = start_node_u64 as usize;
    if start_node >= num_nodes || visited[start_node] {
      continue;
    }

    _ = stack.push((start_node_u64, 0));
    visiting[start_node] = true;

    while let Some((curr, dep_idx)) = stack.pop() {
      let curr_idx = curr as usize;
      if curr < length.get() {
        let dep_range = resolver(curr);
        let dep_len = dep_range.end.saturating_sub(dep_range.start);

        let dep_opt = if dep_len > 0 {
          if let Some(nonzero) = NonZeroU64::new(dep_len) {
            let perm = HashedPermutation::new_with_seed(nonzero, permutations.seed);
            perm
              .shuffle(dep_idx as u64)
              .map(|offset| dep_range.start + offset)
          } else {
            None
          }
        } else {
          None
        };

        if let Some(next_dep) = dep_opt {
          _ = stack.push((curr, dep_idx + 1));
          let next_dep_idx = next_dep as usize;

          if next_dep_idx < num_nodes && visiting[next_dep_idx] {
            cycle_mask[curr_idx] = true;
            cycle_mask[next_dep_idx] = true;
          } else if next_dep_idx < num_nodes && !visited[next_dep_idx] {
            visiting[next_dep_idx] = true;
            _ = stack.push((next_dep, 0));
          }
        } else {
          visiting[curr_idx] = false;
          visited[curr_idx] = true;
          _ = order.push(curr);
        }
      } else {
        if curr_idx < num_nodes {
          visiting[curr_idx] = false;
          visited[curr_idx] = true;
        }
        _ = order.push(curr);
      }
    }
  }

  let mut cycle_nodes = FixedVec::new(num_nodes);
  for i in 0..num_nodes {
    if cycle_mask[i] {
      _ = cycle_nodes.push(i as u64);
    }
  }

  (order, cycle_nodes)
}
