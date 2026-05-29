use savm::acaot::acdag::acdag_resolve;
use std::{
  collections::{HashMap, HashSet},
  num::NonZeroU64,
  ops::Deref,
};

fn main() {
  let mut graph = HashMap::new();

  graph.insert(0, vec![1u64, 5, 9]);
  graph.insert(1, vec![2u64, 6]);
  graph.insert(2, vec![3u64, 7]);
  graph.insert(3, vec![4u64, 8]);
  graph.insert(4, vec![0u64, 9]);
  graph.insert(5, vec![6u64, 1]);
  graph.insert(6, vec![7u64, 2]);
  graph.insert(7, vec![8u64, 3]);
  graph.insert(8, vec![0u64, 4]);
  graph.insert(9, vec![5u64, 2, 0]);

  graph.insert(10, vec![11u64, 12]);
  graph.insert(11, vec![13u64, 14]);
  graph.insert(12, vec![14u64, 15]);
  graph.insert(13, vec![16u64]);
  graph.insert(14, vec![16u64, 17]);
  graph.insert(15, vec![17u64, 18]);
  graph.insert(16, vec![19u64]);
  graph.insert(17, vec![19u64]);
  graph.insert(18, vec![19u64]);
  graph.insert(19, vec![]);

  graph.insert(20, vec![21u64, 0]);
  graph.insert(21, vec![22u64, 10]);
  graph.insert(22, vec![20u64]);
  graph.insert(23, vec![24u64, 25]);
  graph.insert(24, vec![23u64, 26]);
  graph.insert(25, vec![26u64, 5]);
  graph.insert(26, vec![27u64]);
  graph.insert(27, vec![25u64, 15]);
  graph.insert(28, vec![29u64]);
  graph.insert(29, vec![28u64, 8]);

  for i in 0..10_000 {
    let (linker_order, detected_cycles) = acdag_resolve(
      NonZeroU64::new(graph.len() as u64).unwrap(),
      &mut |id| match graph.get(&id) {
        Some(d) => d as &[_],
        None => {
          panic!("Unable to load for {id}");
        }
      },
    );

    println!(
      "#{} Cycles ({}) {linking:?}",
      i + 1,
      detected_cycles.len(),
      linking = linker_order.deref(),
    );
    let mut resolvedmap: HashSet<u64> = HashSet::new();
    for k in linker_order.deref() {
      let v = graph.get(&k).unwrap();
      for dep in v {
        _ = resolvedmap
          .get(dep)
          .or_else(|| detected_cycles.get(dep))
          .unwrap();
      }
      resolvedmap.insert(*k);
    }
  }
}
