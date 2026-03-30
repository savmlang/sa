function runTests() {
  let ctr = 0;

  while (ctr < 1000000) {
    ctr += 1;
  }
}

const iterations = 100;
const latencies = new BigUint64Array(iterations);

for (let i = 0; i < iterations; i++) {
  const start = process.hrtime.bigint();
  runTests();
  const end = process.hrtime.bigint();
  latencies[i] = end - start;
}

// Calculate Median
latencies.sort();
const median = latencies[iterations / 2];

console.log(`Median Execution Time: ${median} ns`);
