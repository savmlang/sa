function runTests() {
  let ctr = 0;

  while (ctr < 1000000) {
    ctr += 1;
  }
}

const iterations = 100;
const latencies = new BigUint64Array(iterations);

// Warm up - Let V8 optimize the function
for (let i = 0; i < 200_000; i++) runTests();

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
