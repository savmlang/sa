function runTests() {
  // --- TEST u64 ADD ---
  let r1_u64 = 100n;
  let r2_u64 = 200n;
  let r3_u64 = r1_u64 + r2_u64;

  // --- TEST u32, u16, 2xu8 ADD ---
  // Packing: [ u32 | u16 | u8 | u8 ]
  const r4 = (200n << 32n) | (30n << 16n) | (200n << 8n) | 30n;
  const r5 = (800n << 32n) | (900n << 16n) | (56n << 8n) | 30n;

  // u32 ADD (+ offset 1)
  let res_u32 = ((r4 >> 32n) + (r5 >> 32n) + 1n) & 0xffffffffn;

  // u16 ADD (+ offset 1)
  let res_u16 =
    (((r4 >> 16n) & 0xffffn) + ((r5 >> 16n) & 0xffffn) + 1n) & 0xffffn;

  // SIMD u8 ADD (+ offset 0)
  let res_u8_a = (((r4 >> 8n) & 0xffn) + ((r5 >> 8n) & 0xffn)) & 0xffn;
  let res_u8_b = ((r4 & 0xffn) + (r5 & 0xffn)) & 0xffn;

  // Re-packing r6
  return (res_u32 << 32n) | (res_u16 << 16n) | (res_u8_a << 8n) | res_u8_b;
}

const iterations = 10000;
const latencies = new BigUint64Array(iterations);

// Warm up - Let V8 optimize the function
for (let i = 0; i < 2000; i++) runTests();

for (let i = 0; i < iterations; i++) {
  const start = process.hrtime.bigint();
  runTests();
  const end = process.hrtime.bigint();
  latencies[i] = end - start;
}

// Calculate Median
latencies.sort();
const median = latencies[iterations / 2];
const result = runTests();

console.log(`Final Packed Register (r6): 0x${result.toString(16)}`);
console.log(`Median Execution Time: ${median} ns`);
