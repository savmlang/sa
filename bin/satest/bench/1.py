import time
import statistics

def run_tests():
    # --- TEST u64 ADD ---
    # r1 = 100, r2 = 200
    r1_u64 = 100
    r2_u64 = 200
    r3_u64 = r1_u64 + r2_u64 # Result: 300

    # --- TEST u32, u16, 2xu8 ADD ---
    # Packing: [ u32 | u16 | u8 | u8 ]
    def pack_val(v32, v16, v8_a, v8_b):
        return (v32 << 32) | (v16 << 16) | (v8_a << 8) | v8_b

    def unpack_val(val):
        return {
            "u32": (val >> 32) & 0xFFFFFFFF,
            "u16": (val >> 16) & 0xFFFF,
            "u8_a": (val >> 8) & 0xFF,
            "u8_b": val & 0xFF
        }

    r4 = pack_val(200, 30, 200, 30)
    r5 = pack_val(800, 900, 56, 30)

    # u32 ADD (+ offset 1)
    # The vadd offset 1 suggests adding 1 to the result of the packed components
    res_u32 = ((r4 >> 32) + (r5 >> 32) + 1) & 0xFFFFFFFF
    
    # u16 ADD (+ offset 1)
    res_u16 = (((r4 >> 16) & 0xFFFF) + ((r5 >> 16) & 0xFFFF) + 1) & 0xFFFF
    
    # SIMD u8 ADD (+ offset 0)
    # Note: SIMD u8 adds both u8 slots independently
    res_u8_a = (((r4 >> 8) & 0xFF) + ((r5 >> 8) & 0xFF)) & 0xFF
    res_u8_b = ((r4 & 0xFF) + (r5 & 0xFF)) & 0xFF

    # Re-packing r6 (simulating the final state)
    r6 = (res_u32 << 32) | (res_u16 << 16) | (res_u8_a << 8) | res_u8_b
    return r6

# Execution loop
latencies = []

for _ in range(10_000):
    start = time.perf_counter_ns()
    run_tests()
    end = time.perf_counter_ns()
    latencies.append(end - start)

median_latency = statistics.median(latencies)

print(f"Median Execution Time: {median_latency:.2f} ns")