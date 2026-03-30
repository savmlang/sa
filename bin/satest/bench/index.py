import time

def test_1():
    # 1. TEST u64 ADD
    r1 = 100
    r2 = 200
    r3 = (r1 + r2) & 0xFFFFFFFFFFFFFFFF

    # 2. Pack the u64 (u32 | u16 | u8 | u8)
    r4 = (200 << 32) | (30 << 16) | (200 << 8) | 30
    r5 = (800 << 32) | (900 << 16) | (56 << 8) | 30

    # 3. u32 ADD (+ offset 1)
    r6_u32 = ((r4 >> 32) + (r5 >> 32) + 1) & 0xFFFFFFFF

    # 4. u16 ADD (+ offset 1)
    r4_u16 = (r4 >> 16) & 0xFFFF
    r5_u16 = (r5 >> 16) & 0xFFFF
    r6_u16 = (r4_u16 + r5_u16 + 1) & 0xFFFF

    # 5. SIMD u8 ADD (2 iterations)
    # Adding the two u8 lanes separately
    for i in range(2):
        shift = i * 8
        lane_r4 = (r4 >> shift) & 0xFF
        lane_r5 = (r5 >> shift) & 0xFF
        res = (lane_r4 + lane_r5) & 0xFF

start = time.perf_counter_ns()
test_1()
end = time.perf_counter_ns()

print(f"Test #1: {(end - start) / 1000:.3f}µs")