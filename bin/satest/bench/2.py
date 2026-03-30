import time
import statistics

def run_tests():
    # Registers
    r1 = 0          # Counter
    r2 = 1          # Increment
    r8 = 1_000_000  # Stop value

    while True:
        # vadd r1 = r1 + r2 (+ offset 1, simulated)
        r1 = r1 + r2 + 1  # the assembler offset

        # #eq macro: compare r1 and r8
        r7 = 1 if r1 == r8 else 0

        # jif (jump if non-zero)
        if r7 != 0:
            break  # jump to mark 2

        # else continue looping (jmp 1)
        # loop continues automatically


# Execution loop
latencies = []

for _ in range(100):
    start = time.perf_counter_ns()
    run_tests()
    end = time.perf_counter_ns()
    latencies.append(end - start)

median_latency = statistics.median(latencies)

print(f"Median Execution Time: {median_latency:.2f} ns")