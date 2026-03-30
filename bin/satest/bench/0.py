import time
import statistics

def benchmark_while_loop(iterations):
    # Mimics your 2.asm logic exactly
    r1 = 0
    r2 = 1
    r8 = iterations
    
    start_time = time.perf_counter()
    while r1 < r8:
        r1 += r2
    end_time = time.perf_counter()
    
    return (end_time - start_time) * 1000  # Convert to ms

def run_benchmarks(num_runs=20, iterations=1_000_000):
    results = []
    print(f"--- Running {num_runs} Trials ({iterations:,} iterations each) ---")
    
    for i in range(num_runs):
        duration = benchmark_while_loop(iterations)
        results.append(duration)
        print(f"Run {i+1:02d}: {duration:.4f} ms")
        
    median_val = statistics.median(results)
    
    print("-" * 40)
    print(f"YOUR ASSEMBLER: ~106.65 ms")
    print(f"PYTHON MEDIAN:  {median_val:.4f} ms")
    print("-" * 40)
    
    if 106.65 < median_val:
        print("STATUS: SaVM is BEATING Python! 🚀")
    else:
        print(f"STATUS: Python is faster by {106.65 - median_val:.2f} ms.")

if __name__ == "__main__":
    run_benchmarks()