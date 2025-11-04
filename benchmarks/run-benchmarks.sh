#!/bin/bash

# izumi-reflect Benchmark Runner
# Automates common benchmark execution scenarios

set -e

BENCHMARK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$BENCHMARK_DIR")"

echo "=== izumi-reflect Benchmark Suite ==="
echo

# Function to run benchmarks with specific configuration
run_benchmark() {
    local name=$1
    local pattern=$2
    local extra_args=$3
    
    echo "Running $name benchmarks..."
    echo "Pattern: $pattern"
    echo "Args: $extra_args"
    echo
    
    sbt "jmh:run $pattern $extra_args"
    echo
}

# Parse command line arguments
BENCHMARK_TYPE=${1:-"all"}
OUTPUT_FORMAT=${2:-"text"}

case $BENCHMARK_TYPE in
    "all")
        echo "Running complete benchmark suite..."
        run_benchmark "Complete" "" "-rf json -rff results.json"
        ;;
    "cache")
        echo "Running cache-specific benchmarks..."
        run_benchmark "Cache Operations" "CacheBenchmarks" "-rf json -rff cache-results.json"
        ;;
    "compile")
        echo "Running compile-time scenario benchmarks..."
        run_benchmark "Compile-Time" "CompileTimeBenchmarks" "-rf json -rff compile-results.json"
        ;;
    "quick")
        echo "Running quick benchmark (reduced iterations)..."
        run_benchmark "Quick Test" "" "-wi 2 -i 3 -f 1"
        ;;
    "memory")
        echo "Running benchmarks with memory profiling..."
        run_benchmark "Memory Profile" "" "-prof gc -rf json -rff memory-results.json"
        ;;
    "concurrency")
        echo "Running concurrency-focused benchmarks..."
        run_benchmark "Concurrent Access" ".*concurrent.*" "-t 8 -rf json -rff concurrency-results.json"
        ;;
    *)
        echo "Usage: $0 [benchmark_type] [output_format]"
        echo
        echo "Benchmark types:"
        echo "  all        - Run complete benchmark suite (default)"
        echo "  cache      - Run cache operation benchmarks"
        echo "  compile    - Run compile-time scenario benchmarks"
        echo "  quick      - Run quick test with reduced iterations"
        echo "  memory     - Run with GC profiling"
        echo "  concurrency- Run concurrent access benchmarks"
        echo
        echo "Output formats:"
        echo "  text       - Console output (default)"
        echo "  json       - JSON format output"
        echo
        echo "Examples:"
        echo "  $0 all json                # Complete suite with JSON output"
        echo "  $0 cache                   # Cache benchmarks only"
        echo "  $0 quick                   # Fast test run"
        echo
        exit 1
        ;;
esac

echo "=== Benchmark run completed ==="

if [ -f "results.json" ]; then
    echo
    echo "Results saved to: results.json"
    echo "You can analyze the results using JMH visualization tools or custom scripts."
fi