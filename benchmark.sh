#!/usr/bin/env bash
# =============================================================================
# benchmark.sh  –  Compare CPU vs GPU performance for HeatFlow/ThermalFlow
#
# Usage:
#   ./benchmark.sh [RUN_DIR] [REPEATS]
#
#   RUN_DIR  : directory containing inputs/ (default: /home/hm556/Cloak)
#   REPEATS  : number of timed runs per backend (default: 3)
#
# Output:
#   Prints a table comparing wall times for the simulation loop.
#   Results are also written to benchmark_results.txt in RUN_DIR.
# =============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RUN_DIR="${1:-/home/hm556/Cloak}"
REPEATS="${2:-3}"

CPU_BIN="$SCRIPT_DIR/bin/ThermalFlow.x"
GPU_BIN="$SCRIPT_DIR/bin/ThermalFlow-gpu.x"
RESULTS="$RUN_DIR/benchmark_results.txt"
COMMON_ENV=(OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 OMP_PROC_BIND=spread OMP_PLACES=threads)
GPU_ARGS=(-use_gpu_aware_mpi 0)

# ── helpers ──────────────────────────────────────────────────────────────────
die()  { echo "ERROR: $*" >&2; exit 1; }
sep()  { printf '%s\n' "$(printf '─%.0s' {1..60})"; }
bold() { printf '\033[1m%s\033[0m\n' "$*"; }

extract_wall() {
    # Pull "simulation wall time =   NNN.NNN s" from stdout
    awk '/simulation wall time =/ {print $(NF-1); found=1} END {if (!found) print "N/A"}' "$1"
}

run_once() {
    local bin="$1" logfile="$2"
    cd "$RUN_DIR"
    if [[ "$bin" == "$GPU_BIN" ]]; then
        env "${COMMON_ENV[@]}" "$bin" "${GPU_ARGS[@]}" > "$logfile" 2>&1
    else
        env "${COMMON_ENV[@]}" "$bin" > "$logfile" 2>&1
    fi
    extract_wall "$logfile"
}

median() {
    # $@ = list of floats; print median
    python3 -c "
import sys, statistics
vals = [float(x) for x in sys.argv[1:] if x != 'N/A']
if not vals:
    print('N/A')
else:
    print(f'{statistics.median(vals):.3f}')
" "$@"
}

# ── build ─────────────────────────────────────────────────────────────────────
bold "=== HeatFlow CPU / GPU Benchmark ==="
echo "Run dir : $RUN_DIR"
echo "Repeats : $REPEATS"
sep

echo ""
bold "Step 1/2 – Building CPU binary (system PETSc)"
cd "$SCRIPT_DIR"
make clean -s
make -s
[[ -f "$CPU_BIN" ]] || die "CPU binary not found after build: $CPU_BIN"
echo "  → $CPU_BIN"

echo ""
bold "Step 2/2 – Building GPU binary (PETSc native CUDA)"
make gpuclean -s 2>/dev/null || true
make gpu -s
[[ -f "$GPU_BIN" ]] || die "GPU binary not found after build: $GPU_BIN"
echo "  → $GPU_BIN"

# ── run ───────────────────────────────────────────────────────────────────────
sep
bold "Running benchmarks  ($REPEATS run(s) each) …"

CPU_TIMES=()
GPU_TIMES=()

for i in $(seq 1 "$REPEATS"); do
    echo ""
    echo "  CPU run $i/$REPEATS …"
    t=$(run_once "$CPU_BIN" "$RUN_DIR/.bench_cpu_$i.log")
    CPU_TIMES+=("$t")
    echo "    wall time = ${t} s"

    echo "  GPU run $i/$REPEATS …"
    t=$(run_once "$GPU_BIN" "$RUN_DIR/.bench_gpu_$i.log")
    GPU_TIMES+=("$t")
    echo "    wall time = ${t} s"
done

# ── report ────────────────────────────────────────────────────────────────────
CPU_MED=$(median "${CPU_TIMES[@]}")
GPU_MED=$(median "${GPU_TIMES[@]}")

speedup="N/A"
if [[ "$CPU_MED" != "N/A" && "$GPU_MED" != "N/A" ]]; then
    speedup=$(python3 -c "print(f'{$CPU_MED / $GPU_MED:.2f}x')")
fi

sep
bold "Results"
sep
printf "%-30s %s\n"   "CPU wall time (median) :"  "${CPU_MED} s   [${CPU_TIMES[*]}]"
printf "%-30s %s\n"   "GPU wall time (median) :"  "${GPU_MED} s   [${GPU_TIMES[*]}]"
printf "%-30s %s\n"   "GPU speedup            :"  "$speedup"
sep

# Save results
{
    echo "HeatFlow CPU/GPU Benchmark – $(date)"
    echo "Run dir  : $RUN_DIR"
    echo "Repeats  : $REPEATS"
    echo "CPU bin  : $CPU_BIN"
    echo "GPU bin  : $GPU_BIN"
    echo ""
    echo "CPU wall time (median) : ${CPU_MED} s"
    echo "GPU wall time (median) : ${GPU_MED} s"
    echo "GPU speedup            : $speedup"
    echo ""
    echo "Individual CPU runs (s): ${CPU_TIMES[*]}"
    echo "Individual GPU runs (s): ${GPU_TIMES[*]}"
} | tee "$RESULTS"

echo ""
echo "Full results written to $RESULTS"
