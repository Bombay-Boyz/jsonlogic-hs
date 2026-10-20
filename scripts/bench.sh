#!/usr/bin/env bash
# Runs the benchmark suite and records the numbers with the environment they
# were taken in, so a result can be traced and repeated.
#
#   bash scripts/bench.sh [OUTPUT_DIR]      (default: bench-results)
#
# Writes OUTPUT_DIR/results.csv (tasty-bench: mean, stdev, allocations, and
# peak memory per workload) and OUTPUT_DIR/environment.json. Extra
# arguments after the directory go to the benchmark executable, for example
# `-p precompiled` to run one group.
set -euo pipefail
cd "$(dirname "$0")/.."

out=${1:-bench-results}
[ "$#" -gt 0 ] && shift
mkdir -p "$out"

sha256() { if command -v sha256sum >/dev/null; then sha256sum "$1"; else shasum -a 256 "$1"; fi | cut -d' ' -f1; }
cpu=$( (grep -m1 'model name' /proc/cpuinfo 2>/dev/null | cut -d: -f2- | sed 's/^ *//') || true)
[ -n "$cpu" ] || cpu=$(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo unknown)
json_escape() { printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'; }

cat > "$out/environment.json" <<EOF
{
  "commit": "$(git rev-parse HEAD 2>/dev/null || echo unknown)",
  "dirty": $(if [ -n "$(git status --porcelain 2>/dev/null)" ]; then echo true; else echo false; fi),
  "date_utc": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "ghc": "$(json_escape "$(ghc --numeric-version)")",
  "cabal": "$(json_escape "$(cabal --numeric-version)")",
  "os": "$(json_escape "$(uname -srm)")",
  "cpu": "$(json_escape "$cpu")",
  "cores": $(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 0),
  "optimisation": "-O2",
  "workload_source_sha256": "$(sha256 bench/Bench.hs)"
}
EOF

cabal run -v0 -O2 jsonlogic-hs-bench -- --csv "$out/results.csv" "$@"
echo "results: $out/results.csv"
echo "environment: $out/environment.json"
