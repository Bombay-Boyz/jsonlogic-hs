#!/usr/bin/env bash
# Runs the benchmarks and fails if any workload is materially slower than a
# baseline taken on the SAME machine with scripts/bench.sh.
#
#   bash scripts/bench-compare.sh BASELINE.csv [PERCENT]   (default 15)
#
# tasty-bench compares each workload's mean against the baseline, taking both
# standard deviations into account, so noise inside the stated deviation is
# not reported as a regression. Compare only runs from one machine: timings
# from different hardware say nothing about a change in the code.
set -euo pipefail
cd "$(dirname "$0")/.."

baseline=${1:?usage: bench-compare.sh BASELINE.csv [PERCENT]}
percent=${2:-15}
[ -f "$baseline" ] || { echo "no such baseline: $baseline" >&2; exit 2; }

cabal run -v0 -O2 jsonlogic-hs-bench -- --baseline "$baseline" --fail-if-slower "$percent"
