# 0003. Benchmarks use tasty-bench, with regression checks run on a stable machine

**Rule interpreted:** Standard 5.6 asks for `criterion` "or equivalent" with
regression thresholds in CI.

**Decision:** `bench/Bench.hs` uses tasty-bench. It has the same statistical
approach, reports allocation, and supports `--baseline` and `--fail-if-slower`.
It adds one small dependency to the benchmark executable only, not to the library.

**Why not hard thresholds on shared CI runners:** timings from a shared runner
vary by more than the regressions worth catching. Regression checks compare
against a baseline taken on the same machine (`scripts/bench-compare.sh`); the
CI job keeps a smoke run and archives the CSV.
