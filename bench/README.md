# Benchmarks

`bench/Bench.hs` times the engine with
[tasty-bench](https://hackage.haskell.org/package/tasty-bench), which runs
each workload until the estimate is stable and reports the mean, its standard
deviation, and bytes allocated.

```
bash scripts/bench.sh                       # writes bench-results/results.csv and environment.json
bash scripts/bench.sh out -p precompiled    # one group only
bash scripts/bench-compare.sh old.csv 15    # fail if anything is 15% slower than old.csv
```

## What is measured

| Group | What it times |
| --- | --- |
| `one-shot` | `apply`: read the rule and evaluate it, once |
| `compile only` | `compile`: read the rule, no evaluation |
| `precompiled run` | `run` of a rule read beforehand (repeated use) |
| `limits` | a rule that is refused at a resource limit |

The same twelve rules are used in the first three groups, covering simple
arithmetic, short-circuiting, deep nesting, variable lookup (nested and deep
into a 10,000-element array), `map`/`filter`/`reduce` over 10,000 elements,
string building, number parsing and printing, and a custom operator.
One-shot minus precompiled is the cost of reading the rule.

## How to read the numbers

- Compare runs from **one machine** only. `environment.json` records the
  commit, GHC, CPU and a hash of `Bench.hs`, so two result files can be seen to
  be comparable (or not).
- Do not draw a conclusion from one short run. Look at the standard deviation
  column and repeat before believing a small difference.
- These workloads are not comparable to other libraries' benchmarks unless the
  other library runs the same rules on the same data; language and runtime
  overhead differ.
