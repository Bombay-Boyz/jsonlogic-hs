# The community test suite

These files are a verbatim copy of the `suites/` directory of
[json-logic/compat-tables](https://github.com/json-logic/compat-tables) at
commit `f70933b8d9ef363afc73af6f6e12e9161fedce17` (2026-10-19), licensed under
the Apache License 2.0 (`LICENSE` in this directory). `index.json` lists the
files in the order the compatibility tables use.

The suite has 1,138 cases. Its first file, `compatible.json`, is the classic
suite (the same 278 rules as `test/tests.json`). The rest describe the
**JSON Logic 2.0 dialect** implemented by json-logic-engine: new operators
(`val`, `??`, `exists`, `try`, `throw`, ...), `NaN` as an error rather than a
value, variadic `/` and `-`, chained comparisons, and lazily evaluated
comparison chains. jsonlogic-hs implements the classic dialect of
json-logic-js 2.0.5, so most of those cases are not expected to pass, and the
gaps are a statement of scope, not bugs (see `docs/COMPATIBILITY.md`).

They are used twice:

- **As a measure.** `cabal test jsonlogic-hs-community` scores every case the
  way the compatibility tables score an implementation, in both arity modes,
  and compares the per-file pass counts with `expected.json`. Any change,
  better or worse, fails the run until `expected.json` is updated in the same
  commit, so the score can only move on purpose. Regenerate it with
  `cabal run jsonlogic-hs-community -- --write`.
- **As more outliers.** `differential/outliers.js` runs every rule of the suite
  through json-logic-js and records its answers in `test/outliers.jsonl`
  (family `community`), so lenient mode is held to the reference on all 1,138
  rules, including the ones written for the other dialect.

To update the suite, copy a newer `suites/` here, record the commit above,
regenerate `test/outliers.jsonl` and `expected.json`, and update
`fixtures.sha256`.
