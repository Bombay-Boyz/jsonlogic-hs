# Changelog for jsonlogic-hs

## 0.1.0.0 -- unreleased

First release.

### Reading and evaluating rules

* Reads JsonLogic rules from JSON into a closed, exhaustively matched tree
  and evaluates every builtin operator of the classic specification, with
  JavaScript's coercion rules, including `NaN`, infinities, `undefined`,
  signed zero, UTF-16 strings and JavaScript-identical number parsing and
  printing (including its shortest-digit tie rule).
* `apply`, `applyWith` and `applyWithConfig`; `compile` and `run` to read a
  rule once and run it many times.
* Strict arity by default (a precise `ArityMismatch`); `arityMode = Lenient`
  behaves exactly like the reference implementation.
* Configurable resource limits, on by default: rule depth, evaluation steps,
  string length and array length, so a rule from an untrusted source cannot
  exhaust time or memory (`docs/LIMITS.md`). Exceeding one is a typed
  `LimitExceeded` error.
* Custom operators are supplied by the caller. `operatorsFromList` refuses
  a name that is a builtin or is given twice, `Operators` combine with `<>`,
  and a custom operator's result is held to the depth and step limits.
* Error constructors are stable API; `renderError` is for humans.
* `missing` and `missing_some` follow the reference: an array key is a `var`
  argument list, a string option list counts UTF-16 length, a `null` option
  list is an `InvalidOperand` error.

### How it is tested

* All 278 cases of the classic shared suite. The runner fails if its fixture
  is missing, truncated or changed.
* About 26,000 outlier cases with answers recorded from json-logic-js 2.0.5,
  replayed offline by `cabal test` in both arity modes, with a 138,000-case
  tier nightly (`differential/outliers.js`, `docs/COMPATIBILITY.md`).
* A differential test against the reference with a fresh seed per CI run,
  failing-case shrinking and replay.
* Boundary tests for every limit and amplification tests for rules built to
  exhaust time or memory.
* The harnesses were mutation-checked, and `scripts/check-harness.sh` keeps
  them honest.

### Deliberate differences from the reference

Listed with reasons and tests in `docs/COMPATIBILITY.md`: strict arity in
the default mode, arrays and objects never equal under `==` / `===`, own
properties only (no prototype chain), data never run as rules by `missing`,
a slice through a surrogate pair gives U+FFFD, `log` does not print,
exact custom operator names, and the resource limits.

### Fixed during hardening

Found by the outlier catalogue and fixed before release: string-to-number
whitespace (U+FEFF, U+2028, U+2029 are trimmed; U+0085 and U+180E are not),
negative zero from `%`, `max` and `min`, string length, indexing, `substr` and
ordering by UTF-16 code unit, `missing` with array keys, `missing_some` with
non-array and `null` option lists, undefined-valued object properties,
shortest-digit ties below 2^53, and quadratic-time reading of very long
numeric strings.
