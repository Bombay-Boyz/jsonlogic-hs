# Changelog for jsonlogic-hs

## 0.1.0.0 -- unreleased

First release.

### Reading and evaluating rules

* Every builtin operator of the classic JsonLogic specification, with
  JavaScript's semantics as json-logic-js 2.0.5 implements them: coercion,
  `NaN`, the infinities, `undefined`, negative zero, UTF-16 strings, and
  JavaScript-identical number parsing and printing.
* `apply`, `applyWith` and `applyWithConfig`; `compile` to read, check and
  build a rule once, and `run` to evaluate it any number of times. Custom
  operators are resolved when the rule is compiled.
* Strict arity by default (a precise `ArityMismatch`); `arityMode = Lenient`
  behaves exactly like the reference, including evaluating surplus operands
  and throwing on an empty `*` only if it is evaluated.
* Configurable resource limits, on by default: rule depth, evaluation steps,
  string length and array length, so a rule from an untrusted source cannot
  exhaust time or memory (`docs/LIMITS.md`). Exceeding one is a typed
  `LimitExceeded` error.
* Every error carries the `Location` of the node that failed, a JSON Pointer
  (RFC 6901): `ParseFailure Location ParseError` and
  `EvalFailure Location EvalError`; `renderError` starts with it.
* Custom operators: `operatorsFromList` refuses a builtin name or a name given
  twice, `Operators` combine with `<>`, and an operator's result is held to
  the depth and step limits.
* `dataUse`: which paths of the data a rule can read, found without running
  it, scope-aware (an iteration body reads the element, not the data), with a
  tested guarantee.

### How it is tested

* All 278 cases of the classic shared suite; the runner fails if its fixture
  is missing, truncated or changed.
* 26,980 outlier cases with the answers json-logic-js 2.0.5 gave, replayed
  offline by `cabal test` in both arity modes, with a 140,000-case tier
  nightly (`differential/outliers.js`).
* The community suite of json-logic/compat-tables (1,138 cases), scored as the
  compatibility tables score implementations and pinned per file: 782 in
  lenient mode, the same as json-logic-js file for file, and 839 in strict
  mode. Every one of its rules is also an outlier case.
* A differential test against the reference with a fresh seed per CI run,
  failing-case shrinking and replay.
* Unit and property tests (278 examples): boundary triples for every limit,
  amplification rules, laws, error locations that always resolve to the
  operator at fault, every fast numeric algorithm against an exact one, and
  `dataUse`'s guarantee.
* Every example in `docs/TUTORIAL.md` is checked by a test suite.
* `scripts/check-harness.sh` feeds every runner missing, empty, truncated and
  tampered fixtures and demands a failure.
* CI: GHC 9.6 to 9.14 on Linux, plus macOS and Windows; `-Werror` on the
  primary compiler; hlint, fourmolu, weeder, shellcheck and actionlint;
  `cabal check`; the source distribution built, tested and documented on its
  own; a downstream project using only `JsonLogic`.

### Deliberate differences from the reference

Listed with reasons and tests in `docs/COMPATIBILITY.md`: strict arity in the
default mode, arrays and objects never equal under `==` / `===`, own
properties only (no prototype chain), data never run as rules by `missing`,
a slice through a surrogate pair gives U+FFFD, `log` does not print, exact
custom operator names, and the resource limits.

### Changes made after the external audit (2026-10-22)

These describe how the release candidate changed; nothing here was ever
released in another form.

* Fixed: lenient mode rejected an empty `*` while reading the rule, so
  `{"if": [true, 1, {"*": []}]}` failed where the reference gives `1`.
* Fixed: the licence text (placeholder copyright line, a typo in the warranty
  clause) and the README, which named the wrong licence. The licence is MIT.
* Fixed: `cabal check` failed on the benchmark's `-O2`; the weeder CI job never
  found its interface files, and is now blocking and clean.
* `compile` takes the custom operators and builds the evaluator once; `run`
  takes only the compiled rule.
* Errors carry locations (`docs/adr/0006`).
* One table of builtin operators (`JsonLogic.Core.Operator`) and one statement
  of each operator's operands (`shapeOf`), replacing four hand-kept lists.
* `recursion-schemes`, `deriving-compat` and Template Haskell are no longer
  dependencies; `vector` is (it was already a dependency of aeson).
* Faster: arrays are vectors (an index is constant time and, as documented,
  costs no extra steps), numbers are read with Clinger's fast path and written
  with Ryū, literal `var` paths are split once, and a step allocates once.
  `map` over 10,000 elements runs in about 2.5 ms instead of 3.3 ms, printing
  1,000 doubles in 0.3 ms instead of 3.1 ms.
* `var`'s strict arity is reported as `Between 0 2`, which is what it always
  accepted.
