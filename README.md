# jsonlogic-hs

A native Haskell implementation of [JsonLogic](https://jsonlogic.com): a
small, closed rule language that is itself JSON, so rules can be stored,
shared between systems, and evaluated against data without running
arbitrary code.

Author: Sabrinathan Nair. Licence: [MPL-2.0](LICENSE).

**Status: release candidate, not yet on Hackage.**

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode)
import JsonLogic (apply)

main :: IO ()
main =
  print (apply <$> decode "{\"==\": [{\"var\": \"a\"}, 1]}" <*> decode "{\"a\": 1}")
  -- Just (Right (Bool True))
```

`apply rule data` takes both as aeson `Value`s and returns
`Either Error Value`. `Error` says whether the rule could not be read
(`ParseFailure`) or could not be evaluated (`EvalFailure`); match on its
constructors, and use `renderError` only for messages.

To run one rule against many inputs, read it once:

```haskell
case compile defaultConfig rule of
  Left err -> ...
  Right compiled -> map (run noOperators compiled) inputs
```

### Rules from an untrusted source

The default `Config` already bounds rule depth (64), evaluation steps
(1,000,000), string length (1 MiB) and array length (100,000), so a hostile
rule cannot hang or exhaust the process; see [`docs/LIMITS.md`](docs/LIMITS.md).
Tighten them with `applyWithConfig defaultConfig {maxSteps = 10000} noOperators rule input`.
The limits bound work, not wall-clock time: add a `timeout` if that matters.

### Strict and lenient arity

By default a wrong number of arguments is a precise error (`{"==": [1]}` is
an `ArityMismatch`). The reference silently ignores extras and pads missing
ones; `defaultConfig {arityMode = Lenient}` behaves exactly like it.

### Custom operators

Operators outside the builtin set are supplied by the caller, at the one
place that needs them:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value (..))
import JsonLogic

doubling :: Operators
doubling = either (error . show) id (operatorsFromList [(OperatorName "double", run')])
  where
    run' _ [Number n] = Right (Number (n * 2))
    run' _ _ = Left "expected one number"

-- applyWith doubling rule input
```

An operator receives the data being evaluated against and its
already-evaluated arguments. An unknown operator is an `EvalFailure`, not a
silent `null`. `operatorsFromList` refuses a name that is a builtin or that
is given twice, instead of ignoring it; registries combine with `<>`
(left-biased). The full contract is on `CustomOperator`.

## How it works

- A rule is read from JSON into a closed tree (`RuleF`, tied with `Fix`).
  The operator set is a sum type, so every operator is handled exhaustively
  or the code does not compile.
- Reading is an unfold with a depth bound, so a hostile, deeply nested rule
  is rejected before anything evaluates it.
- Evaluation is a fold over that tree. Its carrier is a function of the
  data, which is what lets `map`, `filter` and `reduce` rerun a sub-rule per
  element, and lets `if`, `and` and `or` short-circuit with no extra code.
  It runs in a small state-passing monad that carries the step budget, so
  it is pure and needs no `IO`.
- JsonLogic is defined by its JavaScript reference, so mixed-type behaviour
  (`"1" == 1`, `[] == 0`, `"10" < "9"`) is JavaScript's, including its
  UTF-16 strings and signed zero. That table is written out plainly in
  `JsonLogic.Core.Value` rather than abstracted.

The supported API is the `JsonLogic` module. `JsonLogic.Core.*` is exposed
for advanced use and may change before 1.0 ([`docs/STABILITY.md`](docs/STABILITY.md)).

Design and decisions: [`VISION.md`](VISION.md). Engineering rules this
codebase follows: [`ENGINEERING_STANDARD.md`](ENGINEERING_STANDARD.md).

## How it is tested

- **The classic suite.** All 278 cases of the shared JsonLogic test suite
  ([jsonlogic.com/tests.json](https://jsonlogic.com/tests.json)) must pass.
  The runner fails the build if the fixture is missing, truncated, or has any
  other number of cases, so it cannot pass by running nothing.
- **An outlier catalogue replayed offline.** `test/outliers.jsonl` holds
  about 26,000 deliberately awkward rules and inputs with the answers the
  reference gave: type coercion, UTF-16 and non-BMP strings, signed zero,
  NaN and infinities, the full range of string-to-number forms (hex, octal,
  binary, every Unicode space, 700-digit literals), shortest-digit printing
  ties, odd arities of every operator, awkward variable paths and data,
  short-circuiting, and deep and wide rules. It is generated with no
  randomness by `differential/outliers.js`, and CI fails if the committed
  file is stale. A 138,000-case tier of the same catalogue runs nightly.
  Every case runs in both arity modes. Needs no Node: `cabal test` runs it.
- **A differential test against the real thing.** It generates random rules
  over every builtin operator, feeds them awkward values, runs each through
  this library and through
  [json-logic-js](https://github.com/jwadhams/json-logic-js) 2.0.5 via Node,
  and on a mismatch shrinks the rule to a minimal one and writes a
  reproduction. CI runs a fresh seed on every push and a larger run nightly.
- **Unit and property tests**: boundary tests for every resource limit,
  amplification rules that must finish quickly, laws (De Morgan, `!!`),
  UTF-16 round trips, and a check that every number written out reads back
  as the same double.

The harnesses were checked by mutation: planting a bug in string ordering or
in whitespace handling makes the outlier suite and the differential test
fail.

What this does not show: it is as good as the catalogue and the generator
are. `docs/COMPATIBILITY.md` lists every known difference.

## Deliberate differences from the reference

Listed with reasons and the test that pins each in
[`docs/COMPATIBILITY.md`](docs/COMPATIBILITY.md). In short:

- In strict mode (the default) a wrong number of arguments is a parse error;
  lenient mode matches the reference.
- Arrays and objects are never equal under `==` / `===`.
- Only own properties of the data are visible (`constructor` and
  `__proto__` are not found on `{}`).
- Data is never executed as rules by `missing` / `missing_some`.
- A string slice through a surrogate pair gives U+FFFD.
- `log` returns its argument and does not print anything.
- Custom operator names are matched exactly; builtin names are reserved.
- Resource limits exist; the reference has none.

## Building and testing

```
cabal build
cabal test
```

`cabal test` runs the unit and property tests, the conformance suite
(`test/tests.json`, a copy of the shared suite; check its licence before
redistributing) and the outlier suite.

The repository's `cabal.project` turns on flags that the released package
does not: `werror` (warnings are errors), `differential` (builds the
differential-test executable, which needs Node.js; see
[`differential/README.md`](differential/README.md)) and `bench` (builds the
benchmarks).

Other checks: `bash scripts/lint.sh` (hlint and fourmolu),
`bash scripts/check-harness.sh` (the runners fail when they should, and the
outlier fixture is current).

## Licence

Mozilla Public License 2.0. See [`LICENSE`](LICENSE).
