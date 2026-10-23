# jsonlogic-hs

A native Haskell implementation of [JsonLogic](https://jsonlogic.com), the
small rule language that is itself JSON, so rules can be stored, shared
between systems and evaluated against data without running arbitrary code.

It is held to the JavaScript reference implementation,
[json-logic-js](https://github.com/jwadhams/json-logic-js) 2.0.5, more
closely than any other implementation we know of, and it is safe to run on
rules from an untrusted source.

Author: Sabrinathan Nair. Licence: [MIT](LICENSE).
**Status: release candidate, not yet on Hackage.**

| | |
| --- | --- |
| Classic shared suite (jsonlogic.com/tests.json) | 278 / 278 |
| Outlier catalogue, answers recorded from json-logic-js | 26,980 / 26,980 (lenient mode) |
| Random differential test against json-logic-js | every CI run, fresh seed |
| Community suite (json-logic/compat-tables) | 782 / 1138 lenient, 839 / 1138 strict |
| Haddock coverage | 100% of every module |

The community suite is mostly written for the newer JSON Logic 2.0 dialect.
In lenient mode this library scores exactly what json-logic-js scores, file
for file, because it implements that reference's dialect; see
[What it is not](#what-it-is-not).

## Quick start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (decode)
import JsonLogic (apply)

main :: IO ()
main = print (apply <$> decode "{\"==\": [{\"var\": \"a\"}, 1]}" <*> decode "{\"a\": 1}")
-- Just (Right (Bool True))
```

`apply rule data` takes both as aeson `Value`s and returns
`Either Error Value`. [`docs/TUTORIAL.md`](docs/TUTORIAL.md) walks through
everything else: compiling once, configuring limits, custom operators,
reading errors, and finding out what a rule reads.

## The API in one screen

```haskell
apply           :: Value -> Value -> Either Error Value
applyWith       :: Operators -> Value -> Value -> Either Error Value
applyWithConfig :: Config -> Operators -> Value -> Value -> Either Error Value

compile :: Config -> Operators -> Value -> Either Error Rule   -- read, check, build once
run     :: Rule -> Value -> Either Error Value                 -- run many times
dataUse :: Rule -> DataUse                                     -- what it can read

operatorsFromList :: [(OperatorName, CustomOperator)] -> Either OperatorsError Operators
type CustomOperator = Value -> [Value] -> Either Text Value

data Error = ParseFailure Location ParseError | EvalFailure Location EvalError
renderLocation :: Location -> Text                             -- an RFC 6901 JSON Pointer
```

Everything is pure: no `IO`, no global state. The supported API is the
`JsonLogic` module; `JsonLogic.Core.*` is exposed for advanced use and may
change before 1.0 ([`docs/STABILITY.md`](docs/STABILITY.md)).

## What you can rely on

- **The reference's answers.** Mixed-type behaviour is JavaScript's:
  `"1" == 1`, `[] == 0`, `"10" < "9"`, `NaN`, the infinities, negative zero,
  `undefined` (what an empty `and` yields, which is not `null`), UTF-16 string
  length, indexing and ordering, ECMAScript whitespace in numeric strings, and
  numbers printed digit for digit as JavaScript prints them. Every place it
  deliberately differs is listed with a reason and a test in
  [`docs/COMPATIBILITY.md`](docs/COMPATIBILITY.md).
- **Bounded work.** The default `Config` limits rule depth (64), evaluation
  steps (1,000,000), string length (1 MiB in UTF-16 code units) and array
  length (100,000), so a hostile rule cannot hang the process or exhaust its
  memory. Each limit is exact at both edges ([`docs/LIMITS.md`](docs/LIMITS.md)).
- **Errors you can act on.** A closed set of constructors to match on, each
  with the JSON Pointer of the node that failed:
  `renderError` gives `at /if/1: unknown operator "nope"`.
- **Strict or lenient arity.** By default a wrong argument count is a precise
  `ArityMismatch` (the reference silently pads and ignores). With
  `arityMode = Lenient` the library behaves exactly like the reference.
- **Custom operators** supplied by the caller, refused if they would shadow a
  builtin, resolved once when the rule is compiled, and held to the same
  limits.
- **Speed.** Interpretation is in the same range as json-logic-js running in
  V8 on the same machine (`map` over 10,000 elements: about 2.5 ms here,
  2.1 ms for the reference), with `compile` once and `run` many.

## How it is checked

Every claim above has a mechanical check behind it, and the checks are
checked: [`scripts/check-harness.sh`](scripts/check-harness.sh) feeds every
runner missing, empty, truncated and tampered fixtures and demands that each
fails.

- **The classic suite**, pinned at exactly 278 cases.
- **The outlier catalogue** (`test/outliers.jsonl`): rules and data chosen to
  be awkward (type coercion, every whitespace code point, 700-digit numbers,
  shortest-digit ties, signed zero, surrogate pairs, odd arities of every
  operator, deep and wide rules, every community rule), with the answers
  json-logic-js gave. Generated deterministically by
  `differential/outliers.js`; CI fails if the committed file is stale. Every
  case runs in both arity modes. A 140,000-case tier runs nightly.
- **A differential test** that generates random rules, compares each answer
  with json-logic-js running in Node, and shrinks any disagreement to a
  minimal reproduction.
- **The community suite**, scored per file and pinned in
  `test/community/expected.json`, so the score only changes on purpose.
- **Unit and property tests**: boundary triples for every limit,
  amplification rules that must stop quickly, algebraic laws, error locations
  that always resolve to the operator at fault, the fast numeric algorithms
  against exact ones on hundreds of thousands of doubles, and `dataUse`'s
  guarantee.
- **Hygiene, all blocking in CI**: `-Werror` on the primary GHC, a matrix of
  GHC 9.6 to 9.14 plus macOS and Windows, hlint, fourmolu, weeder (no dead
  code), shellcheck, actionlint, `cabal check`, a build of the source
  distribution on its own, and a downstream project that uses only the public
  module.

Implementing JsonLogic in another language? The fixtures are plain JSON and
JSONL with a documented format; [`docs/CONFORMANCE.md`](docs/CONFORMANCE.md)
explains how to run them against your implementation.

## What it is not

- **Not the JSON Logic 2.0 dialect** (`val`, `??`, `exists`, `try`/`throw`,
  `NaN` as an error). That dialect is measured by the community suite and
  tracked on the roadmap, not implemented.
- **Not a sandbox for custom operators.** The library bounds its own work and
  a custom operator's result, not the operator's own computation.
- **Not a wall-clock timeout.** Limits bound work; add
  `System.Timeout.timeout` if seconds matter.

## Building and testing

```
cabal build
cabal test all            # unit/property, classic suite, outliers, community score
```

The repository's `cabal.project` turns on what released builds do not:
`-Werror`, the differential tester (needs Node.js; see
[`differential/README.md`](differential/README.md)) and the benchmarks.

| Command | What it checks |
| --- | --- |
| `bash scripts/lint.sh` | hlint, fourmolu, fixture hashes, shellcheck |
| `bash scripts/weeder.sh` | dead code |
| `bash scripts/check-harness.sh` | the test runners fail when they should; the outlier fixture is current |
| `bash scripts/verify-sdist.sh` | the package as Hackage would receive it |
| `bash scripts/consumer-smoke.sh` | a separate project using only `JsonLogic` |
| `bash scripts/bench.sh` | benchmarks, with their environment ([`bench/README.md`](bench/README.md)) |

## Further reading

- [`docs/TUTORIAL.md`](docs/TUTORIAL.md): using the library, step by step.
- [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md): how it is built, and why.
- [`docs/COMPATIBILITY.md`](docs/COMPATIBILITY.md): every difference from the reference.
- [`docs/LIMITS.md`](docs/LIMITS.md): resource limits and what a step is.
- [`docs/CONFORMANCE.md`](docs/CONFORMANCE.md): using these fixtures to test any implementation.
- [`docs/STABILITY.md`](docs/STABILITY.md): what is covered by the versioning policy.
- [`docs/AUDIT-2026-10.md`](docs/AUDIT-2026-10.md): the external audit's findings and how each was resolved.
- [`docs/adr/`](docs/adr/): recorded decisions and deviations from the engineering standard.
- [`VISION.md`](VISION.md) and [`ENGINEERING_STANDARD.md`](ENGINEERING_STANDARD.md): the design and the rules this codebase follows.

## Licence

MIT. See [`LICENSE`](LICENSE). The vendored test suites keep their own
licences: `test/community/` is Apache-2.0 (its `LICENSE` is included);
`test/tests.json` is the shared classic suite from jsonlogic.com.
