# jsonlogic-hs

A native Haskell implementation of [JsonLogic](https://jsonlogic.com): a
small, closed rule language that is itself JSON, so rules can be stored,
shared between systems, and evaluated against data without running
arbitrary code.

Author: Sabrinathan Nair

**Status: pre-release, not yet on Hackage.** It passes all 278 cases of the
classic shared test suite ([jsonlogic.com/tests.json](https://jsonlogic.com/tests.json)),
plus its own unit and property tests.

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
(`ParseFailure`) or could not be evaluated (`EvalFailure`).

### Custom operators

Operators outside the builtin set are supplied by the caller, at the one
place that needs them:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value (..))
import JsonLogic

doubling :: Operators
doubling = operatorsFromList [(OperatorName "double", run)]
  where
    run _ [Number n] = Right (Number (n * 2))
    run _ _ = Left "expected one number"

-- applyWith doubling rule input
```

An operator receives the data being evaluated against and its
already-evaluated arguments. An unknown operator is an `EvalFailure`, not a
silent `null`.

## How it works

- A rule is read from JSON into a closed tree (`RuleF`, tied with `Fix`).
  The operator set is a sum type, so every operator is handled exhaustively
  or the code does not compile.
- Reading is an unfold with a depth bound (default 64), so a hostile,
  deeply nested rule is rejected before anything evaluates it.
- Evaluation is a fold over that tree. Its carrier is a function of the
  data, which is what lets `map`, `filter` and `reduce` rerun a sub-rule per
  element, and lets `if`, `and` and `or` short-circuit with no extra code.
- JsonLogic is defined by its JavaScript reference, so mixed-type behaviour
  (`"1" == 1`, `[] == 0`, `"10" < "9"`) is JavaScript's. That table is
  written out plainly in `JsonLogic.Core.Value` rather than abstracted.

Design and decisions: [`VISION.md`](VISION.md). Engineering rules this
codebase follows: [`ENGINEERING_STANDARD.md`](ENGINEERING_STANDARD.md).

## Deliberate differences from the reference

Each is recorded with its reason in `VISION.md` §11.

- A wrong number of arguments is a parse error; the reference silently
  ignores extras and pads missing ones. (`{"var": []}` is accepted, as the
  suite requires.) `reduce` needs all three arguments.
- Arrays and objects are never equal under `==` / `===`; the reference
  compares them by reference identity, which parsed JSON does not have.
- Strings are ordered and indexed by code point, not UTF-16 code unit.
- `NaN` and infinities are real during evaluation and become `null` in the
  result, as `JSON.stringify` does.
- `log` returns its argument and does not print anything.
- Custom operator names are matched exactly; dotted names are not special.

## Building and testing

```
cabal build
cabal test
```

`cabal test` runs the unit and property tests and the conformance suite
(`test/tests.json`, a copy of the shared suite; check its licence before
redistributing).
