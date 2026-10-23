# Tutorial

This walks through jsonlogic-hs from a first rule to running untrusted rules
in production. Every example here is compiled and checked by
`tutorial/Tutorial.hs` (`cabal test jsonlogic-hs-tutorial`, part of `cabal test all`), so the
answers shown are the library's real answers.

The examples use these imports:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (Value (..), decode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import JsonLogic

-- | JSON from a string literal (examples only: use 'decode' properly in code).
json :: String -> Value
json = fromMaybe Null . decode . Char8.pack
```

## 1. A rule is JSON

A rule is an object with one key, the operator, whose value is the list of
operands. Operands can themselves be rules. `var` reads from the data.

```haskell
apply (json "{\"and\": [{\">=\": [{\"var\": \"age\"}, 18]}, {\"var\": \"verified\"}]}")
      (json "{\"age\": 21, \"verified\": true}")
-- Right (Bool True)
```

Anything that is not a one-key object is a literal: `5`, `"x"`, `null`, and
objects with several keys evaluate to themselves; arrays are evaluated element
by element.

```haskell
apply (json "[{\"var\": \"a\"}, 2]") (json "{\"a\": 1}")
-- Right (Array [Number 1.0,Number 2.0])
```

Paths are dotted, and numbers index arrays:

```haskell
apply (json "{\"var\": \"user.roles.0\"}") (json "{\"user\": {\"roles\": [\"admin\"]}}")
-- Right (String "admin")
apply (json "{\"var\": [\"missing.path\", \"default\"]}") (json "{}")
-- Right (String "default")
```

## 2. JavaScript semantics, on purpose

JsonLogic is defined by its JavaScript reference, so comparisons and
arithmetic coerce the way JavaScript does. This surprises people; it is also
what every other implementation they share rules with does.

```haskell
apply (json "{\"==\": [1, \"1\"]}") Null      -- Right (Bool True)
apply (json "{\"===\": [1, \"1\"]}") Null     -- Right (Bool False)
apply (json "{\"<\": [\"10\", \"9\"]}") Null  -- Right (Bool True): two strings compare as strings
apply (json "{\"+\": [\"3\", 4]}") Null       -- Right (Number 7.0)
apply (json "{\"/\": [1, 0]}") Null           -- Right Null: Infinity has no JSON form
apply (json "{\"cat\": [0.1, \"+\", 0.2]}") Null
-- Right (String "0.1+0.2")
```

An empty array is falsy (JsonLogic's one change to JavaScript truthiness),
and `"0"` is truthy:

```haskell
apply (json "{\"if\": [[], \"yes\", \"no\"]}") Null   -- Right (String "no")
apply (json "{\"!!\": \"0\"}") Null                   -- Right (Bool True)
```

`docs/COMPATIBILITY.md` lists the few places the library deliberately differs.

## 3. Compile once, run many times

`apply` reads the rule every time. To evaluate one rule against many inputs,
`compile` it once: that parses it, checks it, resolves custom operators and
builds the evaluator, and `run` only evaluates.

```haskell
case compile defaultConfig noOperators (json "{\"*\": [{\"var\": \"price\"}, 1.2]}") of
  Left err -> putStrLn (show err)
  Right rule -> print (map (run rule) [json "{\"price\": 10}", json "{\"price\": 2.5}"])
-- [Right (Number 12.0),Right (Number 3.0)]
```

A compiled `Rule` is an ordinary immutable value: share it between threads,
keep it in a cache, run it as often as you like.

## 4. Reading errors

Every failure is an `Error` with a stage, a reason you can match on, and the
location of the node that failed, as a JSON Pointer into the rule.

```haskell
apply (json "{\"if\": [true, {\"nope\": [1]}]}") Null
-- Left (EvalFailure (fromSegments [Key "if",Index 1]) (UnknownOperator (OperatorName "nope")))

either renderError (const "") (apply (json "{\"if\": [true, {\"nope\": [1]}]}") Null)
-- "at /if/1: unknown operator \"nope\""
```

Match on constructors, not on `renderError`'s text, which may change:

```haskell
describe :: Either Error Value -> String
describe result = case result of
  Right v -> "ok: " ++ show v
  Left (ParseFailure at (ArityMismatch (OperatorName op) _ given)) ->
    "rule error at " ++ show (renderLocation at) ++ ": " ++ show op ++ " given " ++ show given
  Left (EvalFailure _ (LimitExceeded _)) -> "too expensive"
  Left other -> "failed: " ++ show (renderError other)
```

Two things are not errors in JsonLogic, because they are not in JavaScript:
odd operands produce `null`, `false` or `NaN`-turned-`null`, and an unknown
operator in a branch that is never taken is never looked at:

```haskell
apply (json "{\"or\": [true, {\"nope\": []}]}") Null   -- Right (Bool True)
```

## 5. Strict and lenient arity

By default a wrong number of operands is an error, because it is almost
always a mistake:

```haskell
apply (json "{\"==\": [1]}") Null
-- Left (ParseFailure (fromSegments []) (ArityMismatch (OperatorName "==") (Exactly 2) 1))
```

The reference instead pads missing operands with `undefined` and ignores
extra ones. If you share rules with systems that rely on that, ask for it:

```haskell
applyWithConfig defaultConfig {arityMode = Lenient} noOperators (json "{\"==\": [1]}") Null
-- Right (Bool False)
```

Lenient mode reproduces the reference exactly, including its quirks (a
surplus operand is still evaluated, so if it fails the rule fails).

## 6. Rules from users: limits

A rule is data, so rules often come from people you do not control. The
default `Config` is already safe; tighten it for the open internet:

```haskell
let strict = defaultConfig {maxSteps = 10000, maxStringLength = 4096, maxArrayLength = 1000}
applyWithConfig strict noOperators (json "{\"map\": [{\"var\": \"xs\"}, {\"map\": [{\"var\": \"\"}, 1]}]}") (object ["xs" .= [[1 .. 2000 :: Int]]])
-- Left (EvalFailure (fromSegments [Key "map",Index 1]) (LimitExceeded (ArrayLimit 1000)))
```

What each limit counts, and what is not bounded (wall-clock time, a custom
operator's own work, the size of the input you decoded), is in
`docs/LIMITS.md`. Decode untrusted JSON with a size limit first, and wrap
evaluation in `System.Timeout.timeout` if seconds matter.

## 7. Custom operators

Operators beyond the builtin set are supplied by you. An operator receives
the datum in scope and its operands, already evaluated, as JSON:

```haskell
import qualified Data.Text as Text

startsWith :: CustomOperator
startsWith _ [String prefix, String s] = Right (Bool (prefix `Text.isPrefixOf` s))
startsWith _ _ = Left "starts_with takes two strings"

operators :: Either OperatorsError Operators
operators = operatorsFromList [(OperatorName "starts_with", startsWith)]

-- with Right ops = operators:
applyWith ops (json "{\"starts_with\": [\"ad\", {\"var\": \"role\"}]}") (json "{\"role\": \"admin\"}")
-- Right (Bool True)
applyWith ops (json "{\"starts_with\": [1]}") Null
-- Left (EvalFailure (fromSegments []) (CustomOperatorFailed (OperatorName "starts_with") "starts_with takes two strings"))
```

A registry refuses a builtin name (`ReservedOperatorName`) and a name given
twice (`DuplicateOperatorName`) instead of silently picking one. Registries
combine with `<>`, the left one winning. Operators are resolved when the rule
is compiled; the full contract is on `CustomOperator`.

## 8. What does this rule read?

`dataUse` tells you, without running the rule, which paths of the data it can
read: useful to fetch only those fields, or to know which rules a change to
the data can affect.

```haskell
dataUse <$> compile defaultConfig noOperators
  (json "{\"and\": [{\"var\": \"user.age\"}, {\"some\": [{\"var\": \"orders\"}, {\">\": [{\"var\": \"total\"}, 100]}]}]}")
-- Right (DataUse {usedPaths = ["orders","user.age"], usesAllData = False})
```

The `var "total"` inside `some` reads each order, not the data, so it is not
listed. When `usesAllData` is `False`, the answer is guaranteed to depend on
the data only through the listed paths. A computed path, `{"var": ""}`, or a
custom operator (which is handed the whole datum) sets `usesAllData`.

## Where next

- `docs/COMPATIBILITY.md`: every deliberate difference from the reference.
- `docs/LIMITS.md`: the resource limits in detail.
- `docs/ARCHITECTURE.md`: how the library is built.
- The Haddock documentation of the `JsonLogic` module: the reference for
  every function and constructor.
