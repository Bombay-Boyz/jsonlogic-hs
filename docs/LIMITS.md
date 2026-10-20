# Resource limits

A JsonLogic rule is data, so rules often come from users. The default
`Config` makes it safe to evaluate them: every way a rule can make the
evaluator do unbounded work or allocate unbounded memory is bounded.

```haskell
import JsonLogic

-- defaults: depth 64, 1,000,000 steps, 1,048,576-unit strings,
--           100,000-element arrays, strict arity
applyWithConfig defaultConfig noOperators rule input

-- tighter, for rules from the open internet
applyWithConfig defaultConfig {maxSteps = 10000, maxStringLength = 4096} noOperators rule input
```

| Limit | Default | Counted in | Error |
| --- | --- | --- | --- |
| `maxDepth` | 64 | levels of nesting of the rule, of any literal in it, and of a custom operator's result | `ParseFailure (DepthExceeded n)` or `EvalFailure (LimitExceeded (ValueDepthLimit n))` |
| `maxSteps` | 1,000,000 | see below | `EvalFailure (LimitExceeded (StepLimit n))` |
| `maxStringLength` | 1,048,576 | UTF-16 code units, the longest string `cat` builds | `LimitExceeded (StringLimit n)` |
| `maxArrayLength` | 100,000 | elements, the longest array `merge` builds and the longest `map` or `filter` iterates | `LimitExceeded (ArrayLimit n)` |

Each limit is exact: a rule that needs exactly the limit succeeds and one
more fails, and the tests check both sides of every boundary.

## What a step is

A step is one rule node evaluated, plus: one extra step per element of an
array operand that an operator converts to a string or number (so comparing a
big array a thousand times is paid for), one per element or code unit that
`cat` or `merge` builds, and one per node of a value a custom operator
returns. Walking a path into the data is paid for too: a `var`, `missing`
or `missing_some` path costs one step per array element it passes over and
one per code point of a string it indexes or measures, so a lookup into a
large array is charged for the work it does. Iteration (`map`, `filter`, `reduce`, `all`, `some`, `none`) is
charged through the nodes its body evaluates. Because building a result costs
steps in proportion to its size, the total work of an evaluation is bounded
by `maxSteps` up to a constant factor, even for rules designed to amplify
(nested maps over ten elements, a `reduce` that doubles a string, a rule
with two million operands). These cases are tests, and each finishes in well
under a second.

## What is not bounded

- **Wall-clock time.** The limits bound work, not seconds. Run untrusted
  evaluations with a timeout (`System.Timeout.timeout`) as well if that
  matters; evaluation is pure and can be interrupted safely.
- **A custom operator's own work.** The library cannot see inside it. Its
  *result* is held to the depth and step limits.
- **The size of the input.** The rule and the data are values you already
  hold. Parsing JSON text is aeson's job, and it has no depth limit of its
  own: bound the size of untrusted text before decoding it.
- **Searching or slicing a very large string.** `in`, `substr` and string
  comparison take time linear in the string, which comes from the data you
  hold; the string limit bounds what `cat` builds, not what the data holds.
- **Reading a long numeric string.** It is linear in its length (a
  million-digit string takes about 0.15 s), not bounded by a step.

## Cost

The limits are not free. Against the previous version, on one machine,
iteration-heavy rules (`map`, `filter`, `reduce` over 10,000 elements) take
roughly 15 to 45 per cent longer, and a tiny rule applied once takes about
twice as long (about 2 microseconds instead of 1). To apply one rule many
times, `compile` it once and `run` it: that skips reading it again.

`cabal run jsonlogic-hs-bench` (build with `-f bench`) times a fixed set of
workloads; compare two runs on the same machine.
