# Architecture

jsonlogic-hs reads a JSON rule into a closed tree, compiles the tree once into
a function of the data, and runs that function in a small pure monad that
counts work. This page explains each stage, the modules that implement it, and
why it is built this way.

```
            parse (unfold)               compile (fold)                 run
  Value ───────────────────▶ Rule ────────────────────▶ Program ──────────────────▶ Value
  JSON      depth, arity,    tree of Node,  literals,    Val -> Run Val   step budget,
            locations        each with its  custom ops,                   limits, errors
                             JSON Pointer   var paths                      with locations
```

## Modules

The dependency graph is acyclic and points inward; nothing depends on
`JsonLogic`, the public facade.

| Module | Responsibility |
| --- | --- |
| `JsonLogic` | The supported API: `apply`, `compile`, `run`, `dataUse`, configuration, errors, locations. |
| `JsonLogic.Core.Operator` | The closed set of builtin operators and their names. The only list of them. |
| `JsonLogic.Core.Location` | JSON Pointers (RFC 6901) into a rule. |
| `JsonLogic.Core.Errors` | The closed error vocabulary, one type per stage, and their rendering. |
| `JsonLogic.Core.Config` | Limits and arity mode. |
| `JsonLogic.Core.Rule` | `RuleF`, one layer of the rule tree; `Node`, a layer with its location; `Rule = Fix Node`. |
| `JsonLogic.Core.Parse` | JSON to `Rule`: the unfold, the depth bound, and each operator's operand shape (`shapeOf`), which drives both arity modes. |
| `JsonLogic.Core.Value` | `Val`, JSON plus `NaN`, the infinities, `-0` and `undefined`; JavaScript truthiness, conversions, equality and ordering. |
| `JsonLogic.Core.Number` | Reading and writing JavaScript numbers exactly; `%`, `Math.max`, `Math.min`. |
| `JsonLogic.Core.Ryu` | Ryū, the shortest round-tripping decimal of a double. |
| `JsonLogic.Core.Utf16` | JavaScript's UTF-16 string length, indexing, slicing and ordering over `Text`. |
| `JsonLogic.Core.Custom` | The registry of caller-supplied operators. |
| `JsonLogic.Core.Eval` | The fold that compiles a `Rule` to a `Program`, the `Run` monad, and every operator's semantics. |
| `JsonLogic.Core.Analysis` | `dataUse`: which data paths a rule can read. |

## Reading: an unfold

`parseRuleWith` is `unfoldFixM step seed`. A seed is a piece of JSON still to
read, how deeply it is nested, and its location. `step` produces one `Node`
whose children are further seeds:

- an object with exactly one key is an operator call; its operands are the
  list under the key (a bare value is a list of one), at locations
  `/key/0`, `/key/1`, ... (or `/key` for the bare value);
- an array is a node whose elements are rules, at `/0`, `/1`, ...;
- anything else is a literal, and its own nesting counts towards the depth
  limit, so a deeply nested object cannot be smuggled through as data.

The depth bound is checked on every seed before any of its children exist,
so a hostile rule is refused after reading at most the limit's depth of it.

Each builtin's operands are described once, in `shapeOf`: the counts strict
mode accepts, how many leading operands the reference reads by position, and
whether it evaluates operands past the last one it uses. Strict mode checks
the count against the shape. Lenient mode repairs it as the reference would:
missing operands become `undefined` (an empty `and`), or `null` for
`reduce`'s initial value; surplus operands are dropped, or kept in a `Discard`
node when the reference evaluates them for their side effects (that is, for
whether they fail). A builtin's constructor is shaped so that a wrong count
cannot be built at all: `Lt a a (Maybe a)`, `Sub a (Maybe a)`.

## Compiling: a fold

`compileRule` is `foldFix compileNode`. Its carrier is not a value but a
function from the datum to a computation (`Val -> Run Val`). That is what lets
`map`, `filter`, `reduce`, `all`, `some` and `none` run a sub-rule once per
element against a different datum, and what makes `if`, `and` and `or`
short-circuit for free: a branch not taken is a function never called.

Everything that does not depend on the data happens in the fold, once:
literals are converted to `Val`, custom operators are looked up in the
registry, and a `var` whose path is a literal has the path split at dots.
Children are compiled lazily and shared, so a branch no run reaches is never
compiled.

Operand order follows the reference: every operand is evaluated, left to
right, before its operator runs, except for `if`, `and`, `or` and the
iterating operators' bodies.

## Running: a counted, pure computation

`Run a` is `Int -> Result a`, where the `Int` is the remaining step budget and
`Result` is either a value with the budget left or a failure with its
location. It is a state monad and an error monad in one, written out by hand
so that a step costs one allocation; there is no `IO` and no global state.

Every node charges one step before its own work. Work proportional to the
size of something is charged in proportion: converting an array to a string
or number, building a string with `cat` or an array with `merge`, walking a
string to index or measure it, and taking in a custom operator's result. So
total work is bounded by `maxSteps` times a constant, whatever the rule
(`docs/LIMITS.md`).

## Values: JavaScript, written out plainly

`Val` is JSON plus what a JavaScript evaluation can produce and JSON cannot
say: `NaN`, the infinities, negative zero and `undefined`. Arrays are
`Vector`s (constant-time index and length), objects are aeson's `KeyMap`.

The coercion rules (`==`, `<`, `ToNumber`, `parseFloat`, `String()`) are an
arbitrary table fixed by JavaScript, so they are written as one plain clause
per case of the ECMAScript algorithms, not abstracted. Numbers are read with
Clinger's exact fast path or, failing that, one correctly rounded integer
division; they are written with Ryū, whose shortest-digits answer is the one
ECMAScript specifies. Both are checked against slow exact implementations on
hundreds of thousands of doubles, and the printing against V8 directly.

## Errors

One closed type per stage: `ParseError` (depth, arity) and `EvalError`
(unknown operator, a custom operator's failure, an operand the reference
throws on, a limit). `Error` pairs either with the `Location` of the node that
failed. JavaScript semantics make most odd inputs produce `null`, `false` or
`NaN` rather than fail, so evaluation errors are rare by design.

## Extension: one open point

The builtin set is closed: `RuleF` is a sum type matched exhaustively, so an
operator cannot be half-implemented. Operators supplied by the caller are the
one open extension point, `CustomOp`, resolved against an `Operators` registry
at compile time. Their contract (eager, evaluated operands, JSON in and out,
results held to the limits) matches how the reference treats added
operations.

## Testing as part of the design

Each stage has its own tests (`ParseSpec`, `ValueSpec`, `NumberSpec`,
`EvalSpec`, `LimitsSpec`, `LocationSpec`, `AnalysisSpec`), and the whole
pipeline is held to the reference by four independent bodies of evidence:
the classic suite, the outlier catalogue, the differential test and the
community suite (`docs/CONFORMANCE.md`). The runners are themselves tested
(`scripts/check-harness.sh`).

## Decisions

The reasons behind choices that depart from the engineering standard, or
that a reader might question, are recorded in `docs/adr/`.
