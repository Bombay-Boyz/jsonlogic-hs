# Using these fixtures to test any JsonLogic implementation

The fixtures in this repository are plain JSON, independent of Haskell. If you
implement JsonLogic in another language and want to behave like the
reference, json-logic-js 2.0.5, you can run them against your implementation
without touching any Haskell. This page states their format exactly and how
to judge an answer.

| Fixture | Cases | What it checks |
| --- | --- | --- |
| `test/tests.json` | 278 | The classic shared suite from jsonlogic.com |
| `test/outliers.jsonl` | 26,980 | Awkward rules with json-logic-js 2.0.5's own answers |
| full outlier tier | about 140,000 | The same catalogue, unsampled: `OUTLIER_TIER=full node differential/outliers.js out.jsonl` |
| `test/community/` | 1,138 | The community suite, scored as the compatibility tables score it |

## The classic suite (`test/tests.json`)

A JSON array. A string is a section comment. Every other entry is a
three-element array `[rule, data, expected]`: applying `rule` to `data` must
give exactly `expected`. All 278 must pass.

## The outlier catalogue (`test/outliers.jsonl`)

One JSON object per line, UTF-8, LF line endings.

The first line is `{"meta":{"cases":N,"oracle":"2.0.5","tier":"core"}}`. A
runner should check that exactly `N` lines follow, so that a truncated file
fails instead of passing quietly.

Every other line is one case:

```json
{"id":"pair-009843","fam":"pair","op":"<","n":2,"rule":{"<":[0,0.5]},"data":{"a":1,"b":[1,2]},"expect":{"ok":true}}
```

| Field | Meaning |
| --- | --- |
| `id` | Stable within one version of the generator; family name and a counter. |
| `fam` | The family: `arity`, `unary`, `pair`, `triple`, `numstr`, `numfmt`, `tie`, `signedzero`, `unicode`, `var`, `collection`, `flow`, `literal`, `deep`, `wide`, `coercion`, `community`. |
| `op` | The outermost operator (or `lit`). |
| `n` | The number of operands written. |
| `rule`, `data` | What to evaluate. Numbers are written as the generator wrote them, so a rule may contain tokens such as `1e309` or `-0.0`; read them with your JSON parser as JavaScript's `JSON.parse` would. |
| `expect` | `{"ok": value}`, the reference's result, or `{"error": message}`, the message of what it threw. |
| `dev` | Present only on the few cases that exercise a documented deviation of this library (below). |
| `expect_own` | With `dev: "inherited"`: the answer for a path that is certainly absent. |

How the expected values were written: JavaScript's `JSON.stringify` of the
result, so `NaN`, `Infinity` and `undefined` appear as `null`, negative zero
appears as `0`, and an object property whose value is `undefined` is left out.
A lone surrogate in a result string is written as U+FFFD
(`String.prototype.toWellFormed`).

Judging an answer:

- `{"ok": v}`: your result, written as JSON the same way, must equal `v`
  exactly (numbers compared as doubles).
- `{"error": m}`: your implementation must fail. The messages are V8's
  (`Unrecognized operation x`, `Cannot read properties of null (reading
  'length')`, `Reduce of empty array with no initial value`); match on the
  kind of failure, not on the text.

Cases tagged `dev` are where an implementation may reasonably choose
differently from the reference:

- `"inherited"`: the rule looks up a name such as `constructor` that
  JavaScript finds on the prototype chain of a plain object. Use
  `expect_own` if your implementation sees only a value's own properties, as
  this one does.
- `"reeval"`: the reference re-runs a key that came from the data as a rule.
  This library skips these two cases.

The families cover: every operator at 0 to 4 operands, as a list and as a
bare operand; every operator over pools of awkward values; string-to-number
conversion in fifteen contexts (hex, octal, binary, `Infinity`, every Unicode
space, 700-digit literals); number printing and shortest-digit ties;
signed zero, `NaN` and the infinities through every operator; UTF-16 length,
indexing, `substr` and ordering, including astral characters; `var` paths
on awkward data; iteration over awkward collections; control flow and
short-circuiting (including operands that throw only when evaluated); a full
loose and strict equality and ordering matrix; literals; deep and wide rules;
and every rule of the community suite.

An implementation that matches the reference on all of these, in a mode that
pads missing operands with `undefined` and evaluates but ignores surplus
ones, matches it more closely than any public test suite asks.

## The community suite (`test/community/`)

See `test/community/README.md`. Each file is a JSON array of comments
(strings) and cases `{"description", "rule", "data"?, "result" | "error"}`;
`index.json` lists the files. It is scored the way
[compat-tables](https://github.com/json-logic/compat-tables) scores
implementations: an expected `null` accepts any falsy result, numbers are
equal within 1e-10, arrays and objects are compared element by element, and
an expected error `{"type": T}` passes when the implementation fails with an
error whose name contains `T`. `test/community/expected.json` records this
library's per-file score in both arity modes. json-logic-js 2.0.5 scores
782 of 1,138; an implementation of the JSON Logic 2.0 dialect should score
far higher.

## Regenerating

```
cd differential && npm ci                 # installs exactly json-logic-js 2.0.5
node outliers.js ../test/outliers.jsonl   # the committed tier
node outliers.js --check ../test/outliers.jsonl
```

The generator has no randomness. It refuses to run against any other version
of json-logic-js, because a different oracle changes what "correct" means.
