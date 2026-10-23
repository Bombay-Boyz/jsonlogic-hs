# Differential testing against the reference

Two tools, both holding this library to the real JavaScript implementation
([json-logic-js](https://github.com/jwadhams/json-logic-js) **2.0.5**, run
through Node):

- **`outliers.js`** writes the outlier catalogue: a large, fixed set of
  deliberately awkward rules and inputs, with the answers the reference
  gave. The result is committed as `test/outliers.jsonl` and replayed
  *offline* by `cabal test` (`jsonlogic-hs-outliers`).
- **`Differential.hs`** generates random rules and compares each answer with
  the reference live, shrinking any disagreement.

## Setup (once)

```
cd differential && npm ci
```

Needs Node.js 18 or newer. `npm ci` installs exactly the locked oracle
(`2.0.5`); `oracle.js` and `outliers.js` refuse to run against any other
version, because a different oracle changes what "correct" means. Neither is
part of `cabal test`, which must work without Node.

## The outlier catalogue

```
node outliers.js ../test/outliers.jsonl              # write the committed sample (about 27,000 cases)
OUTLIER_TIER=full node outliers.js /tmp/full.jsonl   # the whole catalogue (about 140,000 cases)
node outliers.js --check ../test/outliers.jsonl      # fail if the committed file is stale
cabal run jsonlogic-hs-outliers -- /tmp/full.jsonl   # replay any tier
```

There is no randomness: the same script always writes the same bytes, and
the sample is a stable hash-based thinning of the big cross-product families,
so it only changes when the catalogue does. Regenerate the committed file
whenever `outliers.js` changes; CI fails if you forget.

Families (see the top of `outliers.js`): every operator at argument counts 0
to 4 (`arity`); every operator over a rich pool of awkward values (`unary`,
`pair`, `triple`); string-to-number forms through fifteen contexts
(`numstr`); number printing (`numfmt`) and shortest-digit ties (`tie`);
signed zero, NaN and infinity through every operator and observer
(`signedzero`); UTF-16 length, indexing, `substr`, ordering (`unicode`);
variable paths on awkward data (`var`); iteration operators over awkward
collections and bodies (`collection`); control flow and short-circuiting
(`flow`, including an empty `*` in branches that are and are not taken); a
full loose/strict equality and ordering matrix over mixed types
(`coercion`); literals, deep nesting and very wide rules; and every rule of
the community suite in `test/community/` (`community`). The format is
documented for implementers in other languages in `docs/CONFORMANCE.md`.

Each line is one case: `{"id","fam","op","n","rule","data","expect"}`, where
`expect` is `{"ok":value}` or `{"error":message}`. The first line is a
`meta` line stating how many cases follow, so truncation is detected. A case
that exercises a documented deviation carries `"dev"`:

- `"inherited"`: the rule looks up a name like `constructor` that JavaScript
  finds on the prototype chain. `expect_own` is the reference's answer for a
  path that is certainly absent, which is what this library must return.
- `"reeval"`: `missing` / `missing_some` was given a key that came from the
  data and looks like a rule. Counted, not compared.

Both deviations are explained in `docs/COMPATIBILITY.md`.

## The random differential test

```
cabal run jsonlogic-hs-differential -- COUNT SEED
cabal run jsonlogic-hs-differential -- --replay FILE
```

Defaults: 2000 cases, seed 1. The seed is printed first; the same count and
seed always generate the same cases. CI uses a fresh seed on every run (the
run number) and a larger count nightly.

A disagreement is shrunk, by asking the reference about simpler variants, to
a small rule that still disagrees. The shrunk case, the original, both
answers (lenient and strict) and the reference's are printed, and the shrunk
case is written to `differential/out/repro-N.jsonl`, which `--replay`
re-checks. Every mismatch index is listed in `differential/out/mismatches.txt`.

Cases are compared in lenient arity mode, which models the reference, and
strict mode must give the same answer unless it rejects the arity. The
generator produces every operator at every operand count it accepts,
including an empty `*`, which the reference throws on only when it evaluates
it. Errors are compared by class (unknown operator, invalid operand, empty
`*`); a
reference error of any other kind fails the run, so a new kind of failure
cannot pass unnoticed.

## Reading a mismatch

Generated rules avoid the documented deviations (docs/COMPATIBILITY.md): no
array or object is put in the data where the reference would compare it by
identity, and values JSON cannot write (negative zero, NaN, infinities,
`undefined`) appear only in rules, as computed values. A mismatch is
therefore either a bug here or a new deviation to decide on and record.

## Checking the checkers

`bash scripts/check-harness.sh` feeds the runners missing, empty, truncated
and tampered fixtures and demands a failure each time, and checks the
committed fixture is current. Planting a bug in string ordering or in
whitespace handling must make both the outlier suite and the differential
test fail; when changing the harness, do that once by hand.
