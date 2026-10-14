# Project Vision: jsonlogic-hs

**Author:** Sabrinathan Nair
**Status:** Draft v2 — reconciled against *The Haskell Engineering Standard*
**Governing document:** *The Haskell Engineering Standard* (supplied, binding) is
the constitution for this codebase. Wherever this document and the Standard
disagree, **the Standard wins** — this document has already been corrected
against it once (§4, §7 below); any future disagreement discovered during
implementation is resolved the same way, not by amending the Standard.

---

## 1. What this project is

A complete, spec-conformant, natively-Haskell implementation of
[JsonLogic](https://jsonlogic.com) — a small, closed rule language for expressing
conditional logic as JSON, designed to be shared between systems and languages
(feature flags, business rules, permission checks, form conditionals).

JsonLogic is deliberately **not** a general-purpose programming language. It has
no user-defined functions, no variable binding beyond flat `var` lookups into a
data context, and a fixed, closed set of ~35 operators. That boundedness is the
whole reason this is a good fit for a solo hobby project — there is a real,
checkable finish line, not an open-ended surface.

## 2. Why this project exists (the actual gap)

Confirmed directly against Hackage, Stackage, and GitHub, not assumed:

- `jsonlogic` (Hackage, 2022): one release, ever. 17 total downloads. Not on
  Stackage. Not in any distribution channel except NixOS. GitHub repo has 14
  open issues, all from a single burst of activity in early 2022, all still
  open — including unresolved arithmetic bugs, unresolved NaN handling, and
  an unresolved design spike on how errors should even be handled.
- `json-logic-hs` (nammayatri, GitHub only): a second, independent attempt.
  Its own README states it is **not** out-of-the-box interoperable with other
  JsonLogic implementations — rules written elsewhere are not guaranteed to
  work here. 0 stars, no evidence of verification against the official
  conformance suite.

Meanwhile, mature implementations exist in JS (the reference), Rust, Go, Ruby,
PHP, and Python — several of which explicitly verify themselves against a
shared cross-language test suite and advertise the result (Ruby: 601/601;
Go: passes `tests.json` unaltered).

**Correction, found during this project's own research:** that citation
originally conflated two different things, and is stated precisely now.
There are two suites in play:

1. **The classic suite** — `jwadhams/json-logic-js`'s informal `tests.json`,
   the ~35-operator set, and what the "601/601"-style claims above are
   actually made against. This is what `Rule.hs` targets.
2. **A newer, explicitly in-progress community standardization effort**
   (`json-logic/.github`, backed by real users including OpenFeature),
   actively resolving ambiguities the classic spec left open and adding
   operators (`val`, `try`/`throw`, `??`) not yet in scope here. Its own
   test files include a `legacy.json` — the effort itself distinguishes
   old behavior from new, unstabilized behavior.

**Decision (this session): v1 targets the classic suite.** The newer
community effort is tracked, not ignored — see §7 and §10 — but is
explicitly out of v1 scope as still-unstable ground to build a "verified
conformance" claim on.

**The gap is real and specific:** there is no Haskell implementation that is
both complete and verifiably spec-conformant. That is the deliverable.

## 3. What "better than the alternatives" honestly means here

Rust's `json-logic-rs` is a strong, mature implementation. This project does
**not** claim to out-engineer it in the abstract — Rust's `enum` types give it
the same object/array type-safety and exhaustiveness-checking advantages this
project would otherwise claim as unique. That claim was tested and walked back
during design discussion; it does not belong in this document as a selling
point, and should not resurface as one.

What this project can honestly claim:

1. **A native Haskell library where none currently works**, avoiding an FFI
   boundary to Rust for every custom operator a downstream app wants to add.
2. **Verified, provable spec conformance** — the official test suite passed
   and reported, which no existing Haskell attempt has done.
3. **A more elegant *internal* architecture** where the problem domain
   actually has mathematical structure to exploit (see §4) — this is a
   maintainability and correctness claim about *this codebase*, not a
   performance or capability claim over Rust.

## 4. Architectural philosophy

**Use real mathematical structure where the domain has it. Do not force it
where the domain doesn't.**

### Where it applies — the recursion-scheme core

A JsonLogic rule is a tree. Define its shape as a functor, and its recursive
closure via a fixed point. **The authoritative version of this type is
`Rule.hs` (drafted, self-audited, and cross-checked against an independent
reference implementation's rule-class list — see §10); the sketch below is
illustrative only and intentionally simplified (e.g. `Var`'s path position
is shown as static here, but is actually `a` — a computable sub-rule — in
the real definition):**

```haskell
data RuleF a
  = Lit Value
  | Var Path (Maybe a)
  | And [a] | Or [a] | Not a
  | Eq a a | Neq a a | Lt a a | Gt a a | ...
  | If a a a
  | Map a a | Filter a a | Reduce a a a
  | ...
  deriving (Functor, Foldable, Traversable)

type Rule = Fix RuleF   -- Fix, cata, and ana from the `recursion-schemes` package
```

**Corrected from the v1 draft:** the original draft hand-rolled `Fix`/`cata`
to avoid an "unnecessary" dependency. That decision is reversed under the
Standard's own rules — 4.1 requires naming an existing combinator before
writing recursion by hand, and 4.7/2.11 require depending on a proven
library's implementation of a named construct (`Fix`, `cata`, `ana`, `hylo`
are exactly this) rather than re-deriving it, once one exists and is
well-maintained. `recursion-schemes` is exactly that library. This is a
correction, not a preference — the v1 reasoning was wrong under the Standard
and is not carried forward.

Three consequences fall out of this, and they are the reason to do it:

- **Parsing is an anamorphism** — an unfold from `aeson`'s `Value` into `Rule`.
  Because a JsonLogic rule already *is* a JSON tree, this is a direct
  structural correspondence, not a forced analogy.
- **Evaluation is a catamorphism** — a fold from `Rule` into a result. The
  carrier type must be `Env -> Result Value` (a function), not `Result Value`
  directly, so that `map`/`filter`/`reduce` can invoke a child's evaluator
  once per element against a changing environment. This is a resolved design
  decision, not an open question — see §6 known hazards for why it matters.
- **Validation and pretty-printing are additional algebras over the same
  functor.** One `RuleF` definition; three independent, small folds
  (`eval`, `validate`, `pprint`) instead of three hand-written tree-walkers
  that must be kept in sync by hand. This is the actual engineering payoff —
  not aesthetics, fewer places for the same bug to be fixed inconsistently.

Parse (ana) → Eval (cata) composed end-to-end is a **hylomorphism** — the
standard third member of this recursion-scheme family. The whole pipeline is
describable in one consistent vocabulary.

**Boundary typing (Standard 3.9, 3.14):** internally, the eval algebra may be
written against an `mtl`-style constraint (`MonadError JsonLogicError m`)
rather than hardcoded to `Either`, so the same algebra can run under a pure
test stack or a real one without rewriting it. The *public*, exported
`evaluate` function is concrete — `Rule -> Env -> Either JsonLogicError
Value` — per 3.14: polymorphism is a tool for the internals, not something
a caller of this library should have to instantiate.

**Custom-operator extension (Standard 0.5):** this is a Handle, not a
typeclass — a record of functions supplied at the one call site that needs
custom operators, not ad hoc polymorphism across many types. There is
exactly one implementation live per call site, which is precisely the case
0.5 reserves for a Handle over a typeclass.

### Where it does not apply — and must not be forced

JsonLogic's **truthy/falsy and `==` coercion rules** (`"0"` is truthy, `[]` is
falsy, numeric-string coercion in comparisons) are an arbitrary table mandated
by matching JavaScript's own runtime behavior. There is no clean algebraic
structure underneath it to discover — it is a spec decision, not a theorem.

**Rule:** this table is implemented as a plain, boring, explicit function —
one pattern-match arm per case, fully commented against the spec, no
cleverness, no abstraction. If evaluating whether to abstract this ever comes
up: don't. Forcing structure onto arbitrary content hides the quirky logic
behind a layer future debugging has to see through.

### Formal verification — explicitly out of scope

LiquidHaskell and SMT-checked refinement types were considered and
**rejected for this project.** They have a demonstrated history of stalling
this author's past projects and are disproportionate to a rule-evaluation
library of this size. **Do not introduce LiquidHaskell, refinement types, or
similar formal-proof tooling into this project without an explicit, separate
decision to do so.** QuickCheck-verified algebraic laws (§5) are the
project's actual verification ceiling.

## 5. Correctness strategy

Two independent, complementary layers — neither replaces the other:

1. **The classic test suite** (§2) — `jwadhams/json-logic-js`'s `tests.json`
   lineage, the same one Ruby/Go/PHP implementations verify against. Run the
   implementation against it and report the real pass count honestly,
   including any known deviations and why. Not the newer, in-progress
   `json-logic/.github` community suite — see §2's correction and §7's
   tracked, non-blocking item for that.
2. **Algebraic laws as QuickCheck properties** — real, statable theorems about
   the interpreter, checked over generated inputs rather than only finite
   examples:
   - De Morgan's laws under JsonLogic's boolean semantics:
     `not (and [a,b]) == or [not a, not b]`
   - Idempotence of coercion: `toBoolean (toBoolean x) == toBoolean x`
   - Literal short-circuiting: `eval (If (Lit true) t e) == eval t`

Both layers are required for a milestone to count as "done." Passing the
suite without the property layer, or vice versa, is not sufficient.

## 6. Known hazards (learned from prior failed attempts, not hypothetical)

Every prior Haskell attempt at this stalled on the same handful of things.
Treat these as flagged, not as surprises to discover later:

- **Arithmetic and numeric edge cases** (NaN, integer vs. float coercion) —
  the exact area where `JTeeuwissen/json-logic-haskell` has an unresolved,
  years-old open issue.
- **Indexing with mixed/weird types** (`var` by string vs. numeric index into
  arrays vs. objects) — also a confirmed unresolved issue in the same prior
  attempt.
- **Error-handling design.** Decide the error model (`Either JsonLogicError`,
  what constitutes an error vs. a `null` result per spec) *before* writing the
  evaluator, not during. The prior attempt's own maintainers opened a design
  spike for this and never closed it — don't inherit that by deferring the
  decision.
- **The `Env -> Result Value` carrier type for eval** (§4) — implement this
  from the start. Retrofitting it after building a naive `Rule -> Result
  Value` evaluator means rewriting the evaluator, not patching it.
- **Unbounded recursion depth on adversarial input (Standard 2.4, 4.10).**
  The anamorphism that parses `Value -> Rule` must carry an explicit,
  threaded depth limit and reject a rule that nests beyond it, before
  evaluation ever sees it — a maliciously deep JSON rule is exactly the
  "hostile input, arbitrarily deep nesting" case 2.4 requires a bound for.
  This does not appear as an issue in prior Haskell attempts only because
  neither got far enough to matter; treat it as a requirement from Phase 2
  (the parser), not an optimization added later.

**Note on Standard 5.7:** this section — researching known bug classes from
comparable prior art before implementation — is not a future to-do. It is
already done. The four items above *are* that research, performed during
this project's own design discussion and confirmed directly against the
prior attempts' issue trackers. Cite this section as satisfying 5.7 for the
project's core interpreter component.

## 7. Engineering constraints

**Governing document:** *The Haskell Engineering Standard* (supplied
separately, checked into the repo as `ENGINEERING_STANDARD.md` per its own
Part 6 setup checklist) applies in full to this codebase — every numbered
rule in its Parts 0–5, its precedence order (correctness > representation
safety > architectural clarity > performance > brevity), its mandatory
compiler flags (5.1), its ban list (5.2), and its pre-submission checklist.
It is not repeated here; this section states only where this project makes a
specific choice the Standard leaves open, or tightens a rule the Standard
states as optional.

**Project-specific decisions, consistent with the Standard:**

- **Recursion scheme:** `Fix`/`cata`/`ana`/`hylo` from `recursion-schemes` —
  not hand-rolled (§4; this corrects the v1 draft under Standard 4.1/4.7).
- **Extension mechanism (Standard 0.2, 0.5):** the operator set is
  **closed-by-design** (`RuleF`, exhaustive pattern match — new standard
  operators are a compile error at every site needing updating). Custom,
  user-supplied operators are **open-by-design**, via a Handle passed into
  the evaluator (§4) — the two axes are not blended, per 0.2's explicit rule
  against mixing disciplines on one axis.
- **Error type:** closed sum of closed sums, layered by pipeline stage —
  `ParseError`, `ValidationError`, `EvalError` — per Standard 1.5/2.3, living
  in its own module per 2.15, decided before Phase 3 begins (§6).
- **Formal verification ceiling:** the Standard treats LiquidHaskell as
  optional, opt-in tooling for high-stakes invariants only (Standard 3.11).
  This project **tightens that to a hard no** for the reason stated
  in §4 — a documented history of this specific tool stalling this author's
  projects. This is a project-specific tightening of an already-optional
  Standard rule, not a conflict with it.
- **Scope discipline (Standard 0.7/YAGNI):** v1 targets the classic
  `jwadhams`-lineage spec only (§2) — not the newer, explicitly
  in-progress `json-logic/.github` community standardization effort
  (`val`, `try`/`throw`, `??`), and not CertLogic or other dialect forks.
  This is a tracked, non-blocking decision (§10), not a dismissal — the
  community effort may become the better long-term target once it
  stabilizes, but "verified conformance" is not a claim to build against
  a moving spec.
- **No silent spec deviation:** any deviation from official JsonLogic
  behavior is documented in the README with the specific conformance-suite
  test case and reasoning — this is Standard 5.9 (deviations recorded, not
  silent) applied to spec conformance specifically, in addition to it
  applying to the Standard's own rules.

## 8. Phased plan — each phase is a complete, shippable stopping point

Permission is explicitly granted to stop after any completed phase and call
it done. A finished phase 3 is a success, not an unfinished project.

| Phase | Scope | Rough LOC |
|---|---|---|
| 0 | Standard's Part 6 setup checklist, in full, before any feature module: `cabal`/`hpack` skeleton with shared warning stanza (5.1), `hlint`+`weeder` wired into CI as errors, `ormolu`/`fourmolu` CI check, hexagonal `Core`/`Adapters` module skeleton, error module scaffolded empty, hspec+QuickCheck harness wired. Not skippable, not "clean up later." | ~50–100 (config, no logic) |
| 1 | `RuleF` functor over `Fix` (`recursion-schemes`). No parser yet — rules constructed directly as Haskell values. Proves the core idea. | 100–200 |
| 2 | Real anamorphism-based parser: `Value -> Rule`, with the depth bound (§6) enforced from this phase, not added later. Usable as a toy from here. | +300–400 |
| 3 | Core evaluator (`Env -> Result Value` carrier): literals, `var`, `and`/`or`/`not`, comparisons, `if`. Covers the majority of everyday JsonLogic usage. | +500–700 |
| 4 | Full standard operator set: `map`/`filter`/`reduce`/`all`/`some`/`none`, string ops, `merge`, `in`, arithmetic incl. the numeric edge cases flagged in §6. | +700–1000 |
| 5 | Official test-suite integration + QuickCheck law suite (§5). This is what turns "an interpreter" into "a verified, honest claim of conformance." | +400–600 |
| 6 | Custom-operator extension API, docs, README with an honest conformance report. | +300–500 |

Running total at full scope: roughly 2,300–3,400 core LOC, comfortably inside
the 3,000–5,000 budget with room for polish.

## 9. Definition of done (v1)

- Passes the official `json-logic/.github` suite at a stated, honest
  percentage, with any gap explicitly documented and justified.
- All §5 algebraic-law properties pass under QuickCheck.
- `-Wall -Werror` clean.
- No partial functions in library code (verifiable by inspection).
- Custom-operator extension demonstrated with at least one real example,
  implemented as a Handle (§7).
- README states clearly, without inflation, what this is and is not
  interoperable with.
- The Standard's own Pre-submission Checklist passes in full for every
  change, not just at a final release gate — this is the actual mechanism
  the rest of this section is checked by, not a separate, softer bar.

## 10. Open items for next session

- **Resolved this session:** which spec v1 targets. Answer: the classic
  `jwadhams`-lineage suite, not the newer in-progress `json-logic/.github`
  community effort (§2, §7). This was a real, previously-unnoticed
  conflation in the vision doc's own citations — now corrected.
- **New tracked (non-blocking) item:** the `json-logic/.github` community
  standardization effort — `val`, `try`/`throw`, `??`, and its explicit
  legacy-vs-new distinction — is worth periodically re-checking for
  stabilization. If/when it reaches a stable release, revisit whether
  `jsonlogic-hs` should target it instead of or in addition to the classic
  suite. Not a v1 requirement; don't let it silently resurface as scope
  creep either.
- **Fully resolved this session, with evidence, not just reasoning:** the
  empty-list arity question. Read `jwadhams/json-logic-js`'s actual
  source (`logic.js`, commit `c5c73601`) directly. Confirmed:
  `+`→`0`, `cat`→`""`, `merge`→`[]`, `missing`→`[]` all have clean, defined
  answers on empty input (each has an explicit fold seed or an
  empty-loop-does-nothing structure) — `RuleF` now models these as plain
  lists, not `NonEmpty`. Only `*` is a genuine runtime error in the
  reference (its fold has no seed) — `Mul` correctly keeps `NonEmpty`.
  `and`/`or` return JS `undefined` on empty input, which has no direct
  `Value` equivalent; mapped to JSON `null` as the closest, most
  consistent choice, recorded as a deliberate decision in `Rule.hs`. The
  previous session's "keep NonEmpty everywhere as the safer default"
  call was too broad — corrected now that real evidence exists instead
  of an inference about likely implementation patterns.
- The `RuleF` operator set (`Rule.hs`) is cross-checked against
  `json-everything`'s 34 builtin rule classes — one-to-one match (§4).
- **New dependency decision (Standard 2.11):** `deriving-compat`, for
  `Eq1`/`Show1` on `RuleF` — `Fix RuleF`'s `Eq`/`Show` depend on it; stock
  deriving does not produce these (§4, found on self-audit).
- Complete Phase 0 (Standard Part 6 setup checklist) before further work on
  `Rule.hs`'s dependents, per the Standard's own sequencing. Also blocked
  on network access to `hackage.haskell.org`/`archive.ubuntu.com` in this
  sandbox (`x-deny-reason: host_not_allowed`) — resolve locally or adjust
  the sandbox's network allowlist before Phase 0 can actually be verified
  by compiling, not just drafted.

## 11. Decision log (append-only)

Decisions made while writing code, recorded with their reasons so they can
be revisited deliberately rather than rediscovered.

- **`Arr [a]` added to `RuleF`.** The reference implementation evaluates
  arrays element-wise, so an array can contain rules; `Lit Value` alone
  cannot represent that. Recalled from reading `logic.js`; to be confirmed
  by the conformance suite in Phase 5.
- **`>` and `>=` are strictly binary.** Only `<` and `<=` have the
  three-argument "between" form. `RuleF` originally gave all four the
  optional third operand; corrected in Phase 2. Same confirmation caveat.
- **Wrong argument counts are a `ParseError`** (deliberate deviation,
  Standard 5.9). The reference implementation silently ignores extra
  arguments and pads missing ones; `RuleF` makes those shapes
  unrepresentable instead. If the conformance suite exercises sloppy
  arities, the affected operators are relaxed individually.
- **`"?:"` is parsed as an alias of `"if"`**, as recalled from the
  reference source. To be confirmed.
- **Depth bound:** `parseRule` rejects nesting deeper than 64
  (`defaultMaxDepth`); `parseRuleWithDepth` takes any limit. Nesting of
  exactly the limit is accepted. Property-tested.
- **`anaM` is defined locally in `Parse.hs`**, because its availability in
  the installed `recursion-schemes` could not be confirmed from here. If
  the library exports it, replace the local definition (Standard 4.7).

- **Evaluation domain is `Val`, not aeson's `Value`** (`JsonLogic.Core.Value`).
  The reference is JavaScript, so numbers are doubles and `NaN`/`Infinity`
  are real intermediate values (`1/0 > 5` is true). `Val` is JSON plus
  those; they become `null` only when a result is written back, as
  `JSON.stringify` does. Custom operators still see and return aeson
  `Value`s.
- **`If`, `Max`, `Min` are plain lists; `Sub` is `a (Maybe a)`.** The
  reference defines their empty forms (`null`, `-Infinity`, `Infinity`),
  so they are representable; `Sub` only ever reads two arguments. This
  corrects the earlier `Sub (NonEmpty a)` documentation, which claimed
  left-to-right subtraction the reference does not perform. `Mul` is now
  the only constructor still using `NonEmpty`.
- **Arrays and objects are never equal under `==` / `===`.** The reference
  compares them by reference identity, which parsed JSON does not have.
  (Differs only where one array is reached twice through the same
  variable.)
- **Strings order and index by code point**, not UTF-16 code unit.
  Differs from the reference only for characters outside the Basic
  Multilingual Plane.
- **Number conversion is explicit.** `Number()` semantics (`toNumber`:
  trimmed whole string, hex/octal/binary literals) and `parseFloat`
  semantics (`parseFloat`: longest decimal prefix) are separate functions
  because the reference uses each in different operators. Whitespace
  trimming uses Haskell's `isSpace`, which differs from JavaScript's set
  only for the byte-order mark.

- **Evaluator carrier is `Val -> Either EvalError Val`, concrete.** The
  public `evaluate`/`apply` are concrete (Standard 3.14). The `mtl`-style
  polymorphic internals floated in §4 are deferred (Standard 0.7): there
  is one interpreter and one monad, so the abstraction would have nothing
  to abstract over yet.
- **Operand evaluation follows the reference exactly.** Everything is
  evaluated before its operator except `if`, `and`, `or` (lazy) and the
  scoped collection operators. So `var`'s default is evaluated even when
  unused, and `{"+": [1, {"unknown": []}]}` fails while
  `{"or": [true, {"unknown": []}]}` does not.
- **`log` returns its argument and does nothing else.** The reference
  writes to the console; the pure core performs no I/O.
- **Custom operators** receive the current data and their evaluated
  arguments as aeson `Value`s and return `Either Text Value`. Names are
  matched exactly; the reference's dotted-name lookup (`"Math.max"`
  descending into a nested operations object) is not reproduced.
- **`reduce` requires its third (initial) argument.** The reference
  accepts two and starts from `null`; under the strict-arity decision a
  two-argument `reduce` is a parse error.
- **`missing` and `missing_some`** treat their keys as plain data. The
  reference re-evaluates each already-evaluated key as a rule, which only
  matters for a key that happens to look like a one-key object. A
  non-array key list to `missing_some` is treated as empty.
- **`*` with one operand returns it unconverted** (`{"*": ["3"]}` is the
  string `"3"`), because the reference folds without a seed. Noted
  because it looks like a bug and is not ours.
- **`NaN` and the infinities become `null` on output** (`{"/": [1, 0]}`
  is `null`), matching `JSON.stringify`.

- **Conformance runner (`conformance/Conformance.hs`).** Runs the classic
  suite at `test/tests.json` (downloaded from
  `jsonlogic.com/tests.json`, the suite other implementations share; check
  its licence before publishing) and prints a pass count and every
  failure. It reports rather than gates: each failure is either a bug or a
  deviation to record here, and which is a human decision. Once the
  failures are all explained it should become a gate with an explicit
  list of known deviations.

- **Empty `var` arguments.** `{"var": []}` is read as a lookup of the
  empty path, i.e. the data itself, because the classic suite requires it
  (`[{"var":[]}, 1, 1]`). This is the one place the strict-arity rule
  (§11, wrong argument counts are parse errors) yields to the reference.

## 12. Status log

- **2026-10-13: phases 0 to 5 complete.** Scaffold, `RuleF`, depth-bounded
  parser, JavaScript value semantics, evaluator, and conformance. The
  classic shared suite passes in full (278 of 278 cases, 24 comment
  entries skipped); unit and property tests: 116 examples, 0 failures.
  The conformance runner now fails the build on any regression.
- **What the suite does not prove.** It never exercises most of the
  deliberate deviations in §11 (strict argument counts, array equality,
  code-point ordering), so passing it says the reference's own examples
  are honoured, not that those deviations are harmless. A differential
  test against the real JavaScript implementation would close that gap.
- **Still to do before a first release:** a `LICENSE` file and real
  `maintainer` metadata in the `.cabal` file (both currently
  placeholders), `hlint` / `weeder` / formatter enforcement per the
  Standard's setup checklist, a Haddock coverage pass, `cabal check`, and
  a Hackage candidate upload.
- **Tracked, not scheduled:** the newer `json-logic/.github` community
  specification (§2, §10).
- **Differential test (`differential/`).** `cabal run jsonlogic-hs-differential -- COUNT SEED`
  generates random rules over every builtin operator, runs them through this
  library and through the real `json-logic-js` (via Node), and lists every
  disagreement. It needs Node and an `npm install` in `differential/`, so it is
  an executable, not part of `cabal test`. Rules use argument counts this
  library accepts, so a mismatch is a real behavioural difference, not the
  documented strict-arity deviation. Probing the reference directly confirmed
  three of the deviations are real: it ignores extra arguments (`{"-":[5,2,1]}`
  is 3), treats the same array reached twice as equal, and accepts a
  two-argument `reduce`.
