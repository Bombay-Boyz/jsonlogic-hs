# The Haskell Engineering Standard

**Status: binding.** This document is the constitution for any Haskell
codebase it governs — read and applied *before* the first module is
written, not retrofitted afterward. It applies equally to AI-authored and
human-authored code, and to AI or human review of either. Where a request
conflicts with this document, follow this document and say so explicitly.

**Precedence when principles conflict**, highest first:
1. Correctness / totality — a fast or elegant function that can go wrong
   silently loses to a slower one that can't go wrong at all.
2. Safety of representation — illegal states must stay unrepresentable
   even at the cost of a more verbose type.
3. Architectural clarity — a boundary that makes the system easier to
   reason about beats a shortcut that makes today's diff smaller.
4. Performance — addressed deliberately, with evidence, never guessed at
   the expense of 1–3.
5. Brevity — the last thing optimized for, never the first.

---

## Part 0 — Foundational Design Principles, in Haskell Terms

Classic software-design principles are usually stated in OOP vocabulary.
They still hold; they just cash out differently without classes and
inheritance. State them this way so there's no ambiguity about what they
mean here.

### 0.1 Single Responsibility Principle
One module, one reason to change. A module that both parses a format and
knows how to render one has two responsibilities wearing one name; split
it. The test: describe the module's job in one sentence with no "and."

### 0.2 Open/Closed Principle
A system should be open to extension without modifying proven code. In
Haskell this is a genuine design choice with two valid, opposite-shaped
answers — pick the right one deliberately, per extension axis:
- **Closed sum + exhaustive pattern match**, when the set of cases is
  owned and finite (e.g. the source formats *this* project supports).
  Adding a case is a compile error at every site that needs updating —
  extension is "open" in the sense that the compiler *guides* the change,
  even though the type itself is closed.
- **Typeclass or record-of-functions ("Handle") boundary**, when the set
  of cases is genuinely open to third parties (e.g. a plugin renderer a
  downstream consumer supplies). Extension happens with zero edits to
  existing code.

Never blend the two on the same axis of variation — decide, per
extension point, which discipline governs it, and document the choice.

### 0.3 Liskov Substitution — restated as typeclass law-abidance
Any two instances of the same typeclass must satisfy the same laws.
A `Monoid` instance whose `mappend` isn't associative, an `Eq` instance
that isn't reflexive, an `Ord` instance inconsistent with its `Eq` — these
are the Haskell equivalent of a subtype that violates its supertype's
contract. State each instance's laws in a comment and, wherever practical,
test them with a law-checking library (`hspec-hedgehog`, `checkers`).

### 0.4 Interface Segregation
Typeclasses (and Handle records) should be as small as the caller actually
needs. A function should be constrained by the narrowest capability it
uses — `(Monad m) => ...` rather than `(MonadIO m, MonadState S m,
MonadReader R m) => ...` when only `Monad` is actually exercised. A fat
typeclass or a fat Handle forces every implementation to satisfy
capabilities most call sites never touch.

### 0.5 Dependency Inversion — the Handle pattern
Haskell has no interfaces in the OOP sense; the idiomatic equivalent of
"depend on abstractions, not concretions" is passing a small record of
functions (a **Handle**) into pure or effect-polymorphic code, rather than
having that code import a concrete `IO`-performing module directly.

```haskell
-- | The core logic depends on this shape, not on any concrete backend.
data StorageHandle m = StorageHandle
  { loadDocument :: DocId -> m (Either MirrorError Document)
  , saveDocument :: DocId -> Document -> m (Either MirrorError ())
  }

-- | Pure with respect to *how* storage happens — swap the Handle for
-- tests (an in-memory Map) without touching this function.
republish :: Monad m => StorageHandle m -> DocId -> m (Either MirrorError ())
```
This is what makes core logic testable without spinning up real I/O, and
it is preferred over a typeclass when only one concrete instance is ever
needed per call site (a Handle is simpler; a typeclass is for genuine
ad hoc polymorphism across many types).

### 0.6 DRY — with its correct limit stated explicitly
Duplication of *knowledge* (the same invariant, checked two different
ways in two places) must be eliminated — that's what the shared
"raw → validated" translation layer and the shared smart constructors in
Part 2 exist to prevent. Duplication of *code that happens to look
similar but encodes unrelated decisions* should **not** be merged. Two
functions that currently have the same three lines but answer different
questions are not a DRY violation — collapsing them creates a false
coupling that will need to be un-collapsed the moment the two decisions
diverge. When in doubt: ask whether the two call sites would ever need to
change independently. If yes, don't merge them.

### 0.7 KISS and YAGNI
Implement the mechanism the current, real requirement needs — not the one
a hypothetical future requirement might need. An error constructor,
typeclass, or extension point is added in the same change that makes it
reachable, never speculatively (this is also stated in Part 1 as a
closed-vocabulary rule, because it's load-bearing there too). Prefer the
simplest technique in Part 3's toolkit that fully closes the problem;
reach for a more powerful technique (GADT, existential, type family) only
once the simpler one is demonstrably insufficient, and say why in a
comment when you do.

### 0.8 Law of Demeter
A function should not reach through more than one level of a nested
record/structure to get what it needs. If `renderPage` needs
`documentMeta.author.displayName`, that's a sign `documentMeta` should
expose `authorDisplayName` directly, or the caller should be handed the
narrower value it actually needs rather than the whole tree. This keeps
refactors local: changing how `author` is represented shouldn't ripple
into every renderer that happens to reach through it.

### 0.9 Composition over inheritance — the Haskell default
There is no inheritance to avoid reaching for; the discipline instead is
to build behavior by composing small, independent pieces (function
composition, typeclass constraints, monad transformers, or simple value
composition) rather than by building one large record or one large
typeclass that tries to be everything. If a type is accumulating
unrelated fields "because it's convenient to have them all in one place,"
that's the inheritance instinct resurfacing in record form — split it.

### 0.10 Principle of Least Power
Give every function the least expressive type that still lets it do its
job. A function that doesn't need `IO` shouldn't have it in its
signature. A function that only reads state shouldn't be typed to allow
writes. A typeclass constraint should name the weakest class in the
hierarchy that suffices (`Foldable` instead of a concrete `[]`, when only
folding is needed). Least power at the type level is what makes a
signature trustworthy on sight — the type is a promise about what the
function *cannot* do, not just what it does.

### 0.11 Compatibility is a promise about the boundary, not the internals
Preserve backward compatibility for a module's public API where
practical — don't churn an external signature for cosmetic reasons; see
2.13 for how this is enforced through versioning. That promise applies
only to the boundary. Internally, continuously review and simplify type
and module design: refactor implementations freely, delete dead
abstractions, and collapse a type that turned out more general than
needed, as long as the public shape either stays put or moves behind a
deliberate version bump. Stability and simplification aren't in tension
once it's clear which side of the export list each applies to.

---

## Part 1 — Language Principles (never violated)

### 1.1 Every function is total, or its type says it isn't
No partial functions. Never use: `head`, `tail`, `last`, `init`,
`fromJust`, `(!!)`, non-exhaustive pattern matches, `error`, `undefined`,
or incomplete `case`/`if` chains on a closed type. If a function can fail,
its type says so — `Maybe a` for one reason, `Either e a` when the reason
needs to be named. `-Wall -Werror -Wincomplete-uni-patterns
-Wincomplete-record-updates -Wpartial-fields` are mandatory, not optional.

### 1.2 Illegal states are unrepresentable, not merely rejected
Prefer a type that cannot hold invalid data over a type that can, plus a
validator. This is the single most important habit in this document and
recurs throughout Part 3's toolkit — see especially 3.1–3.4.

### 1.3 Cross-field invariants are enforced once, at construction
If correctness depends on a relationship between two or more fields (e.g.
"every row has the same width"), the constructor is not exported; the
only way to obtain a value is a smart constructor returning
`Either err a`. Once constructed, the value is trusted everywhere
downstream — never re-checked. (Full treatment in 3.3.)

### 1.4 Closed domains are closed types, not strings
Any finite, known set of alternatives is a sum type — never a
`Text`/`String` compared against literals. A value outside the closed set
is a compile-time impossibility, not a runtime "unrecognised value"
string.

### 1.5 Errors are a closed, structured vocabulary
One sum type per failure category, each constructor documented with *why
it exists* and *what can produce it*. No stringly-typed errors, no
catch-all `OtherError Text`. A constructor is added only in the same
change that makes it reachable.

### 1.6 Purity is pushed to the edges
Business logic is pure. `IO` is confined to a thin shell at the
boundary — reading input, writing output, catching foreign-library
exceptions and converting them into the structured error type
immediately. A function is never `IO`-typed merely because a library it
calls can throw; catch and convert at that call site.

### 1.7 No implicit escape hatches
No `unsafePerformIO`, no `Debug.Trace` in committed code, no `Show`-based
serialization used as data interchange, no orphan instances (an instance
lives next to the type or next to the class, never floating in an
unrelated module).

### 1.8 Explicit over implicit, always
No global mutable state (`IORef`s reached for reflexively instead of
threading state explicitly), no reliance on typeclass resolution doing
something surprising (`OverlappingInstances`, `IncoherentInstances` are
red flags requiring explicit justification), no reflection-like tricks to
avoid writing an explicit case. If a reader has to run the code mentally
to know what will happen, it's too implicit.

### 1.9 Immutability is the default, not an aspiration
Data is immutable unless a specific, profiled reason calls for a mutable
cell (`IORef`, `STRef`, `MVar`, a mutable vector). Local transformations
use record-update syntax and pure recursion, not accumulation into a
mutable variable "for convenience." This is distinct from 1.8's ban on
*global* mutable state — 1.9 is the default even for a value that never
leaves a single function.

### 1.10 Referential transparency is the target, not just a byproduct of purity
An expression's value must not depend on when, how many times, or in
what order it's evaluated — this is what makes equational reasoning
possible: a sub-expression can be replaced by its value, memoized, or
lifted out and shared, without changing the program's meaning.
`unsafePerformIO` and any hidden mutable state breaks this, which is why
1.7 bans the former outright rather than merely discouraging it.

### 1.11 Zero dead code, zero empty stubs — strictly enforced
Nothing unreachable is committed: no unused top-level bindings, no
commented-out blocks "kept just in case," no functions exported but
never called anywhere in the codebase or its public API surface.
`-Wunused-top-binds -Wunused-local-binds -Wunused-matches` are treated
as errors, and a dead-code detector (`weeder`) runs in CI alongside
`hlint`. Equally strict in the other direction: a function is either
fully implemented or does not exist in the merged code — no
`someFunction = undefined`, no `error "TODO"`, no placeholder that
type-checks but panics if called. If a capability isn't ready, its
call site isn't merged either; half-built scaffolding is worse than an
honest absence, because it compiles clean and looks finished.

---

## Part 2 — Software Architecture (never violated)

### 2.1 One canonical intermediate representation
When a problem involves multiple input or output formats converging on
shared logic, there is exactly **one** internal representation every
input funnels into and every output funnels out of. Format-specific logic
lives only in the thin translation layer at each edge; core logic is
written once, against the IR.

### 2.2 Validation is staged, not scattered
Recovering *shape* is kept separate from checking *content validity*. Use
a shared "raw → validated" translation layer if multiple producers reach
the same validation step — write the validation once, share it.

### 2.3 Errors are layered to match pipeline stages
The top-level error type is a closed sum of closed sums, one case per
pipeline stage. A function's return type tells you unambiguously which
stage produced a failure without inspecting a string.

### 2.4 Recursive structures carry an explicit resource bound
Any recursive data type that hostile or careless input could nest
arbitrarily deep gets an explicit, threaded depth/size limit at every
point of recursive descent, applied **uniformly** across every stage
capable of producing that nesting — not just the one someone happened to
think of first.

### 2.5 Module boundaries are drawn by responsibility, enforced by exports
Each module has one reason to change (0.1). Constructors and functions
that could violate an invariant if exported freely are **not** exported.
`-Wmissing-export-lists` is mandatory. A module's export list is its
contract.

### 2.6 Dependency direction is acyclic and points at the core
Format-specific / adapter modules depend on the core IR module; the core
never imports outward. This is hexagonal architecture stated in Haskell
terms: a pure core, with parsers/renderers/storage/network as adapters at
the boundary, wired in either via closed dispatch (0.2) or a Handle/
typeclass (0.5) — never via the core reaching outward to call them
directly.

### 2.7 Extensibility mechanism is chosen deliberately, per axis, and documented
See 0.2. State, in the relevant module's Haddock, whether a given
extension point is closed-by-design (compiler-enforced exhaustiveness) or
open-by-design (Handle/typeclass) — and don't let a "just this once"
exception blur the line without updating the documentation to match.

### 2.8 Every architectural claim has a mechanical check behind it
"This module is total" is checked by `-Wincomplete-patterns -Werror`.
"This invariant holds for every value of this type" is checked by a
property test. A claim without an enforcement mechanism is a wish, not a
property of the code, and must not be described as though it were
guaranteed.

### 2.9 Testing is part of the architecture, not an afterthought
Property-based tests are a *design tool*, not just a QA step — writing
the property before/alongside the implementation ("for any input, output
satisfies X") is often what reveals whether the type actually needs a
smart constructor. Regression tests are named after the specific defect
they guard, not "test5." Unit tests exercise the pure core directly,
never through an `IO` shell, because the core is pure and testable in
isolation by construction (1.6, 0.5) — if a test needs `IO` to exercise
core logic, that's a signal the architecture leaked an effect it didn't
need to.

### 2.10 Organise modules by domain, not by technical utility
Group modules by what they mean in the problem (`Document`,
`Pipeline`, `Rendering.Html`), not by what kind of thing they technically
are (`Utils`, `Helpers`, `Common`, `Types`). A grab-bag `Utils.hs` is a
confession that some piece of logic was never given a real owner — it
belongs in whichever domain module actually needs it, even at the cost
of a small amount of near-duplicate code across two domains (0.6).

### 2.11 Depend on the fewest, best-maintained libraries the problem needs
Prefer an established, actively maintained library over a hand-rolled
equivalent — including for 4.2's named algorithms: if a well-maintained
library already implements the algorithm correctly, depend on it rather
than re-proving it in-house (4.7), unless the dependency itself becomes
the risk (parsing untrusted input, licensing, supply-chain exposure).
Every dependency joins the trusted computing base; add one deliberately,
not by default, and prefer one well-chosen library over three overlapping
ones that each cover part of the same need.

### 2.12 Builds are deterministic and reproducible
Pin dependency versions (`cabal.project.freeze` or an equivalent
lockfile, a fixed Stackage resolver) so the same commit produces the same
build, today and a year from now. A build that silently picks up a newer
transitive dependency on every fresh checkout undermines every other
guarantee in this document — a passing property test says little if next
week's build compiles different code underneath it.

### 2.13 Package and module versioning follows the PVP, deliberately
Follow the Haskell Package Versioning Policy: a breaking change to any
exported signature bumps the major version; anything else does not.
Combined with 0.11 — a public signature changes only behind a deliberate
version bump, never as the silent side effect of an unrelated refactor.

### 2.14 Code is portable by default
Avoid unnecessary dependence on a specific OS, filesystem layout, or
compiler version beyond what the problem genuinely requires. Use
portable path/file libraries (`filepath`, `directory`) instead of
hardcoded separators or absolute paths; note any genuine, unavoidable
platform dependency explicitly in the module that has it, rather than
letting it surface later as a mysterious build failure somewhere else.

### 2.15 Errors live in a dedicated module, and every message is descriptive
The closed error vocabulary (1.5, 2.3) is defined in its own module,
separate from the domain logic that raises it — one place a reviewer or
a new contributor goes to see the entire universe of ways the system can
fail. Every constructor's rendered message names the specific thing that
went wrong and, wherever the information is available, the specific
input or location that caused it (a file path, a line number, a field
name) — never a bare "invalid input" or "something went wrong." A vague
error message is a silent failure wearing an error type; it satisfies
1.5's letter while missing its point.

---

## Part 3 — The Haskell Type-System Toolkit: What to Reach For, and When

Use the least powerful tool in this list that fully solves the problem
(0.7, 0.10). Escalate only when the simpler tool genuinely can't express
the invariant, and say so in a comment when you escalate.

### 3.1 Algebraic data types — the default, reach for first
Sum types for closed alternatives, product types (records) for "all of
these together." This is the baseline for making illegal states
unrepresentable: a value that can only be one of N well-defined shapes,
never a partially-filled record with a `Maybe` flag simulating a variant.

```haskell
-- Bad: simulates a sum type with a flag and nullable fields.
data Shape = Shape { isCircle :: Bool, radius :: Maybe Double
                    , width :: Maybe Double, height :: Maybe Double }

-- Good: the type itself rules out "circle with a width."
data Shape = Circle Double | Rectangle Double Double
```

### 3.2 Newtypes — eliminate primitive obsession at zero runtime cost
Any domain concept represented as a bare `Text`, `Int`, or `Double` that
could be confused with another value of the same primitive type gets a
`newtype`. `UserId`, `DocId`, and `SessionId` should not all be `Text`
that the compiler will happily let you swap by accident.

```haskell
newtype UserId = UserId Text deriving (Eq, Ord, Show)
newtype DocId  = DocId  Text deriving (Eq, Ord, Show)
-- lookupDoc :: UserId -> DocId -> ...   -- now a compile error to swap them
```

### 3.3 Smart constructors + opaque exports — the standard invariant-enforcement mechanism
Whenever a value's validity depends on content, not just shape (a
rectangular table, a syntactically valid URL, non-empty-after-trimming
text), the type's constructor is unexported and the *only* way to build
one is a function returning `Either err a`. This is not optional
whenever 1.2/1.3 apply — it is the mechanism those principles are
implemented with.

```haskell
module M (Percentage, mkPercentage, unPercentage) where

newtype Percentage = UnsafePercentage Double deriving (Eq, Ord, Show)

mkPercentage :: Double -> Either ValidationError Percentage
mkPercentage x
  | x >= 0 && x <= 100 = Right (UnsafePercentage x)
  | otherwise          = Left (OutOfRange x)

unPercentage :: Percentage -> Double
unPercentage (UnsafePercentage x) = x
```

### 3.4 Phantom types — compile-time-only tags with zero runtime cost
Use a type parameter that never appears on the right-hand side of any
constructor to carry a *static* fact about a value — "this text has been
escaped," "this ID belongs to a draft, not a published document" — that
the compiler checks but that costs nothing at runtime.

```haskell
data Status = Draft | Published

newtype DocRef (s :: Status) = DocRef Text

publish :: DocRef 'Draft -> IO (DocRef 'Published)
-- A published-only operation now cannot even type-check against a draft.
notifySubscribers :: DocRef 'Published -> IO ()
```

### 3.5 GADTs — when different constructors need different result types
Reach for a GADT when a plain ADT would force every constructor to share
one result type even though they logically produce different types, and
you'd otherwise be compensating with an unsafe cast or a partial
extraction function. Typed embedded languages/ASTs, and safety-indexed
wrappers (a value's constructors *are* its safety guarantee) are the
canonical use case.

```haskell
data Expr a where
  IntLit  :: Int -> Expr Int
  BoolLit :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a          -- total: GADT rules out ill-typed terms
eval (IntLit n)  = n
eval (BoolLit b) = b
eval (Add x y)   = eval x + eval y
eval (If c t e)  = if eval c then eval t else eval e
```
Without the GADT, `eval :: Expr -> SomeValue` would need a runtime type
check and a partial extraction (`asInt`, `asBool`) — exactly the
partiality Part 1 prohibits. The GADT moves that check to compile time.

### 3.6 Existential types — last resort, justify explicitly
Use `forall`/existentials only when you genuinely need a heterogeneous
collection of values that share a common interface but whose concrete
type doesn't matter (e.g. a list of "anything renderable"). Existentials
throw away the very type information Part 1–3 spend so much effort
preserving, so treat reaching for one as a flag: state in a comment
*why* the concrete type must be forgotten here and why a closed sum
(3.1) or typeclass-constrained polymorphic function isn't sufficient
instead.

### 3.7 Typeclasses — principled overloading with laws, not ad hoc dispatch
A typeclass is appropriate when multiple types share genuinely the same
abstract operation *and* that operation obeys stated laws (0.3). It is
not a substitute for a Handle (0.5) when there's only ever one
implementation live at a call site, and it is not a way to fake
function overloading for otherwise-unrelated operations. Every
non-trivial typeclass ships with its laws stated in a Haddock comment,
and law-checking tests where a library exists for the class in question.

### 3.8 DataKinds / kind signatures — promote a value to the type level only when the compiler needs it there
Use `DataKinds` when a value genuinely needs to participate in a
type-level distinction (as in 3.4's `Status` kind). Don't promote a value
to the type level "for elegance" if a plain runtime value moving through
an `Either`/smart-constructor already closes the same hole — that's
reaching for 3.4–3.5's power where 3.1/3.3 would fully suffice (0.7).

### 3.9 mtl-style constraints / effect boundaries — dependency inversion at the type level
When a function needs several capabilities (state, config, failure)
without being locked to one concrete monad stack, constrain it
polymorphically (`(MonadReader Config m, MonadError MirrorError m) => ...`)
rather than hardcoding `ReaderT Config (ExceptT MirrorError IO) a`. This
is 0.5's dependency inversion applied to effects: the function states
what it needs, not which stack provides it, and can be run over a pure
test stack and a real `IO` stack without being rewritten. Reach for a
full effect system (`polysemy`/`effectful`/`fused-effects`) only once
the mtl-style constraint set is unwieldy in practice — not by default.

### 3.10 Free monads / tagless-final — advanced, use only when interpreter-swapping is a real requirement
These let business logic be written once and interpreted multiple ways
(a real backend, an in-memory test double, a dry-run/audit log). They are
powerful and legitimately useful, but they add real conceptual and
compile-time cost. Reach for them only when the Handle pattern (0.5)
genuinely can't express the needed flexibility — e.g. when the *sequence
of operations itself* needs to be inspected or optimized before
execution, not just when-swapped. Justify the choice in the module's
Haddock when used.

### 3.11 Refinement types (Liquid Haskell) — optional, for the highest-stakes invariants only
When a smart constructor's `Either`-returning runtime check would be
better as a statically checked refinement (e.g. "this index is always
in bounds," "this list is always sorted"), Liquid Haskell annotations
can move the proof obligation to compile time, checked by an SMT solver.
This is an opt-in escalation for the small number of invariants where
the cost of getting it wrong is high enough to justify the tooling
overhead — not a default for every smart constructor in the codebase.

### 3.12 Type aliases document; they don't protect — don't mistake one for the other
`type CustomerId = Text` buys readability and nothing else: the compiler
still lets a bare `Text` meant as an email address flow anywhere a
`CustomerId` is expected. Reach for `type` only as genuine shorthand for
a long, repeated type expression. The moment two values sharing an
underlying representation must not be interchangeable, that's `newtype`
territory (3.2), not `type`.

### 3.13 Use the standard abstractions idiomatically; prefer the weakest one that suffices
Write code against `Functor`, `Applicative`, `Monad`, `Foldable`, and
`Traversable` using their standard combinators (`fmap`, `traverse`,
`sequence`, `for_`) rather than reimplementing their effect by hand —
4.1's combinator discipline applied to the handful of classes that recur
constantly. Prefer `Applicative` over `Monad` whenever the computation's
structure doesn't actually depend on a previous result: an `Applicative`
signature is a stronger, more honest claim ("these effects are
independent of each other") than a `Monad` signature over the same code,
and it composes and parallelizes better as a direct consequence.

### 3.14 Parametric polymorphism internally; concrete types at the boundary
Prefer a polymorphic type (`Foldable t => t a -> ...`) inside a module
where it genuinely generalizes the logic without adding constraint-
solving noise (0.10). At a module's public boundary — the functions in
its export list — prefer concrete, specific types over polymorphism for
its own sake: `Document -> Either MirrorError Html` is easier to consume,
produces clearer type errors at the call site, and is easier to reason
about than a signature generalized over a typeclass nobody outside the
module will ever instantiate differently.

### 3.15 Linear types — when a resource's usage discipline is itself the invariant
Where correctness depends not just on a value's shape but on *how many
times and in what order* it is used — a file handle that must be closed
exactly once, a protocol token that must not be duplicated or silently
dropped — `LinearTypes` let the compiler enforce that usage discipline
the same way GADTs (3.5) enforce shape. Treat this as a specialized,
last-resort-among-first-resorts tool: reach for it only when the
resource-usage invariant can't be adequately captured by a Handle (0.5)
plus ordinary discipline, since linear types add real signature-level
complexity everywhere the resource is touched.

---

## Part 4 — Code Style: Algorithmic, Not Improvised

Every non-trivial piece of control flow is either a **named combinator**
or a **named, published algorithm** — so a reader verifies it by
recognition, not by re-deriving it line by line. This is not a request
for brevity or point-free cleverness; point-free style is permitted only
where it reveals structure (4.4), and is otherwise prohibited.

### 4.1 Name the recursion scheme before writing the recursion
| Shape | Combinator |
|---|---|
| Consume a structure, build one value | `foldr` / `foldl'` |
| Produce a stream from a seed until done | `unfoldr` |
| Structure-preserving traversal with effects | `traverse` / `mapM` |
| Map while threading accumulator state | `mapAccumL` / `mapAccumR` |
| Repeated application until a fixed point | `until` / `iterate` + `takeWhile` |
| Group / partition by a predicate | `span`, `break`, `groupBy`, `partition` |

Hand-written recursion is reserved for logic with no existing combinator
shape — and even then, the Haddock states which scheme it approximates
and why no combinator fits.

### 4.2 If a published algorithm solves the exact problem, implement that algorithm, by name
Don't reinvent a heuristic that happens to converge on a known
algorithm's answer — implement the known algorithm and cite it (Kahn's
algorithm for topological order, Pratt parsing for precedence-based
expression parsing, Myers' algorithm for sequence diffing, and so on).
Naming it lets a reviewer check the implementation against a reference
instead of re-establishing correctness from first principles.

### 4.3 Match complexity to access pattern
`Map`/`Set` for lookup-heavy code, not a linearly scanned `[a]`; `Seq`
for append/index-heavy code, not a repeatedly reversed list. Do not,
conversely, reach for a more sophisticated algorithm than the problem
needs (0.7) — correct complexity for the real access pattern, not
maximal sophistication for its own sake.

### 4.4 Point-free style only when it reveals structure
`f = g . h` is preferred over an explicit lambda only when the
composition itself is the insight worth stating. If it requires the
reader to mentally re-expand it to check what's happening, write it
pointfully. Never chain more than two or three compositions without a
named intermediate.

### 4.5 Every exported function is one lemma
If a function's body doesn't fit in roughly 15–20 lines, it's not one
claim, it's several — split it and name each part. A long `case` across
a closed type is fine (it's exhaustiveness, not sprawl); a long sequence
of unrelated steps is not.

### 4.6 The type signature is the theorem; the Haddock states the invariant, not the procedure
**Bad:** `-- loops through the list and picks out the valid rows`
**Good:** `-- | The only way to obtain a 'Table': every row, header
included, is checked against the first row's width. On success, the
result's rectangularity is guaranteed for every later consumer.`

### 4.7 Reuse a proven library's implementation of a named algorithm before writing your own
4.2 says to implement a named, published algorithm rather than an ad hoc
heuristic. The stronger version: if a well-maintained library already
provides that exact algorithm correctly, depend on it (2.11) instead of
re-implementing it. Reserve a from-scratch implementation for the case
where no adequate library exists, or where this codebase's handling of
untrusted input specifically requires an implementation whose safety
properties can be independently audited (Part 5).

### 4.8 Consistent naming conventions, applied without exception
One convention per category: types and constructors in `UpperCamelCase`,
functions and fields in `lowerCamelCase`, module names mirror their file
path. Smart constructors are named `mkX` (3.3); unsafe or internal-only
constructors are prefixed `Unsafe` or kept unexported entirely. A reader
should be able to predict a name's category from its case alone, without
opening the module that defines it.

### 4.9 Laziness is exploited on purpose; strictness is added on evidence
Rely on laziness deliberately for what it's actually good at — infinite
or self-referential structures, short-circuiting, decoupling production
from consumption — and treat an unexplained space leak as a bug to
diagnose, not a tax to accept. Add strictness annotations (`!` on
fields, `seq`, `foldl'` over `foldl`, `BangPatterns`) only where
profiling, not intuition, has shown a real leak or a real cost.
Premature strictness is the same unjustified complexity 0.7 already
prohibits, just approached from the performance direction instead of the
abstraction direction.

### 4.10 Recursion over unbounded input is stack-safe — strictly enforced
Beyond 2.4's bound on adversarial nesting, any function that folds or
recurses over a structure whose size is driven by ordinary, legitimate
input (a large file, a long list) must not blow the stack. Use
`foldl'`/`foldMap'`-style strict folds instead of `foldr`/lazy `foldl`
for large accumulations, prefer combinators with known stack behavior
(4.1's table) over bespoke recursion, and make any genuinely necessary
hand-written recursive function tail-recursive with a strict
accumulator. A function that works in tests on small fixtures but is
never checked against a realistically large input is not verified to be
stack-safe — add a large-input case specifically to catch this (5.4).

### 4.11 Known space-leak patterns are avoided by construction
The recurring, well-documented sources of Haskell space leaks are treated
as defects, not idiosyncrasies to tolerate: unbounded thunk buildup from
lazy accumulation (`foldl` on a large list, a lazy `State`/`Writer`
accumulator never forced), quadratic-blowup string/list concatenation
(repeated `(++)` instead of a builder or `Data.Text.Builder`/`Seq`),
holding onto the head of a list or a whole lazy `ByteString`/`Text` far
longer than needed because one small part of it is still referenced, and
unbounded lazy I/O. Each is a specific, nameable pattern (not a vague
"Haskell can leak" caveat), and each has a specific, standard fix — apply
the fix at the point the pattern is introduced, not after a production
memory graph shows it.

---

## Part 5 — Verification and Enforcement

### 5.1 Mandatory compiler flags
```
-Wall -Wcompat -Werror
-Wincomplete-uni-patterns -Wincomplete-record-updates
-Wmissing-export-lists -Wunused-imports -Wunused-top-binds
-Wunused-local-binds -Wunused-matches
-Wredundant-constraints -Wpartial-fields -Widentities
```

### 5.2 Banned outright (lint-enforced)
`head`, `tail`, `fromJust`, `(!!)`, `error`, `undefined`,
`unsafePerformIO`, `Debug.Trace.*` in committed code, partial record
field accessors, `OverlappingInstances`/`IncoherentInstances` without
explicit written justification, and any unreachable code flagged by
`weeder` (1.11).

### 5.3 Formatting is enforced, not a matter of taste
`ormolu` or `fourmolu` runs as a CI check alongside `hlint`, so a diff is
never mixed style-only noise and substantive change, and no two
contributors — human or AI — drift into different house styles over
time.

### 5.4 Tests are exhaustive, including outliers — strictly enforced
A test suite that only exercises the happy path is incomplete by
definition, not merely light. For every function with a bounded or
partially-bounded domain, tests cover: the empty/zero/minimum case, the
single-element case, the maximum or a realistically large case (4.10),
known adjacent-to-boundary values (off-by-one candidates), malformed or
adversarial input for anything crossing a trust boundary (2.4), and, via
property-based testing, a generator wide enough that these outliers are
sampled rather than hand-picked and potentially forgotten. "It passes on
the examples I thought of" is not exhaustive; "the generator's shrinker
was pointed at this class of input and found nothing" is closer to what
this rule requires.

### 5.5 Every stated invariant needs a property test in the same change that introduces it
A Haddock claim ("output is always escaped," "result is always
rectangular," "instance obeys the Monoid laws") ships with a
QuickCheck/Hedgehog property checking exactly that claim, in the same
diff — not as an afterthought.

### 5.6 Benchmarking is part of the test suite, not a side activity
Every module on a path that matters for real-world input size (parsers,
renderers, anything called per-request) has a `criterion` (or
equivalent) benchmark checked in alongside its tests, run in CI with
regression thresholds, not just on demand before a release. This is the
evidence 4.9 requires before adding a strictness annotation, and the
mechanism that makes "performance is addressed deliberately, with
evidence" (the precedence rule at the top of this document) an actual
practice rather than a stated intention. A change that regresses a
benchmark beyond its threshold is treated the same as a failing test.

### 5.7 Known bugs from comparable prior art are pre-empted, not rediscovered
Before implementing a component that has established prior art (a DOCX
parser, a Markdown renderer, an HTML escaper, a diff algorithm), the
issue tracker and changelog of at least one or two comparable existing
projects are reviewed for the specific bug classes they've hit historically
— parser desync on a particular malformed input, an escaping bypass, a
resource-exhaustion vector, an off-by-one in a boundary case. Each
applicable bug class gets a named regression test *written before or
alongside the first implementation*, not added reactively after the same
bug is independently rediscovered in this codebase. This research is
recorded (a short note or linked issue) so the next contributor can see
which known failure modes were already checked, rather than re-doing the
same archaeology or, worse, assuming it was never done.

### 5.8 Two-pass self-audit for AI-authored code
After writing a function, re-read its own Haddock claim and check, line
by line, whether the implementation actually establishes it. Do not
trust a comment written in the same pass that wrote the code. State
explicitly if a claimed invariant is *not* fully established, rather
than leaving an inaccurate guarantee in place.

### 5.9 Deviations are recorded, not silent
If a rule in this document is knowingly not followed for a specific,
justified reason (performance-critical hot path, a library's API forces
an escape hatch), record it as a short Architecture Decision Record next
to the code — what rule was relaxed, why, and what was tried first — so
it reads as a deliberate, reviewed exception, not an oversight.


---

## Part 6 — Before the Project Starts: Setup Checklist

Put this in place *before* the first feature module is written.

- [ ] `cabal`/`hpack` project skeleton with the Part 5 GHC flags set as a
      shared `common warnings` stanza, applied to every component
      (library, executable, tests) — not opt-in per module.
- [ ] `hlint` configured with the Part 5 ban-list as errors, not
      suggestions, wired into CI.
- [ ] Module skeleton drawn along the hexagonal boundary of 2.6 up
      front: a `Core`/`Domain` area (pure, IR-centered) and an
      `Adapters` area (parsers, renderers, storage, network) — so the
      dependency direction is structural from commit one, not something
      to "clean up later."
- [ ] A single canonical IR module identified and agreed before any
      format-specific or adapter-specific module is started (2.1).
- [ ] The closed-vocabulary error type (2.3) scaffolded before the first
      failure case is needed, with the reachability discipline (1.5)
      stated in its own Haddock.
- [ ] Test framework wired for both example-based (hspec) and
      property-based (QuickCheck/Hedgehog) tests from the first
      commit, with the rule from 2.9 (tests exercise the pure core
      directly) established as the default shape, and an explicit
      outlier/boundary-case checklist template for new test modules (5.4).
- [ ] Benchmark harness (`criterion` or equivalent) wired into CI with
      regression thresholds before the first performance-relevant module
      is merged, not added retroactively once something feels slow (5.6).
- [ ] For each planned component with established prior art, the known
      bug classes from at least one or two comparable projects' issue
      trackers reviewed and logged before implementation begins, with a
      named regression test planned for each applicable one (5.7).
- [ ] This document itself checked into the repository (e.g.
      `ENGINEERING_STANDARD.md`) and referenced from the project's
      `README` and from any AI-agent configuration file (`CLAUDE.md`,
      `.cursorrules`, or equivalent), so it is loaded as context
      automatically rather than depended on being remembered.

---

## Pre-submission Checklist

Before presenting any Haskell code written or edited under this
standard, confirm each of the following, explicitly:

- [ ] Single responsibility per module; no "and" needed to describe it (0.1)
- [ ] Extension axis for this change identified as closed-sum or
      Handle/typeclass, and consistent with existing choice (0.2, 2.7)
- [ ] Any new typeclass instance obeys its class's laws (0.3)
- [ ] Typeclass/Handle constraints are the narrowest that suffice (0.4, 0.10)
- [ ] New effectful dependency passed as a Handle or mtl-style
      constraint, not imported and called directly (0.5, 3.9)
- [ ] No duplicated *invariant logic*; deliberate, justified duplication
      of unrelated-but-similar code left alone (0.6)
- [ ] No speculative generality — the mechanism added matches a real,
      current requirement (0.7)
- [ ] No structure reached through more than one level deep (0.8)
- [ ] No partial functions; every failure path is typed (1.1)
- [ ] Illegal states are unrepresentable, not just checked (1.2, 3.1)
- [ ] Cross-field invariants enforced once, at construction, via an
      unexported constructor (1.3, 3.3)
- [ ] Closed domains are sum types, not strings (1.4)
- [ ] Errors are structured and layered by pipeline stage (1.5, 2.3)
- [ ] `IO` confined to the edges; core logic pure and independently
      testable (1.6, 2.9)
- [ ] Considered whether a newtype, phantom type, or GADT would close a
      hole more cheaply than a runtime check (3.2, 3.4, 3.5) — and, if
      an existential was used, justified why a closed sum couldn't (3.6)
- [ ] Single canonical IR respected if multiple formats are involved (2.1)
- [ ] Recursive structures have an explicit depth/size bound, applied
      uniformly across every stage capable of producing the nesting (2.4)
- [ ] Module exports only what's safe to export (2.5)
- [ ] No hand-rolled recursion where a named combinator applies (4.1)
- [ ] Known algorithms implemented and cited by name, not reinvented (4.2)
- [ ] Every function is one lemma, ~15–20 lines or fewer (4.5)
- [ ] Every non-trivial comment states an invariant, not a procedure (4.6)
- [ ] A named algorithm's proven library implementation used in
      preference to a from-scratch one, where a good one exists (4.7)
- [ ] Naming follows the standard convention for its category (4.8)
- [ ] Any new strictness annotation is backed by profiling evidence, not
      intuition (4.9)
- [ ] No new mutable local state where pure recursion/record-update would
      do (1.9)
- [ ] New module placed by domain, not dropped into a `Utils`/`Common`
      catch-all (2.10)
- [ ] Any new dependency is deliberate, well-maintained, and not
      duplicating an existing one (2.11)
- [ ] `type` alias not used where a `newtype` boundary was actually
      needed (3.12)
- [ ] `Applicative` used instead of `Monad` where the effects are
      genuinely independent (3.13)
- [ ] Public boundary functions use concrete types; polymorphism kept
      internal to where it earns its constraints (3.14)
- [ ] No dead code, no `undefined`/stub placeholders merged (1.11, 5.2)
- [ ] Error messages name the specific thing and location that failed;
      errors live in their own dedicated module (2.15)
- [ ] Any function over unbounded legitimate input is checked
      tail-recursive/strict-accumulator and tested on a large input (4.10)
- [ ] Checked against the named space-leak patterns — no `foldl` on a
      large structure, no repeated `(++)`, nothing holding a large
      structure alive longer than needed (4.11)
- [ ] New tests cover the boundary/empty/maximum/adversarial cases, not
      only the happy path (5.4)
- [ ] A benchmark exists and is checked in for any change on a
      performance-relevant path (5.6)
- [ ] For new prior-art-backed components, comparable projects' known
      bug classes were checked and pre-empted with a named test (5.7)
- [ ] Every claimed invariant has a matching property test (5.5)
- [ ] Self-audited: implementation re-checked against its own stated
      claim, in a separate pass (5.8)
- [ ] Any knowing deviation from this document recorded as a short ADR,
      not left silent (5.9)
