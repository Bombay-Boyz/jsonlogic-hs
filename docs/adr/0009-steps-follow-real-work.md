# 0009. Steps are charged for work that is actually done

**Decision:** array indexing and array length cost no steps beyond the node
that asks for them; before, a lookup paid one step per array element it
passed over.

**Why:** arrays used to be Haskell lists, where `xs.9999` walks 9,999 cells, so
charging for them was what kept a rule of repeated deep lookups from doing
quadratic work. Arrays are now `Vector`s, where indexing and length are
constant time. The step limit exists to bound work (Standard 2.4), so charging
for work that no longer happens would only make limits harder to reason
about. Work that is still proportional to size is still charged: converting an
array to a string or number, building strings and arrays, walking a string,
and taking in a custom operator's result.

**Consequence:** a few rules now fit within a step budget they used to
exceed. The limit tests were updated to state the new accounting at both edges
of each boundary, and `docs/LIMITS.md` describes it.
