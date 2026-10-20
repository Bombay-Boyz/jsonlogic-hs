# 0002. `anaM` is defined locally

**Rule relaxed:** Standard 4.7 prefers a proven library's implementation of a
named algorithm.

**What is done instead:** `JsonLogic.Core.Parse` defines the monadic
anamorphism `anaM` in four lines.

**Why:** the pinned `recursion-schemes` (`>=5.2 && <5.3`) did not export an
`anaM` when this was checked (October 2026, against the library's `master`
source). The local definition is the standard unfold, `anaM f = go where go s =
f s >>= fmap Fix . traverse go`.

**Revisit:** if a later `recursion-schemes` exports it, delete the local one.
