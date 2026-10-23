# 0008. The community suite is a pinned score, not a pass/fail gate

**Rule interpreted:** Standard 5.4 asks for exhaustive tests that pass.

**Decision:** `test/community/` (json-logic/compat-tables, 1,138 cases) is run
by `jsonlogic-hs-community`, which fails unless every file's case count and
pass counts, in both arity modes, equal `test/community/expected.json`. It
fails on an improvement as well as on a regression.

**Why:** most of the suite describes the JSON Logic 2.0 dialect, which this
library deliberately does not implement (VISION.md §2), so "all cases pass" is
the wrong bar. Ignoring the suite would hide how far the library is from that
dialect and would let behaviour on its cases drift unnoticed. A pinned score
makes the gap a number that changes only in a commit that says so. That the
lenient score equals json-logic-js's own, file for file, is itself evidence
that lenient mode matches the reference.

**Also:** every rule of the suite is an outlier case, so lenient mode must
give json-logic-js's exact answer on each of them, not just the same score.

**Revisit:** when the 2.0 dialect is implemented, its mode should pass the
suite outright, and this becomes a gate for that mode.
