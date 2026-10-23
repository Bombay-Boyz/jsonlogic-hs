# 0004. `error` in test fixture helpers

**Rule relaxed:** Standard 5.2 bans `error`.

**What is done instead:** `.hlint.yaml` bans it everywhere except four test
modules (`CompatSpec`, `EvalSpec`, `LimitsSpec`, and the shared `Support`), where small helpers build a
rule or an operator registry from a literal in the test and call `error` if the
literal is malformed.

**Why:** a malformed literal is a bug in the test, not a runtime condition, and
the failure should stop the test with the literal in the message. Threading an
`Either` through every example adds noise and hides nothing the type checker
could catch.

**Not relaxed:** the library, the benchmark, the conformance and outlier
runners, and the differential tool.
