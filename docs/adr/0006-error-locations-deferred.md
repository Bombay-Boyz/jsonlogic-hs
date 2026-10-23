# 0006. Error constructors carry no location yet

**Status: closed (2026-10-22).** Locations are implemented, before the first
release, so the stable error API never existed without them.

**Rule that was not met:** Standard 2.15 wants each message to name the input
or location that caused it.

**What was planned:** add a JSON path to each parse and evaluation error in
one change, with the version bump the PVP requires.

**What was done:** every node of a parsed rule records its JSON Pointer
(RFC 6901), and `Error` is now `ParseFailure Location ParseError` or
`EvalFailure Location EvalError`. The location is the node that failed: the
operator with the wrong argument count or the unknown name, the value that
nests too deeply, or the node that ran out of a resource. `renderError`
starts with the pointer (`at /if/1: ...`). Because 0.1.0.0 is not yet
released, no version bump was needed.

**How it is checked:** `LocationSpec` asserts, over random rules, data and
configurations, that every error's location resolves to a value in the rule,
and that for an operator error it resolves to that operator's call.
