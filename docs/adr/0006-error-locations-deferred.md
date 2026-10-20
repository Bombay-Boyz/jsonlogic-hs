# 0006. Error constructors carry no location yet

**Rule not yet met:** Standard 2.15 wants each message to name the input or
location that caused it.

**State:** `DepthExceeded` and `ArityMismatch` name the limit or operator, not
where in the rule it happened. `UnknownOperator` and `InvalidOperand` name the
operator only.

**Why deferred:** adding a path to those constructors changes public API
(`docs/STABILITY.md` says the constructors are stable), and every test and
example that matches on them. It needs a version bump and its own change.

**Plan:** add a JSON-path field to each parse and evaluation error in one
change, with the major-version bump the PVP requires.
