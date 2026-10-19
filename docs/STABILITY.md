# API stability

jsonlogic-hs follows the [Haskell Package Versioning Policy](https://pvp.haskell.org).

- **`JsonLogic`** is the supported API, and everything it exports is covered
  by the policy. Error constructors (`ParseError`, `EvalError`, `LimitKind`,
  `OperatorsError`, `Arity`) are part of it: match on constructors, never on
  `renderError` text, whose wording may change in any release.
- **`JsonLogic.Core.*`** is exposed for the test suites and for advanced use
  (working with the rule tree directly). It carries **no stability promise
  before 1.0**, and may change in any release.
- Behaviour that is pinned by the outlier fixture, the conformance suite or
  `docs/COMPATIBILITY.md` is not changed without a changelog entry and, for a
  deliberate deviation, a version bump that says so.
- `Config` is a record with exported fields; construct it from
  `defaultConfig` (`defaultConfig {maxSteps = 5000}`) rather than with the
  constructor, so new fields do not break your code.
