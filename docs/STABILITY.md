# API stability

jsonlogic-hs follows the [Haskell Package Versioning Policy](https://pvp.haskell.org).

- **`JsonLogic`** is the supported API, and everything it exports is covered
  by the policy. That includes the error vocabulary (`Error`, `ParseError`,
  `EvalError`, `LimitKind`, `OperatorsError`, `Arity`), `Location` and
  `Segment`, `Config` and `ArityMode`, and `DataUse`. Match on constructors,
  never on `renderError` text, whose wording may change in any release. The
  rendered form of a `Location` (`renderLocation`) is RFC 6901 and will not
  change.
- **`JsonLogic.Core.*`** is exposed for the test suites and for advanced use
  (working with the rule tree directly). It carries **no stability promise
  before 1.0**, and may change in any release.
- Behaviour pinned by the classic suite, the outlier fixture, the community
  score (`test/community/expected.json`) or `docs/COMPATIBILITY.md` is not
  changed without a changelog entry and, for a deliberate deviation, a version
  bump that says so.
- Which node an error's location names, and how many steps a rule costs, are
  documented (`docs/LIMITS.md`) and tested, and a change to either is noted in
  the changelog.
- `Config` is a record with exported fields; construct it from
  `defaultConfig` (`defaultConfig {maxSteps = 5000}`) rather than with the
  constructor, so new fields do not break your code.
