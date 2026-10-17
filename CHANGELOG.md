# Changelog for jsonlogic-hs

## 0.1.0.0 -- unreleased

First release.

* Reads JsonLogic rules from JSON into a closed, exhaustively matched tree,
  with a depth bound (default 64) so a hostile, deeply nested rule is
  rejected before anything evaluates it.
* Evaluates every builtin operator of the classic JsonLogic specification,
  with JavaScript's coercion rules, including `NaN`, infinities, and
  `undefined`, and with JavaScript-identical number parsing and printing.
* `apply` and `applyWith` as the public API; custom operators are supplied by
  the caller.
* Passes all 278 cases of the classic shared test suite, and agrees with
  `json-logic-js` 2.0.5 on 100,000 randomly generated rules.
* A small number of deliberate differences from the reference, listed in the
  README.
