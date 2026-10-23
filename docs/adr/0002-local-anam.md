# 0002. `anaM` is defined locally

**Status: superseded (2026-10-22).** The unfold is now `Data.Fix.unfoldFixM`
from `data-fix`, the library that already provides `Fix`, and the fold is
`Data.Fix.foldFix`. The dependency on `recursion-schemes` is gone, and with it
the reason for this record.

**Rule relaxed at the time:** Standard 4.7 prefers a proven library's
implementation of a named algorithm.

**What was done:** `JsonLogic.Core.Parse` defined the monadic anamorphism
`anaM` in four lines, because the pinned `recursion-schemes` (`>=5.2 && <5.3`)
did not export one.

**Why it changed:** the audit asked why the library depended on
`recursion-schemes` (and, through `deriving-compat`, on Template Haskell) for
one fold, one unfold and two instances. `data-fix` has both combinators under
the names `unfoldFixM` and `foldFix`, so Standard 4.7 is now met by the
smaller dependency, and `Eq1` / `Show1` for the rule tree are written with
`Foldable` and the derived instances instead of Template Haskell.
