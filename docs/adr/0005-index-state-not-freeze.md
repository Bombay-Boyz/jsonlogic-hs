# 0005. A pinned `index-state` instead of `cabal.project.freeze`

**Rule interpreted:** Standard 2.12 asks for a lock file or fixed resolver.

**Decision:** `cabal.project` sets `index-state`. Every build then resolves
against the Hackage index as of that day.

**Why not a freeze file:** CI builds five GHC versions, and a freeze file
records one compiler's exact plan (boot-package versions differ), so it would
be wrong for four of them. `index-state` is the same for all of them.

**Consequence:** a package version released after the date is invisible until
the date is moved. Moving it is a deliberate change that CI tests.
