# 0001. `-Werror` is a flag, on the primary compiler only

**Rule relaxed:** Standard 5.1 makes `-Werror` mandatory.

**What is done instead:** `-Werror` is the `werror` cabal flag. `cabal.project`
turns it on for our builds; CI turns it off for every GHC except 9.14.1.

**Why:** `cabal check` rejects an unconditional `-Werror` in a released package,
and a newer GHC adds warnings that would break downstream installs of an
unchanged release. The five-compiler CI matrix would also fail on warnings that
exist in only some versions.

**Tried first:** unconditional `-Werror` (fails `cabal check`).

**Consequence:** a warning that appears only on an older GHC is not caught as an
error. The primary compiler is the one that gates merges.
