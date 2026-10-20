# 0007. Weeder runs in CI as advisory at first

**Rule relaxed for now:** Standard 1.11 and 5.2 make dead-code detection a
blocking check.

**What is done instead:** the `weeder` job in `.github/workflows/ci.yml` runs on
every push but is marked `continue-on-error`, so a finding is visible and does
not fail the build.

**Why:** weeder reads GHC's interface files and must be built with the same GHC.
It has not yet been run against this codebase, so a first run may report
findings that are configuration (`weeder.toml`) rather than dead code, or may
fail to build on the newest compiler.

**Exit condition:** once the job has passed on `main`, remove
`continue-on-error` in the same change that fixes or justifies any finding.
