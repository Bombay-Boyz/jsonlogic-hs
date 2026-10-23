# 0007. Weeder runs in CI as advisory at first

**Status: closed (2026-10-22).** The weeder job is now blocking.

**Rule relaxed for a time:** Standard 1.11 and 5.2 make dead-code detection a
blocking check.

**What was done:** the `weeder` job in `.github/workflows/ci.yml` ran on
every push but was marked `continue-on-error`, so a finding was visible and did
not fail the build.

**Why:** weeder reads GHC's interface files and must be built with the same GHC.
It had not been run against this codebase, so a first run might report
findings that were configuration (`weeder.toml`) rather than dead code.

**What the first runs found:**

- The job exited 4, "No HIE files found", on every run. It wrote the `.hie`
  files to `-hiedir=.hie`, and weeder's file search does not descend into
  hidden directories. A shared `-hiedir` was also wrong on its own: every
  executable has a module named `Main`, so all but the last `Main.hie` were
  overwritten and their roots were never seen. The files are now left next
  to the object files in `dist-newstyle`.
- With the files found, there was one real finding:
  `JsonLogic.Core.Parse.defaultMaxDepth`, an export nothing used. It was
  deleted.

**Exit condition met:** `continue-on-error` is removed; a weeder finding
fails CI. `bash scripts/weeder.sh` runs the same check locally.
