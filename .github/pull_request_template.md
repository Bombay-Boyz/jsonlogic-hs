## What and why

<!-- One paragraph. Link the issue or the roadmap item. -->

## Checks

- [ ] `bash scripts/lint.sh` is clean (hlint, fourmolu, fixture hashes, shellcheck)
- [ ] `cabal test all` passes
- [ ] A claimed invariant has a property test in this change (Standard 5.5)
- [ ] New or changed behaviour has boundary cases: empty, one, at the limit, one over (Standard 5.4)
- [ ] A change to the evaluator or to a path that matters for speed: `bench-compare` run, result linked
- [ ] A change to a public type or function: noted in `CHANGELOG.md`, and the version bump follows the PVP
- [ ] A knowing deviation from `ENGINEERING_STANDARD.md` has an ADR in `docs/adr/`
- [ ] An expected output was changed only with evidence from json-logic-js, never to make a test pass
