# Differential test against the reference

Runs thousands of randomly generated rules through this library and through
the real JavaScript implementation ([json-logic-js](https://github.com/jwadhams/json-logic-js),
via Node), and lists every case where the answers differ.

## Setup (once)

```
cd differential && npm install
```

Needs Node.js. Not part of `cabal test`, because that must work without it.

## Run

```
cabal run jsonlogic-hs-differential -- [COUNT] [SEED]
```

Defaults: 2000 cases, seed 1. The same count and seed always generate the
same cases, so a mismatch can be reproduced. The first dozen mismatches are
printed; all of them go to `differential/out/mismatches.txt`.

## Reading a mismatch

Rules are generated with argument counts this library accepts, and avoid
comparing two arrays or objects for equality, so a mismatch is a real
behavioural difference rather than one of the documented deviations
(`VISION.md` §11). Each is either a bug here, or a new deviation to decide
on and record.
