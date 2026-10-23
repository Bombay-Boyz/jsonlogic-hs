# jsonlogic-hs against other JsonLogic implementations

Run on 2026-10-23. All implementations were driven with the fixtures shipped in this repository
(`test/tests.json`, `test/outliers.jsonl`, `test/community/`), judged exactly as `docs/CONFORMANCE.md` describes.
Community-suite scoring copies compat-tables' own scorers.

## Headline results

| Implementation | Classic suite (278) | Outlier catalogue (26,980) | Community suite (1,138) |
|---|---|---|---|
| **jsonlogic-hs** (figures from its README; *not re-run here*) | 278 (100%) | 26,980 (100%, lenient mode) | 782 lenient / 839 strict |
| json-logic-js 2.0.5 (the reference) | 278 (100.0%) | 26,980 (100.0%) | 782 (68.7%) |
| json-logic-engine 5.0.7, interpreted | 278 (100.0%) | 16,475 (61.1%) | 1,119 (98.3%) |
| json-logic-engine 5.0.7, compiled | 278 (100.0%) | 16,400 (60.8%) | 1,119 (98.3%) |
| datalogic-rs 5.4.0, default features | 278 (100.0%) | 14,989 (55.6%) | 1,060 (93.1%) |
| datalogic-rs 5.4.0, all extension features | 278 (100.0%) | 14,955 (55.4%) | 1,105 (97.1%) |
| jsonlogic 0.5.1 (Rust crate) | 270 (97.1%) | 22,881 (84.8%) | 761 (66.9%) |
| jsonlogic-rs 0.5.0 (Rust crate) | 276 (99.3%) | 13,801 (51.2%) | 782 (68.7%) |
| panzi-json-logic 1.0.1 (Python) | 277 (99.6%) | 21,361 (79.2%) | 790 (69.4%) |
| json-logic-qubit 0.9.1 (Python) | 277 (99.6%) | 15,117 (56.0%) | 759 (66.7%) |

*Percentages are of each suite's cases. For jsonlogic-hs the figures are the ones in its README; they were
**not** re-run for this comparison (see "What was not done").*

How to read it:

- **Outlier catalogue = fidelity to json-logic-js 2.0.5.** Every expected answer is what the reference returned,
  so the reference scores 100% by construction. The column measures *how closely an implementation reproduces the
  reference's behaviour*, not whether its behaviour is better or worse.
- **Community suite = the JSON Logic 2.0 dialect.** This is where json-logic-engine and datalogic-rs shine
  (93-98%), and where the reference itself only gets 68.7%. jsonlogic-hs does not implement that dialect
  (782 lenient / 839 strict).
- The two columns pull in opposite directions: implementations that adopt the 2.0 dialect deliberately diverge
  from the reference, so they score low on the outlier catalogue by design, not through bugs.

## Fidelity by family (outlier catalogue)

| Family (cases) | json-logic-js | engine (interp.) | engine (compiled) | datalogic-rs (default) | datalogic-rs (all ext) | jsonlogic crate | jsonlogic-rs | panzi | qubit |
|---|---|---|---|---|---|---|---|---|---|
| arity (630) | 100% | 42% | 41% | 37% | 37% | 90% | 34% | 86% | 33% |
| unary (2,012) | 100% | 52% | 52% | 38% | 38% | 89% | 37% | 81% | 39% |
| pair (5,015) | 100% | 52% | 52% | 54% | 54% | 87% | 47% | 81% | 60% |
| triple (1,120) | 100% | 59% | 59% | 60% | 60% | 88% | 35% | 74% | 39% |
| numstr (2,376) | 100% | 60% | 60% | 20% | 20% | 80% | 52% | 75% | 49% |
| numfmt (473) | 100% | 93% | 93% | 88% | 88% | 86% | 75% | 89% | 89% |
| tie (628) | 100% | 100% | 100% | 99% | 99% | 99% | 99% | 75% | 82% |
| signedzero (2,326) | 100% | 25% | 25% | 25% | 25% | 58% | 25% | 33% | 24% |
| unicode (4,064) | 100% | 80% | 80% | 67% | 67% | 88% | 55% | 86% | 57% |
| var (3,262) | 100% | 83% | 83% | 88% | 88% | 92% | 72% | 98% | 81% |
| collection (1,486) | 100% | 71% | 70% | 74% | 74% | 88% | 49% | 90% | 76% |
| flow (291) | 100% | 74% | 64% | 77% | 77% | 52% | 39% | 84% | 71% |
| literal (75) | 100% | 73% | 73% | 65% | 65% | 63% | 44% | 88% | 63% |
| deep (34) | 100% | 100% | 100% | 100% | 100% | 100% | 100% | 100% | 100% |
| wide (36) | 100% | 100% | 100% | 100% | 100% | 100% | 100% | 100% | 86% |
| coercion (2,239) | 100% | 38% | 38% | 38% | 38% | 85% | 47% | 73% | 36% |
| community (913) | 100% | 69% | 69% | 74% | 70% | 96% | 78% | 93% | 85% |

(The `community` family here is the 913 community-suite rules judged against json-logic-js's own answers,
which is a different measure from the community-suite column above.)

## What the failures look like

- **`{"==": [0, null]}`**: the reference gives `false`. json-logic-engine, datalogic-rs and panzi give `true`.
- **`NaN` handling**: `{"/": [null, null]}` is `NaN` (serialised `null`) in the reference. json-logic-engine and
  datalogic-rs throw a `NaN` error (their documented 2.0 behaviour). panzi and qubit raise Python exceptions
  (`ZeroDivisionError`, `TypeError`); jsonlogic-rs returns an "unexpected error".
- **Empty string to number**: `{"+": [""]}` is `NaN` in the reference; json-logic-engine and datalogic-rs return `0`,
  qubit raises `ValueError`, jsonlogic-rs returns an error.
- **Unknown operators**: `{"if": [{"nonexistent": []}]}` throws in the reference; jsonlogic-rs returns the object.
- **Both Python libraries fail one classic-suite case**: `reduce` with an initial value given as a rule
  (`{"var": "start_with"}`) instead of a literal. panzi returns `NaN`, qubit raises `TypeError`. With a literal
  initial value both return the right answer.
- **Crashes and unreadable input in Rust**: the `jsonlogic` 0.5.1 crate panics on 108 outlier cases. Both `jsonlogic`
  and `jsonlogic-rs` read rules through `serde_json`, which rejects 73 fixture rules containing out-of-range
  numbers such as `1e309`; those count as failures. (datalogic-rs was fed raw JSON text and parsed all of them.)

## Caveats

1. **The fixtures are the reference's quirks.** They were generated from json-logic-js by this project's author,
   so the judging favours fidelity. That is the stated goal of jsonlogic-hs, but it is not a neutral
   correctness benchmark. Implementations aimed at the 2.0 dialect are measured against a spec they do not target.
2. **Calibration.** The harness reproduces known numbers: json-logic-js scores 278 / 26,980 / 782 (matching this
   repository's README and compat-tables), panzi 790 and qubit 759 (both match compat-tables), jsonlogic-rs 782
   (compat-tables: 781). Two do not match: the `jsonlogic` crate scores 761 here against 798 published, and
   datalogic-rs scores 1,105 against 1,138 published, so the datalogic-rs community figures here may understate
   its best configuration.
3. **datalogic-rs version.** 5.4.0 was tested, not the current 5.9.0, because 5.5.0 onwards declares a minimum Rust
   of 1.98 and the newest toolchain available here was 1.91. Results for newer releases may differ.
4. **Environment choices.** Python integers were read natively (not as doubles); Python's default recursion limit
   was left alone; json-logic-engine ran with its default options; datalogic-rs ran twice (default features, and with
   every `ext-*`, `datetime`, `error-handling` and `templating` feature on).
5. **Same hardware, no performance measurement.** Nothing here measures speed.

## What was not done

- **jsonlogic-hs itself was not run.** There is no suitable GHC in the environment and Hackage is not reachable
  from it. Its figures are the README's. The harness does confirm that the fixtures and judging rules reproduce the
  reference's published numbers, which is consistent with the README's claims.
- **The older Haskell `jsonlogic` package (0.1.0.0) was not run**, for the same reason.
- json-logic-engine 5.x exposes configurable limits (`maxDepth`, `maxArrayLength`, `maxStringLength`), so bounded
  evaluation is not unique to jsonlogic-hs. This comparison did not test whether those limits hold.

## What this supports

On these fixtures, the closest of the seven other implementations tested reproduces the reference on 84.8% of the
outlier catalogue (the `jsonlogic` Rust crate, which also panics on 108 cases), followed by panzi at 79.2%. The
README's claim of being held to the reference "more closely than any other implementation we know of" is
consistent with that. A wording that matches the evidence: *"On our 26,980-case catalogue of reference answers,
the next closest implementation we tested scores 84.8%."*

## Reproducing

The `harness/` directory holds the runners and raw per-implementation results:

```
cd harness
npm install json-logic-js@2.0.5 json-logic-engine        # JS
node run_js.mjs
python3 -m venv v_panzi && ./v_panzi/bin/pip install panzi-json-logic
./v_panzi/bin/python run_py.py "panzi-json-logic 1.0.1" results_panzi.json
python3 -m venv v_qubit && ./v_qubit/bin/pip install json-logic-qubit
./v_qubit/bin/python run_py.py "json-logic-qubit 0.9.1" results_qubit.json
python3 rs_prep_judge.py prep                            # Rust: needs rustc >= 1.85
(cd rs && cargo build --release)
./rs/target/release/rsbench datalogic cases_all.jsonl res_datalogic.jsonl
python3 rs_prep_judge.py judge "datalogic-rs 5.4.0" res_datalogic.jsonl results_datalogic.json
```

Paths inside the scripts point at `/home/claude/jsonlogic-hs/test`; edit `ROOT` to match your checkout.
