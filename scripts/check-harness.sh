#!/usr/bin/env bash
# Checks the checkers: that the conformance and outlier runners fail when
# they should, and that the committed outlier fixture is current.
#
#   bash scripts/check-harness.sh
#
# A test runner that cannot fail is worse than none. This script feeds each
# runner a missing, empty, truncated and tampered fixture and demands a
# non-zero exit every time, then checks that test/outliers.jsonl is exactly
# what differential/outliers.js generates (needs Node and `npm ci` in
# differential/; skipped with a notice if Node is absent unless CI is set).
#
# Binaries are found with `cabal list-bin`; override with CONFORMANCE_BIN,
# OUTLIERS_BIN and COMMUNITY_BIN.
set -uo pipefail
cd "$(dirname "$0")/.." || exit 1
ROOT=$(pwd)

bin() { # name
  if [ "$1" = conformance ] && [ -n "${CONFORMANCE_BIN:-}" ]; then echo "$CONFORMANCE_BIN"; return; fi
  if [ "$1" = outliers ] && [ -n "${OUTLIERS_BIN:-}" ]; then echo "$OUTLIERS_BIN"; return; fi
  if [ "$1" = community ] && [ -n "${COMMUNITY_BIN:-}" ]; then echo "$COMMUNITY_BIN"; return; fi
  cabal build -v0 "test:jsonlogic-hs-$1" >&2 && cabal list-bin "test:jsonlogic-hs-$1"
}
CONF=$(bin conformance) || { echo "cannot build the conformance runner"; exit 1; }
OUT=$(bin outliers) || { echo "cannot build the outlier runner"; exit 1; }
COMM=$(bin community) || { echo "cannot build the community runner"; exit 1; }

failures=0
expect_fail() { # description, command...
  local what=$1; shift
  if "$@" >/dev/null 2>&1; then
    echo "FAIL: $what: the runner exited 0"; failures=$((failures + 1))
  else
    echo "ok:   $what"
  fi
}

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/test"

# the positive controls: the real fixtures pass
if "$CONF" >/dev/null 2>&1; then echo "ok:   conformance passes on the real suite"; else echo "FAIL: conformance fails on the real suite"; failures=$((failures + 1)); fi
if "$OUT" "$ROOT/test/outliers.jsonl" >/dev/null 2>&1; then echo "ok:   outliers pass on the real fixture"; else echo "FAIL: outliers fail on the real fixture"; failures=$((failures + 1)); fi

# conformance: it reads test/tests.json relative to the working directory
run_conf() { (cd "$tmp" && "$CONF"); }
rm -f "$tmp/test/tests.json"
expect_fail "conformance: fixture missing" run_conf
echo '[]' > "$tmp/test/tests.json"
expect_fail "conformance: fixture empty" run_conf
python3 - "$ROOT/test/tests.json" "$tmp/test/tests.json" truncate <<'PY'
import json, sys
d = json.load(open(sys.argv[1])); json.dump(d[:-3], open(sys.argv[2], "w"))
PY
expect_fail "conformance: fixture truncated" run_conf
python3 - "$ROOT/test/tests.json" "$tmp/test/tests.json" <<'PY'
import json, sys
d = json.load(open(sys.argv[1])); d.append(5); json.dump(d, open(sys.argv[2], "w"))
PY
expect_fail "conformance: a malformed entry" run_conf
python3 - "$ROOT/test/tests.json" "$tmp/test/tests.json" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
for x in d:
    if isinstance(x, list):
        x[2] = "WRONG"; break
json.dump(d, open(sys.argv[2], "w"))
PY
expect_fail "conformance: a changed expectation" run_conf

# outliers
expect_fail "outliers: fixture missing" "$OUT" "$tmp/nonexistent.jsonl"
: > "$tmp/empty.jsonl"
expect_fail "outliers: fixture empty" "$OUT" "$tmp/empty.jsonl"
head -n 500 "$ROOT/test/outliers.jsonl" > "$tmp/trunc.jsonl"
expect_fail "outliers: fixture truncated" "$OUT" "$tmp/trunc.jsonl"
python3 - "$ROOT/test/outliers.jsonl" "$tmp/tamper.jsonl" <<'PY'
import sys
lines = open(sys.argv[1], encoding="utf-8").read().split("\n")
# change one expected answer
for i, l in enumerate(lines):
    if '"expect":{"ok":true}' in l:
        lines[i] = l.replace('"expect":{"ok":true}', '"expect":{"ok":false}', 1); break
open(sys.argv[2], "w", encoding="utf-8").write("\n".join(lines))
PY
expect_fail "outliers: a tampered expectation" "$OUT" "$tmp/tamper.jsonl"
python3 - "$ROOT/test/outliers.jsonl" "$tmp/nocover.jsonl" <<'PY'
import sys, json
lines = open(sys.argv[1], encoding="utf-8").read().split("\n")
meta = json.loads(lines[0]); body = [l for l in lines[1:] if l and '"op":"substr"' not in l]
meta["meta"]["cases"] = len(body)
open(sys.argv[2], "w", encoding="utf-8").write(json.dumps(meta, separators=(",", ":")) + "\n" + "\n".join(body) + "\n")
PY
expect_fail "outliers: an operator with no cases" "$OUT" "$tmp/nocover.jsonl"

# community: it reads test/community/ relative to the working directory
if "$COMM" >/dev/null 2>&1; then echo "ok:   community score matches expected.json"; else echo "FAIL: community score differs on the real suite"; failures=$((failures + 1)); fi
run_comm() { (cd "$tmp/c" && "$COMM"); }
fresh_comm() { rm -rf "$tmp/c" && mkdir -p "$tmp/c/test" && cp -r "$ROOT/test/community" "$tmp/c/test/community"; }
fresh_comm; rm "$tmp/c/test/community/index.json"
expect_fail "community: index missing" run_comm
fresh_comm; echo '[]' > "$tmp/c/test/community/truthiness.json"
expect_fail "community: a suite file with no cases" run_comm
fresh_comm; python3 - "$tmp/c/test/community/expected.json" <<'PY'
import json, sys
d = json.load(open(sys.argv[1])); d["truthiness.json"]["lenient"] += 1; json.dump(d, open(sys.argv[1], "w"))
PY
expect_fail "community: a changed pinned score" run_comm
fresh_comm; python3 - "$tmp/c/test/community/expected.json" <<'PY'
import json, sys
d = json.load(open(sys.argv[1])); del d["val.json"]; json.dump(d, open(sys.argv[1], "w"))
PY
expect_fail "community: a suite file missing from expected.json" run_comm

# the committed fixture is what the generator writes
if command -v node >/dev/null 2>&1 && [ -d differential/node_modules/json-logic-js ]; then
  if (cd differential && node outliers.js --check ../test/outliers.jsonl) 2>&1; then
    echo "ok:   test/outliers.jsonl is up to date"
  else
    echo "FAIL: test/outliers.jsonl is stale"; failures=$((failures + 1))
  fi
elif [ -n "${CI:-}" ]; then
  echo "FAIL: Node or differential/node_modules is missing in CI"; failures=$((failures + 1))
else
  echo "skip: fixture freshness check (run \`cd differential && npm ci\` first)"
fi

if [ "$failures" -ne 0 ]; then echo "$failures check(s) failed"; exit 1; fi
echo "harness checks passed"
