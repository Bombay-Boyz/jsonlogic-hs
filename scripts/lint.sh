#!/usr/bin/env bash
# Runs hlint and the fourmolu format check over every Haskell file.
# Fails on any hint or any file that is not formatted.
#   scripts/lint.sh          check only
#   scripts/lint.sh --fix    format the files in place first
set -euo pipefail
cd "$(dirname "$0")/.."
bash scripts/fetch-tools.sh

FILES=$(find src test conformance differential outliers bench consumer -name '*.hs' -not -path '*/node_modules/*' | sort)
HLINT=".tools/hlint-3.10/hlint"

if [ "${1:-}" = "--fix" ]; then
  .tools/fourmolu --mode inplace $FILES
fi

$HLINT $FILES
.tools/fourmolu --mode check $FILES

# The committed fixtures are exactly the ones whose hashes are recorded. To
# change one on purpose, regenerate it, then update the hash in the same commit:
#   sha256sum test/tests.json test/outliers.jsonl differential/package-lock.json > fixtures.sha256
sha256sum -c --quiet fixtures.sha256

if command -v shellcheck >/dev/null; then
  shellcheck --severity=warning scripts/*.sh
else
  echo "shellcheck not installed: shell scripts not checked" >&2
fi
echo "lint clean: no hlint hints, formatting matches fourmolu.yaml, fixtures match fixtures.sha256"
