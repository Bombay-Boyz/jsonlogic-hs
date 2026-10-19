#!/usr/bin/env bash
# Runs hlint and the fourmolu format check over every Haskell file.
# Fails on any hint or any file that is not formatted.
#   scripts/lint.sh          check only
#   scripts/lint.sh --fix    format the files in place first
set -euo pipefail
cd "$(dirname "$0")/.."
bash scripts/fetch-tools.sh

FILES=$(find src test conformance differential outliers bench -name '*.hs' -not -path '*/node_modules/*' | sort)
HLINT=".tools/hlint-3.10/hlint"

if [ "${1:-}" = "--fix" ]; then
  .tools/fourmolu --mode inplace $FILES
fi

$HLINT $FILES
.tools/fourmolu --mode check $FILES
echo "lint clean: no hlint hints, formatting matches fourmolu.yaml"
