#!/usr/bin/env bash
# Runs hlint and the fourmolu format check over every Haskell file, checks the
# committed fixtures against their recorded hashes, and runs shellcheck.
# Fails on any hint or any file that is not formatted.
#
#   scripts/lint.sh          check only
#   scripts/lint.sh --fix    format the files in place first
#
# By default the pinned tool versions are downloaded into .tools/ (Linux
# x86_64 release binaries; scripts/fetch-tools.sh). On any other platform,
# point HLINT and FOURMOLU at your own builds of the same versions:
#
#   HLINT=$(which hlint) FOURMOLU=$(which fourmolu) bash scripts/lint.sh
set -euo pipefail
cd "$(dirname "$0")/.."

if [ -z "${HLINT:-}" ] || [ -z "${FOURMOLU:-}" ]; then
  bash scripts/fetch-tools.sh
fi
HLINT=${HLINT:-.tools/hlint-3.10/hlint}
FOURMOLU=${FOURMOLU:-.tools/fourmolu}

mapfile -t FILES < <(find src test conformance community tutorial differential outliers bench consumer -name '*.hs' -not -path '*/node_modules/*' | sort)

if [ "${1:-}" = "--fix" ]; then
  "$FOURMOLU" --mode inplace "${FILES[@]}"
fi

"$HLINT" "${FILES[@]}"
"$FOURMOLU" --mode check "${FILES[@]}"

# The committed fixtures are exactly the ones whose hashes are recorded. To
# change one on purpose, regenerate it, then record the new hashes in the same
# commit: bash scripts/update-fixture-hashes.sh
sha256sum -c --quiet fixtures.sha256

if command -v shellcheck >/dev/null; then
  shellcheck --severity=warning scripts/*.sh
elif [ -n "${CI:-}" ]; then
  echo "shellcheck is required in CI" >&2
  exit 1
else
  echo "shellcheck not installed: shell scripts not checked" >&2
fi
echo "lint clean: no hlint hints, formatting matches fourmolu.yaml, fixtures match fixtures.sha256"
