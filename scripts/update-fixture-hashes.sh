#!/usr/bin/env bash
# Records the SHA-256 of every committed fixture in fixtures.sha256, which
# scripts/lint.sh (and so CI) checks. Run it after changing a fixture on
# purpose, and commit both together.
#
#   bash scripts/update-fixture-hashes.sh
set -euo pipefail
cd "$(dirname "$0")/.."

mapfile -t community < <(find test/community -name '*.json' | LC_ALL=C sort)
sha256sum test/tests.json test/outliers.jsonl "${community[@]}" differential/package-lock.json > fixtures.sha256
echo "fixtures.sha256: $(wc -l < fixtures.sha256) files"
