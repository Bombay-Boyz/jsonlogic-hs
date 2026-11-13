#!/usr/bin/env bash
# Builds and tests the package exactly as a Hackage user would receive it:
# from the sdist tarball, in a scratch directory outside the repository, with
# the package's default flags (no -Werror, no differential tool).
#
# Run it before every upload:  bash scripts/verify-sdist.sh
set -euo pipefail
cd "$(dirname "$0")/.."
REPO=$(pwd)

VERSION=$(awk '/^version:/ {print $2; exit}' jsonlogic-hs.cabal)
cabal sdist
TARBALL="dist-newstyle/sdist/jsonlogic-hs-$VERSION.tar.gz"
echo "tarball: $TARBALL ($(wc -c < "$TARBALL") bytes)"

# Read the listing once: piping tar straight into `grep -q` makes tar die of a
# broken pipe, which `pipefail` then reports as a failure.
LISTING=$(tar -tzf "$TARBALL" | sort)

echo "--- contents ---"
echo "$LISTING"

echo "--- required files ---"
for f in LICENSE README.md CHANGELOG.md jsonlogic-hs.cabal src/JsonLogic.hs test/tests.json test/outliers.jsonl test/community/index.json test/community/expected.json test/community/LICENSE; do
  if grep -q "/$f\$" <<< "$LISTING"; then
    echo "present: $f"
  else
    echo "MISSING from the tarball: $f"
    exit 1
  fi
done

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
tar -xzf "$TARBALL" -C "$WORK"
cd "$WORK/jsonlogic-hs-$VERSION"

echo "--- building from the tarball, with the package's default flags ---"
cabal build all --enable-tests
cabal test all --test-show-details=direct
bash "$REPO/scripts/cabal-check.sh"
cabal haddock jsonlogic-hs

echo "sdist verified: builds, tests pass, cabal check and haddock are clean"
