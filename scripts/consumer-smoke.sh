#!/usr/bin/env bash
# Builds the source distribution and a separate downstream project against it,
# then runs the downstream program. It proves that the package as a user would
# receive it compiles, that the public JsonLogic module is enough to use it,
# and that the documented entry points behave. The project is built outside
# this repository, with the package's own defaults (no -Werror, no dev tools).
#
#   bash scripts/consumer-smoke.sh
set -euo pipefail
cd "$(dirname "$0")/.." || exit 1
root=$(pwd)

cabal sdist -v0
tarball=$(ls "$root"/dist-newstyle/sdist/jsonlogic-hs-*.tar.gz | head -n 1)
[ -f "$tarball" ] || { echo "no source distribution was produced" >&2; exit 1; }

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
tar -xzf "$tarball" -C "$work"
package=$(basename "$(ls -d "$work"/jsonlogic-hs-*/ | head -n 1)")
cp -r consumer "$work/consumer"

{
  printf 'packages: %s consumer\n' "$package"
  grep -E '^index-state:' cabal.project || true
} > "$work/cabal.project"

cd "$work"
cabal run -v1 jsonlogic-consumer
