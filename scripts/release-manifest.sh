#!/usr/bin/env bash
# Collects what a release is made of: the exact source distribution, its
# SHA-256, and a manifest recording the commit, versions and fixture hashes it
# was verified against. Run by .github/workflows/release.yml after every check
# has passed; safe to run locally.
#
#   bash scripts/release-manifest.sh OUTPUT_DIR [TAG]
set -euo pipefail
cd "$(dirname "$0")/.." || exit 1
root=$(pwd)

out=${1:?usage: release-manifest.sh OUTPUT_DIR [TAG]}
tag=${2:-untagged}
mkdir -p "$out"
out=$(cd "$out" && pwd)

sha256() { if command -v sha256sum >/dev/null; then sha256sum "$1"; else shasum -a 256 "$1"; fi | cut -d' ' -f1; }
json_escape() { printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g'; }

version=$(awk '/^version:/ {print $2; exit}' jsonlogic-hs.cabal)
cabal sdist -v0
tarball="$root/dist-newstyle/sdist/jsonlogic-hs-$version.tar.gz"
[ -f "$tarball" ] || { echo "expected $tarball" >&2; exit 1; }
cp "$tarball" "$out/"
name=$(basename "$tarball")

reference=$(grep -m1 -A1 '"node_modules/json-logic-js"' differential/package-lock.json | grep -o '"version": *"[^"]*"' | cut -d'"' -f4 || true)

cat > "$out/manifest.json" <<EOM
{
  "package": "jsonlogic-hs",
  "version": "$version",
  "tag": "$(json_escape "$tag")",
  "commit": "$(git rev-parse HEAD)",
  "date_utc": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "ghc": "$(json_escape "$(ghc --numeric-version)")",
  "cabal": "$(json_escape "$(cabal --numeric-version)")",
  "reference_implementation": "json-logic-js ${reference:-unknown}",
  "source_distribution": "$name",
  "source_distribution_sha256": "$(sha256 "$out/$name")",
  "fixtures_sha256": {
    "test/tests.json": "$(sha256 test/tests.json)",
    "test/outliers.jsonl": "$(sha256 test/outliers.jsonl)",
    "differential/package-lock.json": "$(sha256 differential/package-lock.json)"
  },
  "checks": ["lint", "cabal test all", "harness check", "differential run", "cabal check", "consumer smoke test"],
  "known_deviations": "docs/COMPATIBILITY.md"
}
EOM

(cd "$out" && if command -v sha256sum >/dev/null; then sha256sum "$name" manifest.json; else shasum -a 256 "$name" manifest.json; fi > SHA256SUMS)
echo "wrote $out: $name manifest.json SHA256SUMS"
