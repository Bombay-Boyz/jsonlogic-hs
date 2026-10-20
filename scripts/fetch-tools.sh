#!/usr/bin/env bash
# Downloads the pinned lint and format tools into .tools/ (Linux x86_64).
# Used by scripts/lint.sh locally and by CI, so both run the same versions.
set -euo pipefail
cd "$(dirname "$0")/.."

FOURMOLU_VERSION=0.21.0.0
HLINT_VERSION=3.10

# SHA-256 of each release asset, so a re-pointed or tampered download fails
# here instead of running in CI. Update them together with the versions.
FOURMOLU_SHA256=aadf6931653df7b96f52014cf8eb4285a3e8778cd0b531499fc3ee013e1e00c7
HLINT_SHA256=ccabc8802a58154699a3583b8dddc5ea2e6d65753a62c45c0e80088ebb16b42b

verify() { # file, expected sha256
  echo "$2  $1" | sha256sum -c - >/dev/null || { echo "checksum mismatch for $1" >&2; rm -f "$1"; exit 1; }
}

mkdir -p .tools

if [ ! -x .tools/fourmolu ]; then
  echo "fetching fourmolu ${FOURMOLU_VERSION}..."
  curl -fsSL -o .tools/fourmolu.zip \
    "https://github.com/fourmolu/fourmolu/releases/download/v${FOURMOLU_VERSION}/fourmolu-${FOURMOLU_VERSION}-linux-x86_64.zip"
  verify .tools/fourmolu.zip "$FOURMOLU_SHA256"
  python3 -c "import zipfile; zipfile.ZipFile('.tools/fourmolu.zip').extractall('.tools/fourmolu-unpacked')"
  cp ".tools/fourmolu-unpacked/fourmolu-${FOURMOLU_VERSION}-linux-x86_64/fourmolu" .tools/fourmolu
  chmod +x .tools/fourmolu
  rm -rf .tools/fourmolu.zip .tools/fourmolu-unpacked
fi

if [ ! -x ".tools/hlint-${HLINT_VERSION}/hlint" ]; then
  echo "fetching hlint ${HLINT_VERSION}..."
  curl -fsSL -o .tools/hlint.tar.gz \
    "https://github.com/ndmitchell/hlint/releases/download/v${HLINT_VERSION}/hlint-${HLINT_VERSION}-x86_64-linux.tar.gz"
  verify .tools/hlint.tar.gz "$HLINT_SHA256"
  tar -xzf .tools/hlint.tar.gz -C .tools
  rm -f .tools/hlint.tar.gz
fi
