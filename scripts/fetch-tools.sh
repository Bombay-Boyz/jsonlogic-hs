#!/usr/bin/env bash
# Downloads the pinned lint and format tools into .tools/ (Linux x86_64).
# Used by scripts/lint.sh locally and by CI, so both run the same versions.
set -euo pipefail
cd "$(dirname "$0")/.."

FOURMOLU_VERSION=0.21.0.0
HLINT_VERSION=3.10

mkdir -p .tools

if [ ! -x .tools/fourmolu ]; then
  echo "fetching fourmolu ${FOURMOLU_VERSION}..."
  curl -fsSL -o .tools/fourmolu.zip \
    "https://github.com/fourmolu/fourmolu/releases/download/v${FOURMOLU_VERSION}/fourmolu-${FOURMOLU_VERSION}-linux-x86_64.zip"
  python3 -c "import zipfile; zipfile.ZipFile('.tools/fourmolu.zip').extractall('.tools/fourmolu-unpacked')"
  cp ".tools/fourmolu-unpacked/fourmolu-${FOURMOLU_VERSION}-linux-x86_64/fourmolu" .tools/fourmolu
  chmod +x .tools/fourmolu
  rm -rf .tools/fourmolu.zip .tools/fourmolu-unpacked
fi

if [ ! -x ".tools/hlint-${HLINT_VERSION}/hlint" ]; then
  echo "fetching hlint ${HLINT_VERSION}..."
  curl -fsSL -o .tools/hlint.tar.gz \
    "https://github.com/ndmitchell/hlint/releases/download/v${HLINT_VERSION}/hlint-${HLINT_VERSION}-x86_64-linux.tar.gz"
  tar -xzf .tools/hlint.tar.gz -C .tools
  rm -f .tools/hlint.tar.gz
fi
