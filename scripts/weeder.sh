#!/usr/bin/env bash
# Dead-code check (Standard 1.11), the same as the CI job: build every
# component with .hie files, then run weeder over them with weeder.toml.
#
#   bash scripts/weeder.sh
#
# weeder must be built with the same GHC as the code. If it is not on PATH:
#   cabal install weeder --install-method=copy --overwrite-policy=always
set -euo pipefail
cd "$(dirname "$0")/.."

command -v weeder >/dev/null || { echo "weeder is not on PATH (see the comment at the top of this script)" >&2; exit 2; }

# No -hiedir: the files stay next to the object files. A shared directory
# would keep only one of the several Main.hie files, and weeder does not
# search hidden directories.
cabal build all -v0 --enable-tests --ghc-options=-fwrite-ide-info
weeder
echo "weeder: no dead code"
