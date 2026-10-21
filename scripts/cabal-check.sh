#!/usr/bin/env bash
# `cabal check` exits 0 even when it prints warnings, so a plain call is not a
# gate. This fails on anything other than the clean message.
set -uo pipefail

output=$(cabal check 2>&1)
status=$?
echo "$output"

if [ "$status" -ne 0 ]; then
  exit "$status"
fi
if ! grep -q "No errors or warnings could be found" <<< "$output"; then
  echo "cabal check reported issues (see above): fix each one or justify it before releasing."
  exit 1
fi
