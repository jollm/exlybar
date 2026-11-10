#!/bin/bash
# slothbar-font-cache-benchmark.sh --- measure cold font loading
#
# Runs test/slothbar-font-cache-benchmark.el with an isolated pcache.
#
# Environment:
#   EMACS                     Emacs binary (default: emacs)
#   SLOTHBAR_FONTSLOTH_DIR    fontsloth source directory (default: ../fontsloth)

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
EMACS=${EMACS:-emacs}
FONTSLOTH=${SLOTHBAR_FONTSLOTH_DIR:-"$ROOT/../fontsloth"}

if ! command -v "$EMACS" >/dev/null 2>&1; then
  echo "SKIP: Emacs not found: $EMACS"
  exit 77
fi

timeout 300 "$EMACS" -Q --batch \
  --eval "(setq load-prefer-newer t)" \
  -L "$ROOT" -L "$FONTSLOTH" \
  -l "$ROOT/test/slothbar-font-cache-benchmark.el"
