#!/bin/bash
# slothbar-dbusmenu-smoke.sh --- live dbusmenu client test

set -u
ROOT=$(cd "$(dirname "$0")/.." && pwd)
EMACS=${EMACS:-emacs}
for tool in dbus-run-session "$EMACS"; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "SKIP: required tool not found: $tool"
    exit 77
  fi
done
exec dbus-run-session -- "$EMACS" -Q --batch \
  --eval "(setq load-prefer-newer t)" \
  -L "$ROOT" -l "$ROOT/test/slothbar-dbusmenu-smoke.el"
