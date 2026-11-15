#!/bin/bash
# slothbar-tray-sni-probe.sh --- Phase 0 StatusNotifier probe
#
# Starts a private D-Bus session and runs the tray/SNI spike.
# This is not part of the normal test suite.
#
# Environment:
#   EMACS       Emacs binary (default: emacs)
#   ELWL_DIR    elwl checkout (default: ../elwl)
#
# Exit status is 0 on success, 1 on failure, and 77 when a tool is missing.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
EMACS=${EMACS:-emacs}
ELWL_DIR=${ELWL_DIR:-$(cd "$ROOT/.." && pwd)/elwl}

for tool in dbus-run-session "$EMACS"; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "SKIP: required tool not found: $tool"
    exit 77
  fi
done
if ! "$EMACS" -Q --batch --eval "(progn (require 'dbus) (kill-emacs 0))" >/dev/null 2>&1; then
  echo "SKIP: $EMACS has no D-Bus support"
  exit 77
fi

exec dbus-run-session -- "$EMACS" -Q --batch \
  -L "$ROOT" -L "$ELWL_DIR" \
  -l "$ROOT/test/slothbar-tray-sni-probe.el"
