#!/bin/bash
# slothbar-tray-sni-smoke.sh --- live StatusNotifier integration test
#
# Runs the production SNI transport against a fake StatusNotifierItem in a
# private D-Bus session.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
EMACS=${EMACS:-emacs}
TEST_ELISP="$ROOT/test/slothbar-tray-sni-smoke.el"

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
  -L "$ROOT" -l "$TEST_ELISP"
