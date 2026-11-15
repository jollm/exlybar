#!/bin/bash
# slothbar-xvfb-smoke.sh --- run the X backend smoke test under Xvfb + xmonad
#
# Boots Xvfb, starts xmonad with XMonad.Hooks.EwmhDesktops, and runs
# test/slothbar-xvfb-smoke.el in a GUI Emacs on that display.
#
# Environment:
#   EMACS                     Emacs binary (default: emacs)
#   XMONAD                    xmonad binary (default: xmonad)
#   SLOTHBAR_FONTSLOTH_DIR    optional fontsloth source directory
#
# Exit status is 0 on success, 1 on test failure, and 77 when a required
# tool is missing.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
EMACS=${EMACS:-emacs}
XMONAD=${XMONAD:-xmonad}
TEST_ELISP="$ROOT/test/slothbar-xvfb-smoke.el"

if [ "${SLOTHBAR_XVFB_SMOKE_INNER:-0}" != 1 ]; then
  for tool in xvfb-run "$EMACS" "$XMONAD"; do
    if ! command -v "$tool" >/dev/null 2>&1; then
      echo "SKIP: required tool not found: $tool"
      exit 77
    fi
  done
  exec xvfb-run -a -s "-screen 0 1280x720x24 +extension RANDR" \
    env SLOTHBAR_XVFB_SMOKE_INNER=1 bash "$0" "$@"
fi

for tool in "$EMACS" "$XMONAD" wmctrl xprop ghc timeout; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "SKIP: required tool not found: $tool"
    exit 77
  fi
done

if ! "$EMACS" -Q --batch --eval "(kill-emacs (if (featurep 'x) 0 1))" >/dev/null 2>&1; then
  echo "SKIP: $EMACS has no X11 support; set EMACS to an X-capable build"
  exit 77
fi

WORK=$(mktemp -d "${TMPDIR:-/tmp}/slothbar-xvfb-XXXXXX") || exit 1
export HOME="$WORK/home"
export XDG_DATA_HOME="$WORK/data"
export XDG_CONFIG_HOME="$WORK/config"
mkdir -p "$HOME/.xmonad" "$XDG_DATA_HOME" "$XDG_CONFIG_HOME"

cat > "$HOME/.xmonad/xmonad.hs" <<'EOF'
import XMonad
import XMonad.Hooks.EwmhDesktops

main = xmonad $ ewmh def
EOF

wm_pid=
cleanup() {
  if [ -n "$wm_pid" ]; then
    kill "$wm_pid" 2>/dev/null
  fi
  wait 2>/dev/null
}
trap cleanup EXIT

timeout 180 "$XMONAD" > "$WORK/xmonad.log" 2>&1 &
wm_pid=$!

ready=
for _ in $(seq 1 120); do
  if xprop -root _NET_SUPPORTING_WM_CHECK 2>/dev/null | grep -q "window id"; then
    ready=1
    break
  fi
  if ! kill -0 "$wm_pid" 2>/dev/null; then
    break
  fi
  sleep 0.25
done
if [ -z "$ready" ]; then
  echo "ERROR: xmonad did not expose EWMH properties"
  sed -n '1,80p' "$WORK/xmonad.log"
  exit 1
fi

echo "Xvfb display $DISPLAY is up with xmonad"

EXTRA_LOAD_PATH=()
if [ -n "${SLOTHBAR_FONTSLOTH_DIR:-}" ]; then
  EXTRA_LOAD_PATH=(-L "$SLOTHBAR_FONTSLOTH_DIR")
fi

SLOTHBAR_SMOKE_RESULTS="$WORK/results.txt" \
  timeout 120 "$EMACS" -Q --display="$DISPLAY" \
  -L "$ROOT" "${EXTRA_LOAD_PATH[@]}" \
  -l "$TEST_ELISP" > "$WORK/emacs.log" 2>&1
status=$?

echo "--- results"
if [ -f "$WORK/results.txt" ]; then
  cat "$WORK/results.txt"
else
  echo "(no results file)"
fi

if [ "$status" -ne 0 ]; then
  echo "--- emacs log"
  sed -n '1,100p' "$WORK/emacs.log"
  echo "--- xmonad log"
  sed -n '1,80p' "$WORK/xmonad.log"
fi

echo "logs kept in $WORK"
exit "$status"
