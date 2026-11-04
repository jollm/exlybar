#!/bin/bash
# slothbar-wayland-smoke.sh --- run the Wayland backend smoke test under sway
#
# Starts a headless sway, sets up a private XDG_RUNTIME_DIR, and runs
# test/slothbar-wayland-smoke.el in batch Emacs as a Wayland client.
#
# Environment:
#   EMACS                     Emacs binary (default: emacs)
#   SWAY                      sway binary (default: sway)
#   ELWL_DIR                  elwl checkout (default: ../elwl)
#   FONTSLOTH_DIR             fontsloth checkout (default: ../fontsloth)
#   FD_FAIRY_DIR              fd-fairy checkout (default: ../process-send-message)
#   FD_FAIRY_MODULE           path to libfd_fairy.so
#   SLOTHBAR_TEST_FONT        font file for ^f0
#
# Exit status is 0 on success, 1 on test failure, and 77 when a required
# tool or dependency is missing.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
EMACS=${EMACS:-emacs}
SWAY=${SWAY:-sway}
ELWL_DIR=${ELWL_DIR:-"$ROOT/../elwl"}
FONTSLOTH_DIR=${FONTSLOTH_DIR:-"$ROOT/../fontsloth"}
FD_FAIRY_DIR=${FD_FAIRY_DIR:-"$ROOT/../process-send-message"}
FD_FAIRY_MODULE=${FD_FAIRY_MODULE:-"$FD_FAIRY_DIR/target/debug/libfd_fairy.so"}
TEST_ELISP="$ROOT/test/slothbar-wayland-smoke.el"

for tool in "$EMACS" "$SWAY" fc-match timeout; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "SKIP: required tool not found: $tool"
    exit 77
  fi
done

if [ ! -f "$FD_FAIRY_MODULE" ]; then
  echo "SKIP: fd-fairy module not found: $FD_FAIRY_MODULE"
  exit 77
fi
if [ ! -f "$ELWL_DIR/elwl.el" ]; then
  echo "SKIP: elwl checkout not found: $ELWL_DIR"
  exit 77
fi

FONT=${SLOTHBAR_TEST_FONT:-$(fc-match -f '%{file}' 'DejaVu Sans Mono' 2>/dev/null)}
if [ -z "$FONT" ] || [ ! -f "$FONT" ]; then
  echo "SKIP: no test font found (set SLOTHBAR_TEST_FONT)"
  exit 77
fi

RUNTIME=$(mktemp -d "${TMPDIR:-/tmp}/slothbar-wl-xdg-XXXXXX") || exit 1
WORK=$(mktemp -d "${TMPDIR:-/tmp}/slothbar-wayland-XXXXXX") || exit 1
chmod 700 "$RUNTIME"

printf 'xwayland disable\noutput HEADLESS-1 resolution 1280x720 scale 2\n' \
  > "$WORK/sway.conf"

sway_pid=
cleanup() {
  if [ -n "$sway_pid" ]; then
    kill "$sway_pid" 2>/dev/null
  fi
  wait 2>/dev/null
}
trap cleanup EXIT

XDG_RUNTIME_DIR="$RUNTIME" \
WLR_BACKENDS=headless \
WLR_LIBINPUT_NO_DEVICES=1 \
WLR_RENDERER=pixman \
  "$SWAY" -c "$WORK/sway.conf" > "$WORK/sway.log" 2>&1 &
sway_pid=$!

WAYLAND_SOCKET=
for _ in $(seq 1 100); do
  for candidate in "$RUNTIME"/wayland-*; do
    case "$candidate" in
      *.lock) continue ;;
    esac
    if [ -S "$candidate" ]; then
      WAYLAND_SOCKET=$candidate
      break
    fi
  done
  if [ -n "$WAYLAND_SOCKET" ]; then
    break
  fi
  sleep 0.1
done
if [ -z "$WAYLAND_SOCKET" ]; then
  echo "ERROR: headless sway did not start"
  sed -n '1,40p' "$WORK/sway.log"
  exit 1
fi
WAYLAND_DISPLAY=$(basename "$WAYLAND_SOCKET")

LOAD_ARGS=(-L "$ROOT")
if [ -d "$FONTSLOTH_DIR" ]; then
  LOAD_ARGS+=(-L "$FONTSLOTH_DIR")
fi
if [ -d "$ELWL_DIR" ]; then
  LOAD_ARGS+=(-L "$ELWL_DIR")
fi
if [ -d "$FD_FAIRY_DIR" ]; then
  LOAD_ARGS+=(-L "$FD_FAIRY_DIR")
fi

echo "Wayland display $WAYLAND_DISPLAY is up"

XDG_RUNTIME_DIR="$RUNTIME" \
WAYLAND_DISPLAY="$WAYLAND_DISPLAY" \
FD_FAIRY_MODULE="$FD_FAIRY_MODULE" \
SLOTHBAR_TEST_FONT="$FONT" \
SLOTHBAR_SMOKE_RESULTS="$WORK/results.txt" \
SLOTHBAR_TEST_LOAD_PATH="${SLOTHBAR_TEST_LOAD_PATH:-}" \
  timeout 120 "$EMACS" -Q --batch "${LOAD_ARGS[@]}" \
  --eval "(setq load-prefer-newer t)" \
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
  echo "--- sway log"
  sed -n '1,40p' "$WORK/sway.log"
fi

echo "logs kept in $WORK"
exit "$status"
