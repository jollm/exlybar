#!/bin/bash
# slothbar-xwayland-smoke.sh --- run the X backend smoke test under Xwayland
#
# Boots a headless sway, starts xwayland-satellite against it, and runs
# test/slothbar-xwayland-smoke.el in a GUI Emacs on that X display.
#
# Environment:
#   EMACS                     Emacs binary (default: emacs)
#   SLOTHBAR_FONTSLOTH_DIR    optional fontsloth source directory
#   SWAY                      sway binary (default: sway)
#   XWAYLAND_SATELLITE        xwayland-satellite binary
#
# Exit status is 0 on success, 1 on test failure, and 77 when a required
# tool is missing.

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
EMACS=${EMACS:-emacs}
SWAY=${SWAY:-sway}
XWAYLAND_SATELLITE=${XWAYLAND_SATELLITE:-xwayland-satellite}
TEST_ELISP="$ROOT/test/slothbar-xwayland-smoke.el"

for tool in "$EMACS" "$SWAY" "$XWAYLAND_SATELLITE"; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "SKIP: required tool not found: $tool"
    exit 77
  fi
done
if ! command -v timeout >/dev/null 2>&1; then
  echo "SKIP: 'timeout' is required"
  exit 77
fi

if [ ! -d /tmp/.X11-unix ]; then
  if ! mkdir -p /tmp/.X11-unix; then
    echo "ERROR: cannot create /tmp/.X11-unix"
    exit 1
  fi
  chmod 1777 /tmp/.X11-unix 2>/dev/null || true
fi
if [ ! -w /tmp/.X11-unix ]; then
  echo "ERROR: /tmp/.X11-unix is not writable"
  exit 1
fi

RUNTIME=$(mktemp -d "${TMPDIR:-/tmp}/slothbar-xdg-XXXXXX") || exit 1
WORK=$(mktemp -d "${TMPDIR:-/tmp}/slothbar-xwayland-XXXXXX") || exit 1
chmod 700 "$RUNTIME"

printf 'xwayland disable\noutput HEADLESS-1 resolution 1280x720\n' \
  > "$WORK/sway.conf"

sway_pid=
satellite_pid=
cleanup() {
  if [ -n "$satellite_pid" ]; then
    kill "$satellite_pid" 2>/dev/null
  fi
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

# Try a few high display numbers.  A display can be unusable even when no
# socket file exists (for example an abstract socket left by another server),
# so verify that Xwayland actually comes up before accepting the number.
DISPLAY_NUM=
for n in $(seq 20 79); do
  if [ -e "/tmp/.X11-unix/X$n" ] || [ -e "/tmp/.X$n-lock" ]; then
    continue
  fi
  env -u DISPLAY \
    XDG_RUNTIME_DIR="$RUNTIME" \
    WAYLAND_DISPLAY="$WAYLAND_DISPLAY" \
    "$XWAYLAND_SATELLITE" ":$n" >> "$WORK/xwayland.log" 2>&1 &
  candidate_pid=$!
  ready=
  for _ in $(seq 1 100); do
    if [ -S "/tmp/.X11-unix/X$n" ]; then
      ready=1
      break
    fi
    if ! kill -0 "$candidate_pid" 2>/dev/null; then
      break
    fi
    sleep 0.1
  done
  if [ -n "$ready" ]; then
    DISPLAY_NUM=$n
    satellite_pid=$candidate_pid
    break
  fi
  kill "$candidate_pid" 2>/dev/null
  wait "$candidate_pid" 2>/dev/null
done

if [ -z "$DISPLAY_NUM" ]; then
  echo "ERROR: xwayland-satellite did not create an X display"
  sed -n '1,60p' "$WORK/xwayland.log"
  exit 1
fi

echo "Xwayland display :$DISPLAY_NUM is up"

EXTRA_LOAD_PATH=()
if [ -n "${SLOTHBAR_FONTSLOTH_DIR:-}" ]; then
  EXTRA_LOAD_PATH=(-L "$SLOTHBAR_FONTSLOTH_DIR")
fi

DISPLAY=":$DISPLAY_NUM" \
SLOTHBAR_SMOKE_RESULTS="$WORK/results.txt" \
SLOTHBAR_TEST_LOAD_PATH="${SLOTHBAR_TEST_LOAD_PATH:-}" \
  timeout 120 "$EMACS" -Q --display=":$DISPLAY_NUM" \
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
  sed -n '1,80p' "$WORK/emacs.log"
  echo "--- xwayland log"
  sed -n '1,60p' "$WORK/xwayland.log"
fi

echo "logs kept in $WORK"
exit "$status"
