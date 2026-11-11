#!/usr/bin/env bash
# Build a temporary MELPA recipe for slothbar in a Guix container and
# exercise the resulting package.  The package includes the Wayland backend;
# it must build and load without elwl, and only fail when that backend is
# explicitly requested.
set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
melpa=${MELPA_DIR:-"$repo/../melpa"}
guix=${GUIX:-guix}

if ! command -v "$guix" >/dev/null 2>&1; then
  echo "guix was not found; set GUIX or install Guix" >&2
  exit 127
fi
melpa=$(cd "$melpa" && pwd)

tmp=$(mktemp -d "${TMPDIR:-/tmp}/slothbar-melpa.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$tmp/source" "$tmp/recipes" "$tmp/packages" "$tmp/working" \
         "$tmp/home" "$tmp/cache" "$tmp/tmp"

tar -C "$repo" \
  --exclude=.git --exclude=scratch --exclude='*.elc' \
  --exclude='session-*.md' --exclude='#*#' \
  -cf - . | tar -C "$tmp/source" -xf -

git -C "$tmp/source" init -q
git -C "$tmp/source" add -A
git -C "$tmp/source" \
  -c user.name='slothbar test' \
  -c user.email='slothbar-test@example.invalid' \
  -c commit.gpgsign=false \
  commit -qm 'MELPA test snapshot'

cat > "$tmp/recipes/slothbar" <<EOF
(slothbar
 :fetcher git
 :url "file://$tmp/source"
 :files ("slothbar.el"
         "slothbar-*.el"
         "README.org"
         "CHANGELOG.org"
         "LICENSE"))
EOF

echo "Running slothbar MELPA recipe test in a Guix container..."
"$guix" shell --container --network \
  --root="$tmp/guix-profile" \
  --expose="$melpa" \
  --expose="$repo" \
  --share="$tmp" \
  emacs emacs-all-the-icons emacs-backlight emacs-compat emacs-dash \
  emacs-f emacs-fontsloth emacs-log4e emacs-nerd-icons emacs-s \
  emacs-volume emacs-xelb git coreutils tar gzip bash nss-certs \
  -- env \
     HOME="$tmp/home" \
     XDG_CACHE_HOME="$tmp/cache" \
     TMPDIR="$tmp/tmp" \
     SLOTHBAR_TEST_TMP="$tmp" \
     SLOTHBAR_TEST_MELPA="$melpa" \
     SLOTHBAR_TEST_REPO="$repo" \
     emacs -Q --batch \
       -L "$melpa/package-build" \
       -l "$repo/scripts/melpa-test.el"
