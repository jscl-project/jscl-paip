#!/bin/bash
set -e

REPO_URL="https://github.com/norvig/paip-lisp.git"
TEMP_DIR=$(mktemp -d)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PAIP_DIR="$SCRIPT_DIR/paip"

echo "Cloning PAIP repository..."
git clone --depth 1 "$REPO_URL" "$TEMP_DIR"

echo "Copying Lisp source files..."
rm -rf "$PAIP_DIR"/*.lisp
cp "$TEMP_DIR/lisp"/*.lisp "$PAIP_DIR/"

echo "Cleaning up..."
rm -rf "$TEMP_DIR"

echo "Done. PAIP source files are in $PAIP_DIR"
