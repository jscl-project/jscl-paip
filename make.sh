#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
JSCL_DIR="$SCRIPT_DIR/jscl"
DIST_DIR="$SCRIPT_DIR/dist"

# Clone or update JSCL
if [ -d "$JSCL_DIR" ]; then
    echo "Updating JSCL repository..."
    (
	cd "$JSCL_DIR"
	git fetch origin
	git checkout runtime-compile-application
	git pull origin runtime-compile-application
    )
else
    echo "Cloning JSCL repository..."
    git clone --branch runtime-compile-application https://github.com/jscl-project/jscl.git "$JSCL_DIR"
fi

# Build JSCL (skip if already built)
(
if [ ! -f "$JSCL_DIR/jscl.js" ]; then
    echo "Building JSCL..."
    ( cd $JSCL_DIR; ./make.sh )
else
    echo "JSCL already built, skipping..."
fi
)


mkdir -p "$DIST_DIR"

# Build jscl-worker.js
echo "Building jscl-worker.js..."
node "$JSCL_DIR/jscl.js" -e '(jscl:compile-application (list "compat/auxfns-compat.lisp" "compat/io.lisp" "compat/load.lisp" "jscl/worker/worker.lisp" "compat/start.lisp") "dist/jscl-worker.js")'

# Create dist directory and copy files
echo "Copying files to dist/..."
cp "$JSCL_DIR/jscl-worker-main.js" "$DIST_DIR/"
cp "$JSCL_DIR/jquery.js" "$DIST_DIR/"
cp "$JSCL_DIR/jqconsole.min.js" "$DIST_DIR/"
cp "$JSCL_DIR/style.css" "$DIST_DIR/"
cp "$JSCL_DIR/service-worker.js" "$DIST_DIR/"
cp "$JSCL_DIR/jscl.js" "$DIST_DIR/"


cp "index.html" "$DIST_DIR/"
cp -r "paip" "$DIST_DIR/"

echo "Done. Output files are in $DIST_DIR"
