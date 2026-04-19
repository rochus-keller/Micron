#!/usr/bin/env bash
set -euo pipefail

SOURCES=(
  "/home/me/Entwicklung/Modules/Micron/runtime/MIC++.c"
  "/home/me/Entwicklung/Modules/Micron/runtime/mic_init_musl.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/Args+.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/Files+.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/In+.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/Input+.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/Math+.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/MathL+.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/Out+.c"
  "/home/me/Entwicklung/Modules/Micron/oakwood/Strings+.c"
)

MUSL_ROOT="/home/me/Entwicklung/Libraries/musl_i386"
OUT_NAME="libmicron.a"
BUILD_DIR="${BUILD_DIR:-./build-mic-mustatic}"

CC="${CC:-gcc}"
AR="${AR:-ar}"
RANLIB="${RANLIB:-ranlib}"

CFLAGS=(
  -m32
  -O2
  -c
  -fno-pic
  -nostdinc
  -isystem "$MUSL_ROOT/include"
)

# Ensure BUILD_DIR is absolute so paths don't break
mkdir -p "$BUILD_DIR"
BUILD_DIR="$(cd "$BUILD_DIR" && pwd)"
OBJ_DIR="$BUILD_DIR/obj"
ARCHIVE="$BUILD_DIR/$OUT_NAME"
MRI_SCRIPT="$BUILD_DIR/merge.mri"

mkdir -p "$OBJ_DIR"

if [[ ! -d "$MUSL_ROOT/include" || ! -d "$MUSL_ROOT/lib" ]]; then
  echo "error: MUSL_ROOT does not look valid: $MUSL_ROOT" >&2
  exit 1
fi

# Compile each source file
for src in "${SOURCES[@]}"; do
  if [[ ! -f "$src" ]]; then
    echo "error: source file not found: $src" >&2
    exit 1
  fi

  # Generate a safe unique object file name based on the path
  rel="${src#/}"
  safe="${rel//\//__}"
  safe="${safe//+/p}"
  obj="$OBJ_DIR/${safe%.*}.o"

  echo "==> compiling $src"
  "$CC" "${CFLAGS[@]}" "$src" -o "$obj"
done

# Remove old archive if it exists
rm -f "$ARCHIVE"

echo "==> generating ar MRI script to merge archives cleanly"
# MRI scripts allow us to combine whole archives without extracting them
# This avoids the issue where duplicate object names overwrite each other
{
  echo "CREATE $ARCHIVE"
  echo "ADDLIB $MUSL_ROOT/lib/libc.a"
  if [[ -f "$MUSL_ROOT/lib/libm.a" ]]; then
    echo "ADDLIB $MUSL_ROOT/lib/libm.a"
  fi

  # Add all newly compiled object files
  for obj in "$OBJ_DIR"/*.o; do
    echo "ADDMOD $obj"
  done

  echo "SAVE"
  echo "END"
} > "$MRI_SCRIPT"

echo "==> executing ar -M"
"$AR" -M < "$MRI_SCRIPT"

echo "==> running ranlib"
"$RANLIB" "$ARCHIVE"

echo "==> SUCCESS! Final archive created at: $ARCHIVE"
ls -lh "$ARCHIVE"
