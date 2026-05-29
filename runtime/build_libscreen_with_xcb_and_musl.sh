#!/bin/bash
# Build script for libscreen_linux_i386.a
# Creates a static library containing ScreenXcb.c + minimal libxcb core
# for both glibc and musl i386 targets.
#
# Prerequisites (Debian/Ubuntu):
#   sudo apt-get install gcc gcc-multilib python3
#
# For musl build, you need musl-i386 installed. See build_musl_i386.sh
# or set MUSL_PREFIX to point to your existing musl-i386 installation.

set -euo pipefail

BUILD_DIR="./build-mic-screen"
MUSL_PREFIX="/home/me/Entwicklung/Libraries/musl_i386"

CC="${CC:-gcc}"
AR="${AR:-ar}"
RANLIB="${RANLIB:-ranlib}"

# Directories
LIBXCB_SRC="../thirdparty/xcb"
INCLUDE_DIR="../thirdparty"
SCREEN_SRC="../oakwood/ScreenXcb.c"

# Source files (order doesn't matter for archive)
XCB_SOURCES=(
    "$LIBXCB_SRC/xcb_auth.c"
    "$LIBXCB_SRC/xcb_conn.c"
    "$LIBXCB_SRC/xcb_ext.c"
    "$LIBXCB_SRC/xcb_in.c"
    "$LIBXCB_SRC/xcb_list.c"
    "$LIBXCB_SRC/xcb_out.c"
    "$LIBXCB_SRC/xcb_util.c"
    "$LIBXCB_SRC/xcb_xid.c"
    "$LIBXCB_SRC/xproto.c"
    "$LIBXCB_SRC/bigreq.c"
    "$LIBXCB_SRC/xc_misc.c"
)

# Common flags
COMMON_FLAGS="-m32 -O2 -fno-pic -c -DHAVE_CONFIG_H -I$LIBXCB_SRC -I$INCLUDE_DIR -I$INCLUDE_DIR/xcb"

build_musl() {
    echo ""
    echo "=== Building libscreen_linux_i386.a (musl) ==="

    if [[ ! -d "$MUSL_PREFIX/include" ]]; then
        echo "error: MUSL_PREFIX does not look valid: $MUSL_PREFIX" >&2
        echo "  Set MUSL_PREFIX to your musl-i386 installation or run build_musl_i386.sh first." >&2
        exit 1
    fi

    local OBJ_DIR="$BUILD_DIR/obj-musl"
    local MUSL_FLAGS="-nostdinc -isystem $MUSL_PREFIX/include -fno-stack-protector"
    mkdir -p "$OBJ_DIR"

    for src in "${XCB_SOURCES[@]}"; do
        local base=$(basename "$src" .c)
        echo "  $base.c"
        $CC $COMMON_FLAGS $MUSL_FLAGS "$src" -o "$OBJ_DIR/$base.o"
    done

    echo "  ScreenXcb.c"
    $CC -m32 -O2 -fno-pic -c $MUSL_FLAGS -I"$INCLUDE_DIR" "$SCREEN_SRC" -o "$OBJ_DIR/ScreenXcb.o"

    local ARCHIVE="$BUILD_DIR/libscreen_linux_i386.a"
    rm -f "$ARCHIVE"
    $AR rcs "$ARCHIVE" "$OBJ_DIR"/*.o
    $RANLIB "$ARCHIVE"
    echo "  => $ARCHIVE ($(du -h "$ARCHIVE" | cut -f1))"
}

# ============================================================
# Main
# ============================================================
mkdir -p "$BUILD_DIR"
build_musl

echo ""
echo "=== SUCCESS ==="
echo "Library:"
echo "  $BUILD_DIR/libscreen_linux_i386.a        (for static linking with musl)"
echo ""
echo "To use with micc's ELF linker:"
echo "  micc ... -a x86 --cdecl -L $BUILD_DIR -n screen_linux_i386 -n c -n gcc ..."
