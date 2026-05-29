#!/bin/bash
# Build musl-i386 from source (one-time setup)
#
# Prerequisites:
#   sudo apt-get install gcc gcc-multilib make wget
#
# Creates a musl installation at MUSL_PREFIX (default: ~/musl-i386)
# containing headers and libc.a for i386 static linking.

set -euo pipefail

MUSL_PREFIX="${MUSL_PREFIX:-$HOME/musl-i386}"
MUSL_VERSION="${MUSL_VERSION:-1.2.4}"
BUILD_DIR="${BUILD_DIR:-/tmp/musl-build-$$}"

if [ -f "$MUSL_PREFIX/lib/libc.a" ]; then
    echo "musl-i386 already installed at $MUSL_PREFIX"
    echo "  libc.a: $(ls -lh "$MUSL_PREFIX/lib/libc.a" | awk '{print $5}')"
    exit 0
fi

echo "=== Building musl-$MUSL_VERSION for i386 ==="
echo "  Target: $MUSL_PREFIX"
echo ""

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

if [ ! -f "musl-$MUSL_VERSION.tar.gz" ]; then
    echo "==> Downloading musl-$MUSL_VERSION..."
    wget -q "https://musl.libc.org/releases/musl-$MUSL_VERSION.tar.gz"
fi

echo "==> Extracting..."
tar xzf "musl-$MUSL_VERSION.tar.gz"
cd "musl-$MUSL_VERSION"

echo "==> Configuring..."
CC=gcc CFLAGS="-m32" AR=ar RANLIB=ranlib \
    ./configure --prefix="$MUSL_PREFIX" --target=i386 --disable-shared

echo "==> Building (this may take a minute)..."
make -j$(nproc)

echo "==> Installing..."
make install

echo ""
echo "=== SUCCESS ==="
echo "  musl-i386 installed to: $MUSL_PREFIX"
echo "  libc.a: $(ls -lh "$MUSL_PREFIX/lib/libc.a" | awk '{print $5}')"
echo ""
echo "  To use:"
echo "    gcc -m32 -nostdinc -isystem $MUSL_PREFIX/include -fno-stack-protector -c file.c"

# Cleanup
cd /
rm -rf "$BUILD_DIR"
