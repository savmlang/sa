#! /usr/bin/bash

export SYSROOT=$HOME/sysroot/

# c compiler
export CC="clang"
export CXX="clang++"
export AR="llvm-ar"

# cc flags
export CFLAGS="--target=$TARGET --sysroot=$SYSROOT"
export CXXFLAGS="--target=$TARGET --sysroot=$SYSROOT"

# libffi
export PKG_CONFIG_SYSROOT_DIR="$SYSROOT"
export PKG_CONFIG_PATH="$SYSROOT/usr/lib/$CTARGET/pkgconfig"
export PKG_CONFIG_ALLOW_CROSS=1

echo "Building Rust"

cargo build -Zbuild-std --release --target $TARGET $EXTRA