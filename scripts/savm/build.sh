#! /usr/bin/bash

export SYSROOT=$HOME/sysroot/

# c compiler
export CC="clang"
export CXX="clang++"
export AR="llvm-ar"

# cc flags
export CFLAGS="--sysroot=$SYSROOT"
export CXXFLAGS="--sysroot=$SYSROOT"

# libffi
export PKG_CONFIG_SYSROOT_DIR="$SYSROOT"
export PKG_CONFIG_PATH="$SYSROOT/usr/lib/$CTARGET/pkgconfig"
export PKG_CONFIG_ALLOW_CROSS=1

# use lld linker
export RUSTFLAGS="-Clinker=rust-lld --sysroot=$SYSROOT"

echo "Building Rust"

cargo build --workspace --no-default-features -Zbuild-std --release --target $TARGET $EXTRA