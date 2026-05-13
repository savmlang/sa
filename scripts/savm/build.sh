#! /usr/bin/bash

export SYSROOT=$HOME/sysroot/

# c compiler
export CC="clang"
export CXX="clang++"
export AR="llvm-ar"

# cc flags
export CFLAGS="--sysroot=$SYSROOT -fuse-ld=lld"
export CXXFLAGS="--sysroot=$SYSROOT -fuse-ld=lld"

# libffi
export PKG_CONFIG_SYSROOT_DIR="$SYSROOT"
export PKG_CONFIG_PATH="$SYSROOT/usr/lib/$CTARGET/pkgconfig"
export PKG_CONFIG_ALLOW_CROSS=1

# use lld linker
export RUSTFLAGS="-C linker=clang \
  -C link-arg=-fuse-ld=lld \
  -C link-arg=--target=$CTARGET \
  -C link-arg=--sysroot=$SYSROOT \
  -L target/debug \
  -L target/release \
  -L target/$TARGET/debug \
  -L target/$TARGET/release"

if [[ "$TARGET" == "armv7-unknown-linux-gnueabihf" ]]; then
  export RUSTFLAGS="$RUSTFLAGS \
    -C link-arg=-Wl,--allow-shlib-undefined"
fi

echo "Building SaVM"

cargo build \
  --workspace \
  --no-default-features \
  -Zbuild-std=std,core,proc_macro,panic_abort \
  --release \
  --target $TARGET $EXTRA