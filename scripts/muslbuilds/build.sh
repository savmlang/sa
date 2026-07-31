#!/usr/bin/sh

export SYSROOT=$HOME/sysroot/

# c compiler
export CC="clang"
export CXX="clang++"
export AR="llvm-ar"

GCC_VER="15.2.0"

# Find any target-specific c++ header directory inside /usr/include/c++/$GCC_VER/
TARGET_CXX_DIR=$(ls -d $SYSROOT/usr/include/c++/$GCC_VER/*-alpine-linux-musl* 2>/dev/null | head -n1)

# cc flags
export CFLAGS="--sysroot=$SYSROOT \
  -std=c11 \
  -I$SYSROOT/usr/include"

export CXXFLAGS="--sysroot=$SYSROOT \
  -I$SYSROOT/usr/include \
  -I$SYSROOT/usr/include/c++/$GCC_VER \
  ${TARGET_CXX_DIR:+-I$TARGET_CXX_DIR} \
  -stdlib=libstdc++"

export CXXSTDLIB="stdc++"
export BINDGEN_EXTRA_CLANG_ARGS="--sysroot=$SYSROOT -fuse-ld=lld --target=$CTARGET"

# libffi
export PKG_CONFIG_SYSROOT_DIR="$SYSROOT"
export PKG_CONFIG_PATH="$SYSROOT/usr/lib/$CTARGET/pkgconfig"
export PKG_CONFIG_ALLOW_CROSS=1

# sajit
export SAJIT_SYSROOT="$HOME/sysroot"

GCC_LIB_DIR=$(ls -d $SYSROOT/usr/lib/gcc/*-alpine-linux-musl*/$GCC_VER 2>/dev/null | head -n1)

# use lld linker
export RUSTFLAGS="-C linker=clang \
  -C link-arg=-fuse-ld=lld \
  -C link-arg=--target=$CTARGET \
  -C link-arg=--sysroot=$SYSROOT \
  -C link-arg=-rtlib=compiler-rt \
  -C link-arg=-unwindlib=none \
  -C target-feature=-crt-static \
  -C panic=abort \
  -L target/debug \
  -L target/release \
  -L target/$TARGET/debug \
  -L target/$TARGET/release \
  -L $SYSROOT/usr/include/c++/$GCC_VER \
  ${GCC_LIB_DIR:+-L $GCC_LIB_DIR}"

if [[ "$TARGET" == "armv7-unknown-linux-gnueabihf" ]]; then
  export RUSTFLAGS="$RUSTFLAGS \
    -C link-arg=-Wl,--allow-shlib-undefined"
fi

echo "Building C Libraries"

cargo build \
  --no-default-features \
  -Zbuild-std=std,core,proc_macro,panic_abort \
  --release \
  --target $TARGET \
  -p savmasync -p salloc
  
echo "Building SaVM Substrate"

cargo build \
  --workspace \
  --no-default-features \
  -Zbuild-std=std,core,proc_macro,panic_abort \
  --release \
  --target $TARGET $EXTRA \
  --features savm/ffi_system$FEATURES $FLAGS
