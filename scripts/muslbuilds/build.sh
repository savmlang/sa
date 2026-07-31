#!/usr/bin/sh

export SYSROOT=$HOME/sysroot/

# c compiler
export CC="clang"
export CXX="clang++"
export AR="llvm-ar"

# cc flags
export CFLAGS="--sysroot=$SYSROOT -fuse-ld=lld -stdlib=libc++ -rtlib=compiler-rt -unwindlib=none"
export CXXFLAGS="--sysroot=$SYSROOT -fuse-ld=lld -stdlib=libc++ -rtlib=compiler-rt -unwindlib=none"
export CXXSTDLIB="c++"
export BINDGEN_EXTRA_CLANG_ARGS="--sysroot=$SYSROOT -fuse-ld=lld --target=$CTARGET"

# libffi
export PKG_CONFIG_SYSROOT_DIR="$SYSROOT"
export PKG_CONFIG_PATH="$SYSROOT/usr/lib/$CTARGET/pkgconfig"
export PKG_CONFIG_ALLOW_CROSS=1

# sajit
export SAJIT_SYSROOT="$HOME/sysroot"

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
  -L target/$TARGET/release"

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
