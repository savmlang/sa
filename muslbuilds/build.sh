#!/usr/bin/sh

WORKSPACE_DIR="../"

sudo podman run --rm --platform "$TARGET_PLATFORM" -v "$(pwd)/$WORKSPACE_DIR:/code" ahqrt-build-base:latest bash -c "
  rustup --version
  cargo --version

  ln -s /usr/bin/llvm-config-22 /usr/bin/llvm-config || true

  CXXVER=\$(basename \$(find /usr/include/c++ -maxdepth 1 -mindepth 1 -type d))

  CXXDIR=\"/usr/include/c++/\$CXXVER\"
  TARGETDIR=\$(find \"\$CXXDIR\" -mindepth 1 -maxdepth 1 -type d -name '*linux*' | head -n1)

  cd /code
    export CC=\"clang\"
    export CXX=\"clang++\"
    export AR=\"llvm22-ar\"

    export CFLAGS=\"-fuse-ld=lld\"
    export CXXFLAGS=\"-fuse-ld=lld -I\$CXXDIR -I\$TARGETDIR\"
    export BINDGEN_EXTRA_CLANG_ARGS=\"-fuse-ld=lld\"

    export RUSTFLAGS=\"-C target-feature=-crt-static -L/code/target/release\"

    echo \"Building\"
    cargo build --workspace \$CARGO --release --features savm/ffi_system$FEATURES $FLAGS
  cd /
"