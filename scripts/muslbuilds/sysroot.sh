sudo apt update && sudo apt install -y clang lld llvm symlinks qemu-user-binfmt binfmt-support

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

# Build the SYSROOT
sudo podman build --platform "$TARGET_PLATFORM" -t ahqrt-sysroot:latest $SCRIPT_DIR
ahqsysroot=$(sudo podman create --platform "$TARGET_PLATFORM" ahqrt-sysroot:latest)
sudo podman export $ahqsysroot -o $HOME/sysroot.tar.gz
sudo podman rm $ahqsysroot

# Extract sysroot
rm -rf $HOME/sysroot || true
mkdir $HOME/sysroot || true

tar -xf $HOME/sysroot.tar.gz -C $HOME/sysroot

sudo symlinks -cr $HOME/sysroot
sudo chown -R $(whoami):$(whoami) $HOME/sysroot