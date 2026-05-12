#! /usr/bin/bash

sudo apt update && sudo apt install -y debootstrap symlinks qemu-user-static

export SYSROOT=$HOME/sysroot/
export UBUNTU=noble

# Setup SYSROOT
rm -rf $SYSROOT || true
mkdir -p $SYSROOT || true
sudo debootstrap --arch=$ARCH --variant=minbase $UBUNTU $SYSROOT http://ports.ubuntu.com/ubuntu-ports

# Copy QEMU
sudo cp /usr/bin/qemu-aarch64-static $SYSROOT/usr/bin/

# Install Dependencies
sudo chroot $SYSROOT /usr/bin/qemu-aarch64-static /bin/sh -c \
  "apt-get update && apt-get install -y libffi-dev libtinfo-dev zlib1g-dev llvm-dev"

# Resolve Symlinks
sudo symlinks -cr $SYSROOT
sudo chown -R $(whoami):$(whoami) $SYSROOT