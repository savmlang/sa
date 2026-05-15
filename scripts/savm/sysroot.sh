#! /usr/bin/bash

sudo apt update && sudo apt install -y clang lld llvm debootstrap symlinks qemu-user-static binfmt-support

export SYSROOT=$HOME/sysroot/
export DISTRO=trixie

# Setup SYSROOT
rm -rf $SYSROOT || true
mkdir -p $SYSROOT || true
sudo debootstrap --arch=$ARCH --variant=minbase $DISTRO $SYSROOT https://deb.debian.org/debian

# Install Dependencies
sudo chroot $SYSROOT /bin/sh -c \
  "apt-get update && apt-get install -y libc6-dev gcc g++ build-essential libstdc++-14-dev"

# Resolve Symlinks
sudo symlinks -cr $SYSROOT
sudo chown -R $(whoami):$(whoami) $SYSROOT