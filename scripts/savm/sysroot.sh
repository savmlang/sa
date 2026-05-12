#! /usr/bin/bash

sudo apt update && sudo apt install -y debootstrap symlinks qemu-user-static binfmt-support

export SYSROOT=$HOME/sysroot/
export UBUNTU=noble

# Setup SYSROOT
rm -rf $SYSROOT || true
mkdir -p $SYSROOT || true
sudo debootstrap --arch=$ARCH --variant=minbase $UBUNTU $SYSROOT http://ports.ubuntu.com/ubuntu-ports

# Install Dependencies
sudo chroot $SYSROOT /bin/sh -c \
  "apt-get update && apt-get install -y libc6-dev gcc g++ build-essential"

# Resolve Symlinks
sudo symlinks -cr $SYSROOT
sudo chown -R $(whoami):$(whoami) $SYSROOT