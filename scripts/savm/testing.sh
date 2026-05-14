export SYSROOT=$HOME/sysroot/

echo "Running satest"

sudo cp -r $(pwd)/target/$TARGET/release/ $SYSROOT/root/savm/
sudo cp -r $(pwd)/target/$TARGET/release/libsalloc.so $SYSROOT/usr/lib/libsalloc.so

sudo cp -r $(pwd)/bin/satest/tests/ $SYSROOT/root/tests/

sudo chroot $SYSROOT /bin/sh -c \
  "ldconfig && cd /root && chmod +x * && ./savm/satest"
