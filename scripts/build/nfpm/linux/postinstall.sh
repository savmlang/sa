#!/usr/bin/sh

# Every exotic way to run ldconfig
ldconfig || true
/usr/sbin/ldconfig || true
/sbin/ldconfig || true
/bin/ldconfig || true
/usr/bin/ldconfig || true