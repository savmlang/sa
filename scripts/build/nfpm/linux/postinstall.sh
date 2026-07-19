#!/bin/sh

# Every exotic way to run ldconfig
ldconfig > /dev/null 2>&1 || true
/usr/sbin/ldconfig > /dev/null 2>&1 || true
/sbin/ldconfig > /dev/null 2>&1 || true
/bin/ldconfig > /dev/null 2>&1 || true
/usr/bin/ldconfig > /dev/null 2>&1 || true