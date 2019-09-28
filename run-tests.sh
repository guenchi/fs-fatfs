#!/bin/sh
set -e

dd if=/dev/zero of=test.img bs=512 count=2880
mformat -f 1440 -i test.img
mcopy -i test.img README.md ::
tests/test-fatfs.sps

if command -v loko 2>/dev/null; then
    loko --compile tests/test-fatfs.sps --output tests/test-fatfs
    tests/test-fatfs
fi
