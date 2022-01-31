#!/usr/bin/bash

set -e

# build and run compiler
stack run -- $0

# assemble
nasm -felf64 out.s

# build runtime
make -C rts

# link
gcc -static out.o rts/out/src/libcobble.o rts/out/src/init.o


echo "Compilation successful!"

