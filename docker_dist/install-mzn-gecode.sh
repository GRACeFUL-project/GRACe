#!/bin/bash

# Get dependencies
apt-get update && apt-get install -y clang cmake bison flex autoconf

# Set build prefix
mkdir -p /.local/bin
export PREFIX=/.local/

# Build MiniZinc
mkdir -p /libminizinc/build
cd /libminizinc/build
cmake -DCMAKE_INSTALL_PREFIX:PATH=$PREFIX ..
cmake --build .
cmake --build . --target install
ln -s $PREFIX/bin/mzn-fzn $PREFIX/bin/minizinc

# Build Gecode
cd /release-5.0.0
./configure --prefix=$PREFIX --enable-examples=no --enable-qt=no --enable-gist=no
make
make install
