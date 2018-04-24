#!/bin/sh

# script to install secp256k1 library from sources

git clone https://github.com/bitcoin-core/secp256k1.git && \
    cd secp256k1 && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-recovery && \
    make && \
    make install
