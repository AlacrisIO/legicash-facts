#!/bin/sh

# Magic Incantations for Installing Dependencies Locally on Mac OS

# Dependency: secp256k1

brew tap cuber/homebrew-libsecp256k1
brew install libsecp256k1
opam install secp256k1

# Dependency: scgi

pushd "$(git rev-parse --show-toplevel)"
mkdir -p dependencies/bikallem
cd dependencies/bikallem
git clone https://github.com/bikallem/ocaml-scgi.git && \
    cd ocaml-scgi && \
    make && \
    opam pin -y add scgi . -n && \
    opam install scgi
popd
