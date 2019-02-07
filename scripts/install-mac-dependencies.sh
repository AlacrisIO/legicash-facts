#!/bin/sh

# Magic Incantations for Installing Dependencies Locally on Mac OS

# Dependency: secp256k1

brew tap cuber/homebrew-libsecp256k1
brew install libsecp256k1
opam install secp256k1

# Dependency: scgi

pushd "$(git rev-parse --show-toplevel)"
mkdir -p dependencies/bikallem
mkdir -p dependencies/ocaml-ppx
cd dependencies/

pushd bikallem
git clone https://github.com/bikallem/ocaml-scgi.git && \
    cd ocaml-scgi && \
    make && \
    opam pin -y add scgi . -n && \
    opam install scgi
popd

# Dependency ppx_deriving_yojson after 3.3
# This version of ppx_deriving_yojson requires yojson 1.6.0.
# If you need to download yojson 1.6.0 and `opam ugrade yojson`
# doesn't work, try:
# opam update
# opam upgrade yojson
pushd ocaml-ppx
git clone https://github.com/ocaml-ppx/ppx_deriving_yojson.git && \
    cd ppx_deriving_yojson && \
    make && \
    opam pin -y add ppx_deriving_yojson . -n && \
    opam install ppx_deriving_yojson
popd

popd
