#!/bin/sh

# script to install Tezos from sources

# Debian packages
apt-get install -y libgmp-dev
apt-get install -y libleveldb-dev
apt-get install -y libsnappy-dev

# OPAM dependencies
opam install -y calendar cohttp-lwt-unix depext ezjsonm ipaddr && \
opam install -y irmin jbuilder lwt mtime nocrypto && \
opam install -y ocp-ocamlres ocplib-endian ocplib-json-typed && \
opam install -y omake ounit re ssl stringext

# leveldb (OPAM package doesn't work with 4.06.1)
git clone https://github.com/mfp/ocaml-leveldb.git && \
    cd ocaml-leveldb && \
    omake && \
    omake install

git clone https://gitlab.com/tezos/tezos.git && \
    cd tezos && \
    make && \
    make install
