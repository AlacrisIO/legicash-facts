#!/bin/sh

# script to install Tezos from sources

# Debian packages
apt-get install -y \
        libgmp-dev libleveldb-dev libsnappy-dev

# OPAM dependencies
opam install -y \
     bigstring calendar cohttp-lwt-unix depext ezjsonm ipaddr \
     irmin jbuilder lens leveldb lwt mtime nocrypto \
     ocp-ocamlres ocplib-endian ocplib-json-typed \
     omake opam-installer ounit re ssl stringext

# temporary; remove once dev ocp-json-typed becomes current
opam pin -y add ocplib-json-typed --dev

git clone https://gitlab.com/tezos/tezos.git && \
    cd tezos && \
    make && \
    make install
