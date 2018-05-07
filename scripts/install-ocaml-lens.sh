#!/bin/sh

# script to install our patched ocaml-lens library from sources

git clone https://github.com/legicash-patches/ocaml-lens.git && \
    cd ocaml-lens && \
    jbuilder build @install && \
    jbuilder install
