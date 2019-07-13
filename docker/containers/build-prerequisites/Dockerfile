# Set the base image
FROM ubuntu:disco
#FROM debian:testing-slim
# TODO: before we go to production, use a reproducible build with NixOS instead of Ubuntu

# Original author: Paul Steckler
LABEL maintainer="vedr@nlebo.com / mathieu@legilogic.com"

# Install Ubuntu packages
# NB: installing a system's ocaml is just to bootstrap opam without compiling our own ocaml
RUN apt-get update -y && \
    apt-get install -y \
    apt-utils aspcud bubblewrap cmake dh-autoreconf git \
    libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev libsnappy-dev \
    make nginx nodejs npm ocaml pkg-config rlwrap screen \
    software-properties-common sudo unzip wget zlib1g-dev zsh

# Ethereum repository, for geth and solidity (add-apt-repository depends on software-properties-common)
# avsm repository for ocaml
RUN add-apt-repository ppa:ethereum/ethereum && \
    : add-apt-repository ppa:avsm/ppa && \
    apt-get update -y && \
    apt-get install -y libsecp256k1-dev geth solc

# Install truffle environment for Solidity
#RUN npm install -g truffle

# Add and use non-root user
RUN groupadd -g 1100 appuser && \
    useradd  -m -r -u 1100 -g appuser appuser
USER appuser
WORKDIR /home/appuser

# Compiling OPAM 2.0.5
RUN wget https://github.com/ocaml/opam/releases/download/2.0.5/opam-full-2.0.5.tar.gz && \
    tar -vxzf opam-full-2.0.5.tar.gz && \
    rm opam-full-2.0.5.tar.gz && \
    cd opam-full-2.0.5 && \
    prefix=/home/appuser/opt/opam-full-2.0.5 && \
    ./configure --prefix=$prefix && \
    make lib-ext && \
    make && \
    make install LIBINSTALL_DIR=$prefix/bootstrap_lib && \
    rm -rf opam-full-2.0.5
ENV PATH /home/appuser/opt/opam-full-2.0.5/bin:${PATH}

# Install OPAM packages
#ENV OCAML_VERSION ocaml-base-compiler.4.07.1 ### <= use this format to distinguish from system-provided ocaml of same version
ENV OCAML_VERSION 4.07.1

# NB: Pinning the `integers` package is currently necessary due to this bug:
# https://github.com/ocamllabs/ocaml-integers/issues/26
#    : opam pin -y dune 1.6.3 && \

RUN opam init -a --disable-sandboxing && \
    opam switch create ${OCAML_VERSION} && \
    opam pin -y integers 0.2.2 && \
    opam install -y \
    bisect_ppx cohttp cohttp-lwt-unix cryptokit cstruct dune integers lens \
    leveldb lwt lwt_ppx ppx_blob ppx_deriving ppx_tools_versioned ppx_core \
    ppx_driver ppx_inline_test qcheck scgi secp256k1 yojson ; \
    :
ENV PATH /home/appuser/.opam/${OCAML_VERSION}/bin:${PATH}

# Install Scgi library in OPAM
#RUN git clone https://github.com/bikallem/ocaml-scgi.git && \
#    cd ocaml-scgi && \
#    git checkout bb810ad74ff0900e34eadedfb1bf5aee2066edc8 && \
#    make && \
#    opam pin -y add scgi . -n && \
#    opam install scgi
