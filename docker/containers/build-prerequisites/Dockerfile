# Set the base image
FROM ubuntu
# TODO: before we go to production, use a reproducible build with NixOS instead of Ubuntu

# Original author: Paul Steckler
MAINTAINER Mathieu Dutour Sikiric <mathieu@legilogic.com>

# Install Ubuntu packages
# NB: installing a system's ocaml is just to bootstrap opam without compiling our own ocaml
RUN apt-get update -y && \
    apt-get install -y \
    aspcud bubblewrap cmake dh-autoreconf git \
    libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev libsnappy-dev \
    make nginx nodejs npm ocaml pkg-config rlwrap screen \
    software-properties-common sudo unzip wget zlib1g-dev zsh

# Ethereum repository, for geth and solidity (add-apt-repository depends on software-properties-common)
# avsm repository for ocaml
RUN add-apt-repository ppa:ethereum/ethereum && \
    add-apt-repository ppa:avsm/ppa && \
    apt-get update -y && \
    apt-get install -y libsecp256k1-dev geth solc

# Install truffle environment for Solidity
RUN npm install -g truffle

# Add and use non-root user
RUN groupadd -g 1100 appuser && \
    useradd  -m -r -u 1100 -g appuser appuser
USER appuser
WORKDIR /home/appuser

## Install Solidity compiler
#ENV SOLC_VERSION 0.5.2
#RUN wget https://github.com/ethereum/solidity/releases/download/v${SOLC_VERSION}/solidity_${SOLC_VERSION}.tar.gz && tar -xvzf solidity_${SOLC_VERSION}.tar.gz && rm solidity_${SOLC_VERSION}.tar.gz
#RUN cd solidity_${SOLC_VERSION} && ./scripts/install_deps.sh && ./scripts/build.sh

## Install Secp256k1 libraries
#RUN git clone https://github.com/bitcoin-core/secp256k1.git
#RUN cd secp256k1 && ./autogen.sh && ./configure --prefix=/usr --enable-module-recovery && make && make install

## Compiling the OCAML 4.07.1 for the sole purpose of compiling opam 2.0.2
## Create install dir and set permissions
#RUN mkdir -p /home/appuser/opt
#RUN wget http://caml.inria.fr/pub/distrib/ocaml-4.07/ocaml-4.07.1.tar.gz && tar -vxzf ocaml-4.07.1.tar.gz && rm ocaml-4.07.1.tar.gz
#RUN cd ocaml-4.07.1 && ./configure -prefix /home/appuser/opt/ocaml-4.07.1 && make world.opt && make install
##Let's not put it in the regular PATH, but just modify the PATH during OPAM bootstrap
##ENV PATH /home/appuser/opt/ocaml-4.07.1/bin:${PATH}

# Compiling OPAM 2.0.2
RUN wget https://github.com/ocaml/opam/releases/download/2.0.2/opam-full-2.0.2.tar.gz && \
    tar -vxzf opam-full-2.0.2.tar.gz && \
    rm opam-full-2.0.2.tar.gz && \
    export PATH=/home/appuser/opt/ocaml-4.07.1/bin:$PATH && \
    cd opam-full-2.0.2 && \
    prefix=/home/appuser/opt/opam-full-2.0.2 && \
    LIBINSTALL_DIR=$prefix/bootstrap_lib && \
    ./configure --prefix=$prefix && \
    make lib-ext && \
    make && \
    make install LIBINSTALL_DIR=$LIBINSTALL_DIR && \
    rm -rf opam-full-2.0.2
ENV PATH /home/appuser/opt/opam-full-2.0.2/bin:${PATH}

# Install OPAM packages
#ENV OCAML_VERSION ocaml-base-compiler.4.07.1 ### <= use this format to distinguish from system-provided ocaml of same version
ENV OCAML_VERSION 4.07.1

# NB: Pinning the `integers` package is currently necessary due to this bug:
# https://github.com/ocamllabs/ocaml-integers/issues/26
RUN opam init -a --disable-sandboxing && \
    opam switch create ${OCAML_VERSION} && \
    opam pin -y integers 0.2.2 && \
    opam install -y \
    cohttp cohttp-lwt-unix cryptokit cstruct dune integers lens leveldb \
    lwt lwt_ppx ppx_deriving ppx_tools_versioned ppx_core \
    ppx_driver ppx_inline_test secp256k1 yojson qcheck bisect_ppx ; \
    :
ENV PATH /home/appuser/.opam/${OCAML_VERSION}/bin:${PATH}

# Install Scgi library in OPAM
RUN git clone https://github.com/bikallem/ocaml-scgi.git && \
    cd ocaml-scgi && \
    make && \
    opam pin -y add scgi . -n && \
    opam install scgi

# Install ppx_deriving_yojson library in OPAM
RUN git clone https://github.com/ocaml-ppx/ppx_deriving_yojson.git && \
    cd ppx_deriving_yojson && \
    make && \
    opam pin -y add ppx_deriving_yojson . -n && \
    opam install ppx_deriving_yojson
