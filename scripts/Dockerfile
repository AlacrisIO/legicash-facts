# to do anything with Docker:
#  docker login
# use your credentials on docker.com
# you may need to run this command and other docker commands with "sudo"

# to create the Docker image:
#  docker build -t registry.gitlab.com/legicash/legicash-facts:build-env .

# see the file DOCKER-TESTNET.md for instructions on setting up the
# Ethereum test net in the Docker image, after completing the build step

# to upload image to Gitlab registry:
#  docker push registry.gitlab.com/legicash/legicash-facts:build-env

# Set the base image
FROM ubuntu

# Dockerfile author / maintainer
MAINTAINER Paul Steckler <steck@legi.cash>

# Install Ubuntu packages
RUN apt-get update -y
RUN apt-get install -y aspcud dh-autoreconf libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev libsnappy-dev make nginx nodejs npm opam pkg-config software-properties-common sudo unzip zlib1g-dev
RUN apt-get install zsh screen rlwrap

# Install Solidity compiler
RUN add-apt-repository ppa:ethereum/ethereum
RUN apt-get update -y
RUN apt-get install -y solc

# Install truffle environment for Solidity
RUN npm install -g truffle

# Install Secp256k1 libraries
RUN git clone https://github.com/bitcoin-core/secp256k1.git
RUN cd secp256k1 && ./autogen.sh && ./configure --prefix=/usr --enable-module-recovery && make && make install

# Install OPAM packages
ENV OCAML_VERSION 4.06.1
RUN opam init -a && opam switch ${OCAML_VERSION}
RUN opam install -y cohttp cohttp-lwt-unix cryptokit cstruct dune integers lens leveldb lwt lwt_ppx ppx_deriving ppx_deriving_yojson ppx_tools_versioned ppx_core ppx_driver ppx_inline_test secp256k1 yojson qcheck

# Allow CI script to find OPAM files
ENV PATH /root/.opam/${OCAML_VERSION}/bin:${PATH}

# Install Scgi library in OPAM
RUN git clone https://github.com/legicash-patches/ocaml-scgi.git
RUN cd ocaml-scgi && git checkout patched && make && opam pin -y add scgi . -n && opam install scgi

# Install geth (Ethereum node)
ENV GETH_VERSION geth-linux-amd64-1.8.12-37685930
RUN cd && curl https://gethstore.blob.core.windows.net/builds/${GETH_VERSION}.tar.gz > ${GETH_VERSION}.tar.gz
RUN cd && tar -xzvf ${GETH_VERSION}.tar.gz
RUN cd && mv ${GETH_VERSION}/geth /usr/local/bin/geth

