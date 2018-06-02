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
RUN apt-get install -y aspcud dh-autoreconf libgmp-dev make nodejs npm opam pkg-config software-properties-common unzip zlib1g-dev

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
RUN opam init -a && opam switch 4.06.1
RUN opam install -y cohttp cohttp-lwt-unix cryptokit integers jbuilder secp256k1 yojson \
    ppx_deriving ppx_tools_versioned ppx_core ppx_driver ppx_inline_test ppx_metaquot

# Install ocaml-lens library
RUN git clone https://github.com/legicash-patches/ocaml-lens.git
RUN PATH=/root/.opam/4.06.1/bin:$PATH eval 'jbuilder build @install --root=ocaml-lens && jbuilder install --root=ocaml-lens'

# Install geth (Ethereum node)
ENV GETH_VERSION geth-linux-amd64-1.8.8-2688dab4
RUN cd && curl https://gethstore.blob.core.windows.net/builds/${GETH_VERSION}.tar.gz > ${GETH_VERSION}.tar.gz
RUN cd && tar -xzvf ${GETH_VERSION}.tar.gz
RUN cd && mv ${GETH_VERSION}/geth /usr/local/bin/geth
