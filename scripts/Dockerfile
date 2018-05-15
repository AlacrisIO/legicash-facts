# to create the Docker image:
#  docker build -t registry.gitlab.com/legicash/legicash-facts:build-env .

# to upload image to Gitlab registry:
#  docker push registry.gitlab.com/legicash/legicash-facts:build-env

# Set the base image
FROM ubuntu

# Dockerfile author / maintainer
MAINTAINER Paul Steckler <steck@legi.cash>

# Install Ubuntu packages
RUN apt-get update -y
RUN apt-get install -y aspcud dh-autoreconf libgmp-dev make opam pkg-config unzip zlib1g-dev

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
