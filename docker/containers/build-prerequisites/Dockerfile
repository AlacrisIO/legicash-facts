
# Set the base image
FROM ubuntu

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd  -m -r -u 1100 -g appuser appuser

# Dockerfile maintainer (original author: Paul Steckler)
# Modified by Mathieu Dutour Sikiric with help from Vedran
# Modified by Vedran Lebo (Added Solidity install from source )_
MAINTAINER Mathieu Dutour Sikiric <mathieu@legilogic.com>

# Install Ubuntu packages
RUN apt-get update -y
RUN apt-get install -y aspcud dh-autoreconf libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev libsnappy-dev make nginx nodejs npm pkg-config rlwrap screen software-properties-common sudo unzip zlib1g-dev zsh
RUN apt-get install -y git wget bubblewrap cmake

WORKDIR /home/appuser/
# Install Solidity compiler
ENV SOLC_VERSION 0.4.25
RUN wget  https://github.com/ethereum/solidity/releases/download/v${SOLC_VERSION}/solidity_${SOLC_VERSION}.tar.gz && tar -xvzf solidity_${SOLC_VERSION}.tar.gz
RUN cd solidity_${SOLC_VERSION} && ./scripts/install_deps.sh
RUN cd solidity_${SOLC_VERSION} && ./scripts/build.sh
RUN rm solidity_${SOLC_VERSION}.tar.gz

# Install truffle environment for Solidity
RUN npm install -g truffle

# Install Secp256k1 libraries
RUN git clone https://github.com/bitcoin-core/secp256k1.git
RUN cd secp256k1 && ./autogen.sh && ./configure --prefix=/usr --enable-module-recovery && make && make install

# Create install dir and set permissions
RUN mkdir -p /home/appuser/opt
RUN chown -R appuser:appuser /home/appuser/opt
USER appuser
# Compiling the OCAML 4.07.1 for the sole purpose of compiling opam 2.0.1
RUN wget http://caml.inria.fr/pub/distrib/ocaml-4.07/ocaml-4.07.1.tar.gz
RUN tar -vxzf ocaml-4.07.1.tar.gz && cd ocaml-4.07.1 && ./configure -prefix /home/appuser/opt/ocaml-4.07.1 && make world.opt && make install
#RUN export PATH_SAVE=$PATH
#RUN export PATH=/root/opt/ocaml-4.07.1/bin:$PATH

# Compiling the OPAM 2.0.1
RUN wget https://github.com/ocaml/opam/releases/download/2.0.1/opam-full-2.0.1.tar.gz
RUN tar -vxzf opam-full-2.0.1.tar.gz && export PATH=/home/appuser/opt/ocaml-4.07.1/bin:$PATH && cd opam-full-2.0.1 && ./configure --prefix=/home/appuser/opt/opam-full-2.0.1 && make lib-ext && make && make install
ENV PATH PATH /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/home/appuser/opt/opam-full-2.0.1/bin

# Install OPAM packages
ENV OCAML_VERSION 4.06.1
RUN opam init -a --disable-sandboxing
RUN opam switch create ${OCAML_VERSION}
RUN opam install -y cohttp cohttp-lwt-unix cryptokit cstruct dune integers lens leveldb lwt lwt_ppx ppx_deriving ppx_deriving_yojson ppx_tools_versioned ppx_core ppx_driver ppx_inline_test secp256k1 yojson qcheck

# Allow CI script to find OPAM files
ENV PATH /home/appuser/.opam/${OCAML_VERSION}/bin:${PATH}

# Install Scgi library in OPAM
RUN git clone https://github.com/legicash-patches/ocaml-scgi.git
RUN cd ocaml-scgi && git checkout patched && make && opam pin -y add scgi . -n && opam install scgi

