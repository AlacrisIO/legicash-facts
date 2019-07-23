# Create new image
FROM ubuntu:rolling

LABEL maintainer="vedr@nlebo.com"

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -r -u 1100 -g appuser appuser

# Install dependencies
RUN apt-get update -y && \
    apt-get install -y \
    aspcud dh-autoreconf libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev libsnappy-dev git supervisor software-properties-common

# Install Secp256k1 libraries from the Ethereum repository, for geth and solidity (add-apt-repository depends on software-properties-common)
RUN add-apt-repository ppa:ethereum/ethereum && \
    apt-get update -y && \
    apt-get install -y libsecp256k1-dev geth solc
# RUN git clone https://github.com/bitcoin-core/secp256k1.git && \
#    cd secp256k1 && ./autogen.sh && ./configure --prefix=/usr --enable-module-recovery && \
#    make && make install

# Set permissions for appuser
RUN mkdir -p /var/www/app/legicash-facts/_run ; \
    chown appuser:appuser /var/log/supervisor /var/run /var/www/app

# Set working directory and user
WORKDIR /var/www/app/legicash-facts/
ENV APPLICATION_HOME=/var/www/app/legicash-facts/
ENV ALACRIS_HOME=/var/www/app/legicash-facts/
USER appuser
