# Create new image
FROM ubuntu:rolling

LABEL maintainer="vedr@nlebo.com"

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -r -u 1100 -g appuser appuser

# Install dependecies
RUN apt-get update -y && \
    apt-get install -y \
    aspcud dh-autoreconf libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev \
    libsnappy-dev git software-properties-common supervisor nginx

# Install Secp256k1 libraries from the Ethereum repository, for geth and solidity (add-apt-repository depends on software-properties-common)
RUN add-apt-repository ppa:ethereum/ethereum && \
    apt-get update -y && \
    apt-get install -y libsecp256k1-dev geth solc

# Allow running nginx to use directories
RUN chown -R appuser:appuser /var/log/ /var/lib/nginx /var/log/supervisor /var/run/

# Set working directory and user
WORKDIR /var/www/app/legicash-facts/
ENV APPLICATION_HOME=/var/www/app/legicash-facts/
ENV ALACRIS_HOME=/var/www/app/legicash-facts/
USER appuser
