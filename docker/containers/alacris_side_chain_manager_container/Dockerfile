# Create new image
FROM ubuntu:rolling

LABEL maintainer="vedr@nlebo.com"

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -r -u 1100 -g appuser appuser

# Install dependecies
RUN apt-get update -y && \
    apt-get install -y \
    aspcud dh-autoreconf libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev libsnappy-dev git supervisor

# Install Secp256k1 libraries
RUN git clone https://github.com/bitcoin-core/secp256k1.git && \
    cd secp256k1 && ./autogen.sh && ./configure --prefix=/usr --enable-module-recovery && \
    make && make install

# Set permissions for appuser
RUN chown appuser:appuser /var/log/supervisor /var/run/ && \
    chown -R appuser:appuser /var/www/app/legicash-facts/

# Set working directory and user
WORKDIR /var/www/app/legicash-facts/
ENV APPLICATION_HOME=/var/www/app/legicash-facts/
ENV ALACRIS_HOME=/var/www/app/legicash-facts/
USER appuser
