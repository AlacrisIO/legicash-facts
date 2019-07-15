# Create new image
FROM ubuntu:rolling

LABEL maintainer="vedr@nlebo.com"

# Add non-root user
RUN groupadd -g 1100 appuser && \
    useradd -r -u 1100 -g appuser appuser

RUN mkdir -p  /var/www/app/legicash-facts/_run/logs && \
    touch /var/www/app/legicash-facts/_run/logs/legicash.log && \
    touch /var/www/app/legicash-facts/supervisord.log && touch /var/www/app/legicash-facts/_run/logs/legicash.log

# Set permissions for appuser
RUN chown -R appuser:appuser /var/www/app/legicash-facts/

# Install dependecies
RUN apt-get update -y
RUN apt-get install -y aspcud dh-autoreconf libgdbm-dev libgmp-dev libleveldb-dev libpcre3-dev libsnappy-dev git supervisor nginx

# Install Secp256k1 libraries
RUN git clone https://github.com/bitcoin-core/secp256k1.git
RUN cd secp256k1 && ./autogen.sh && ./configure --prefix=/usr --enable-module-recovery && make && make install

# Allow running nginx to use directories
RUN chown -R appuser:appuser /var/log/
RUN chown -R appuser:appuser /var/lib/nginx

# Create supervisor log file
#RUN touch /tmp/supervisord.log && \
RUN chown appuser:appuser /var/log/supervisor /var/run/

# Set working directory and user
WORKDIR /var/www/app/legicash-facts/
ENV APPLICATION_HOME=/var/www/app/legicash-facts/
ENV ALACRIS_HOME=/var/www/app/legicash-facts/
USER appuser
