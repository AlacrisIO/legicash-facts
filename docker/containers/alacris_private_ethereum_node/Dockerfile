# Set the base image (uses Alpine)
FROM ethereum/client-go

LABEL maintainer="vedr@nlebo.com"

# Install dependencies
RUN apk update
RUN apk add bash g++ git make supervisor

# Add appuser
RUN addgroup -g 1100 -S appuser && \
    adduser -u 1100 -S appuser -G appuser

# Install run scripts
ADD docker/containers/alacris_private_ethereum_node/files/scripts/run-ethereum.sh /usr/local/bin/run-ethereum

# Install supervisor config
ADD docker/containers/alacris_private_ethereum_node/files/conf/supervisord.conf /etc/supervisord.conf

# Set permissions for appuser
RUN chown -R appuser:appuser \
    /var/www/app/ \
    /var/run/

USER appuser

# Override geth entrypoint
ENTRYPOINT

WORKDIR /var/www/app/alacris-private-ethereum-node/_run

EXPOSE 8000 8545

# command when running image
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
