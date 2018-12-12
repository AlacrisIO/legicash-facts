# Set the base image (uses Alpine)
FROM ethereum/client-go

RUN apk update
RUN apk add bash g++ git make npm supervisor

# Add appuser
RUN addgroup -g 1100 -S appuser && \
    adduser -u 1100 -S appuser -G appuser


# Create supervisord log and Ethereum log dir2
RUN mkdir -p /var/www/app/alacris-private-ethereum-node/_run/logs/ && \
    mkdir -p /var/www/app/alacris-private-ethereum-node/_ethereum && \
    touch /tmp/supervisord.log

# Install run scripts
ADD files/scripts/run-ethereum.sh /usr/local/bin/run-ethereum
ADD files/scripts/run-block-explorer.sh /usr/local/bin/run-block-explorer

# Install supervisor config
ADD files/conf/supervisord.conf /etc/supervisord.conf

# Install Block Explorer
RUN git clone https://github.com/etherparty/explorer.git /var/www/app/explorer

# Set permissions for appuser
RUN chown -R appuser:appuser /var/www/app/alacris-private-ethereum-node/_run/logs/  \
                          /var/www/app/  \
                          /var/www/app/explorer  \
                          /var/run/  \
                          /tmp/supervisord.log

WORKDIR /var/www/app/explorer

USER appuser

RUN npm install --save
# allow access from other hosts
RUN sed --in-place s/localhost/0.0.0.0/ package.json

# Override geth entrypoint
ENTRYPOINT

WORKDIR /tmp

EXPOSE 8000 8545

# command when running image
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
