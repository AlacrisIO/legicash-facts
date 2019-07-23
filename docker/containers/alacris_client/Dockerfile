# Base image
FROM gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1 as alacris_client_build

LABEL maintainer="vedr@nlebo.com"

USER root

# Copy app source to container and set permissions for appuser
COPY --chown=appuser:appuser . /var/www/app/legicash-facts

# Build SCGI server
USER appuser

WORKDIR /var/www/app/legicash-facts

RUN make

# Create new App image
FROM gcr.io/legicash-demo-1950/legicash-demo/alacris_client_container:v1

# Copy binary from previous image
COPY --from=alacris_client_build /var/www/app/legicash-facts/_build/default/src/alacris_client/side_chain_client.exe /var/www/app/legicash-facts/_bin/side_chain_client.exe

# Install supervisor config
ADD docker/containers/alacris_client/files/conf/supervisord.conf /etc/supervisord.conf

# Install nginx config
COPY docker/containers/alacris_client/files/conf/nginx/ /etc/nginx/
COPY docker/containers/alacris_client/files/conf/nginx/sites/ /etc/nginx/conf.d/

# Service scripts
ADD docker/containers/alacris_client/files/scripts/run-nginx.sh /usr/local/bin/run-nginx
ADD docker/containers/alacris_client/files/scripts/run-scgi.sh /usr/local/bin/run-scgi

EXPOSE 8081 8800

# command when running image
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
