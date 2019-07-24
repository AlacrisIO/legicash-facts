FROM gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1

LABEL maintainer="vedr@nlebo.com"

# Copy app source to container and set permissions for appuser
# TODO: understand why this doesn't work when source.tar.gz is in a different directory
# and/or work around that (!)
COPY --chown=appuser:appuser source.tar.gz /var/www/app/legicash-facts/

WORKDIR /var/www/app/legicash-facts
USER appuser
RUN tar zxf source.tar.gz && \
    rm source.tar.gz && \
    make all
