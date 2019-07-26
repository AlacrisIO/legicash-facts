FROM gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1

LABEL maintainer="vedr@nlebo.com"

# Copy app source to container and set permissions for appuser
# It can't be included as _build/source.tar.gz because all of _build is
# in the .dockerignore; thus use .source.tar.gz instead.
COPY --chown=appuser:appuser .source.tar.gz /var/www/app/legicash-facts/

WORKDIR /var/www/app/legicash-facts
USER appuser
RUN tar zxf .source.tar.gz && \
    rm .source.tar.gz && \
    make all
