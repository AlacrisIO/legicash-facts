FROM gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1

LABEL maintainer="vedr@nlebo.com"

# Copy app source to container and set permissions for appuser
### Trying to be (probably overly) cleverer than:
###    COPY --chown=appuser:appuser . /var/www/app/legicash-facts/
###    RUN git clean -xfd
### The main issue being to skip _build, _run and probably also .git
COPY --chown=appuser:appuser \
     Makefile META alacris.opam dune-project \
     /var/www/app/legicash-facts/
COPY --chown=appuser:appuser bin /var/www/app/legicash-facts/bin
COPY --chown=appuser:appuser config /var/www/app/legicash-facts/config
COPY --chown=appuser:appuser contracts /var/www/app/legicash-facts/contracts
COPY --chown=appuser:appuser scripts /var/www/app/legicash-facts/scripts
COPY --chown=appuser:appuser src /var/www/app/legicash-facts/src

WORKDIR /var/www/app/legicash-facts
USER appuser
RUN make all
