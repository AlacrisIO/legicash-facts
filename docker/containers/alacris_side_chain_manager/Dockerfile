# Base image
# TODO: separate it to its own Dockerfile, with image and build used by both side_chain_manager and client?
FROM gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1 as build

LABEL maintainer="vedr@nlebo.com"

# TODO: prepare a tarball that excludes bad files *before* to build using the Dockerfile,
# e.g. with git archive
# Copy app source to container and set permissions for appuser
### Trying to be (probably overly) clever with:
### COPY --chown=appuser:appuser . /var/www/app/legicash-facts/
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
RUN git clean -xfd ; \
    make

# Create new image
FROM gcr.io/legicash-demo-1950/legicash-demo/alacris_side_chain_manager_container:v1
USER appuser

# copy binary the previous image
COPY --from=build /var/www/app/legicash-facts/_build/default/src/alacris_lib/side_chain_server.exe /var/www/app/legicash-facts/_bin/side_chain_server.exe

# copy binary the previous image
COPY --from=build /var/www/app/legicash-facts/_build/default/src/alacris_lib/setup_contract.exe /var/www/app/legicash-facts/_bin/setup_contract.exe

# copy prefunder binary from the previous image
COPY --from=build /var/www/app/legicash-facts/_build/default/src/legilogic_ethereum/ethereum_prefunder.exe /var/www/app/legicash-facts/_bin/ethereum_prefunder.exe

# Install supervisor config
ADD docker/containers/alacris_side_chain_manager/files/conf/supervisord.conf /etc/supervisord.conf

# Service scripts
ADD docker/containers/alacris_side_chain_manager/files/scripts/run-side-chain-server.sh /usr/local/bin/run-side-chain-server

# copy binary the previous image
COPY docker/containers/alacris_side_chain_manager/files/scripts/prefund.sh /prefund.sh
COPY docker/containers/alacris_side_chain_manager/files/scripts/setup_contract.sh /setup_contract.sh
COPY docker/containers/alacris_side_chain_manager/files/scripts/prefund_setup_create.sh /prefund_setup_create.sh
RUN mkdir -p /var/www/app/legicash-facts/_run/

ENTRYPOINT ["/prefund_setup_create.sh"]


EXPOSE 8095
# command when running image
CMD ["/usr/bin/supervisord", "-n", "-c", "/etc/supervisord.conf"]
